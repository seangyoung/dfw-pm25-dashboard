.find_tceq_project_root <- function(source_file = NULL, working_dir = getwd()) {
  source_file <- as.character(source_file)
  source_file <- source_file[
    !is.na(source_file) & nzchar(source_file)
  ]
  source_dirs <- vapply(source_file, function(path) {
    dirname(normalizePath(path, winslash = "/", mustWork = FALSE))
  }, character(1))
  starts <- unique(c(
    source_dirs,
    normalizePath(working_dir, winslash = "/", mustWork = TRUE)
  ))

  for (start in starts) {
    here <- start
    repeat {
      if (file.exists(file.path(here, "R", "utils.R")) &&
          file.exists(file.path(here, "config", "config.yml"))) {
        return(here)
      }

      # Also support opening the historical parent folder as the RStudio project.
      nested <- file.path(here, "TX_FireHealth")
      if (file.exists(file.path(nested, "R", "utils.R")) &&
          file.exists(file.path(nested, "config", "config.yml"))) {
        return(nested)
      }

      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }

  stop(
    "Could not locate the TX_FireHealth project root. Open the project in ",
    "RStudio or set the working directory to TX_FireHealth before running ",
    "the dashboard."
  )
}

source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
root <- .find_tceq_project_root(source_file, getwd())
app_dir <- file.path(root, "tceq_pm25_dashboard")
source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "tceq_pm25_dashboard.R"))
source(file.path(root, "R", "dallas_aqmesh.R"))
source(file.path(root, "R", "pm25_regional.R"))
source(file.path(root, "R", "tceq_pm25_dashboard_app.R"))
require_packages(c(
  "bslib", "dplyr", "DT", "ggplot2", "htmltools", "jsonlite", "leaflet",
  "plotly", "purrr", "readr", "scales", "shiny", "tibble", "tidyr",
  "viridisLite", "yaml"
))
sass_cache_path <- file.path(tempdir(), "tx_firehealth_sass_cache")
dir.create(sass_cache_path, recursive = TRUE, showWarnings = FALSE)
options(sass.cache = sass_cache_path)

cfg <- read_config()
combined_cache_dir <- pm25_dashboard_cache_dir(cfg)
tceq_cache_dir <- tceq_dashboard_cache_dir(cfg)
bundled_cache_dir <- file.path(app_dir, "deploy_cache")
cache_is_complete <- function(path) {
  all(file.exists(file.path(
    path, c("station_index.rds", "event_index.rds", "cache_manifest.json")
  )))
}
cache_dir <- if (cache_is_complete(combined_cache_dir)) {
  combined_cache_dir
} else if (cache_is_complete(tceq_cache_dir)) {
  tceq_cache_dir
} else if (cache_is_complete(bundled_cache_dir)) {
  bundled_cache_dir
} else {
  combined_cache_dir
}
if (!cache_is_complete(cache_dir)) {
  stop(
    "The DFW PM2.5 dashboard cache is missing. Run from the project root:\n",
    "Rscript --vanilla scripts/08i_update_dfw_pm25_dashboard.R\n",
    "or restore the versioned tceq_pm25_dashboard/deploy_cache directory."
  )
}

station_index <- readRDS(file.path(cache_dir, "station_index.rds"))
if (!"data_source" %in% names(station_index)) {
  station_index$data_source <- "TCEQ"
  station_index$network_label <- "Texas Commission on Environmental Quality"
  station_index$source_station_id <- station_index$aqs_site_id
  station_index$time_label <- "LST"
}
event_index <- readRDS(file.path(cache_dir, "event_index.rds"))
cache_manifest <- jsonlite::read_json(
  file.path(cache_dir, "cache_manifest.json"), simplifyVector = TRUE
)
raw_manifest_path <- file.path(
  tceq_dashboard_raw_dir(root), "download_manifest.csv"
)
cache_stale <- if (!is.null(cache_manifest$data_sources) &&
                   "Dallas AQMesh" %in% cache_manifest$data_sources) {
  pm25_combined_cache_is_stale(cache_manifest, root, cfg)
} else {
  tceq_cache_is_stale(cache_manifest, raw_manifest_path)
}

max_cached_sites <- suppressWarnings(as.integer(Sys.getenv(
  "TCEQ_DASHBOARD_CACHE_SITES", unset = "3"
)))
if (length(max_cached_sites) != 1L || is.na(max_cached_sites) ||
    max_cached_sites < 1L) {
  max_cached_sites <- 3L
}
load_bundle <- tceq_dashboard_bundle_loader(cache_dir, max_cached_sites)

regional_cache_files <- c(
  daily = "regional_daily.rds", reach = "regional_reach.rds",
  links = "regional_event_links.rds", hourly_index = "regional_hourly_index.rds",
  spatial = "spatial_support.rds", validation = "spatial_validation.rds"
)
regional_hourly_files <- if (!is.null(cache_manifest$regional_analysis$hourly_files)) {
  unlist(cache_manifest$regional_analysis$hourly_files, use.names = FALSE)
} else character()
regional_available <- all(file.exists(file.path(
  cache_dir, c(regional_cache_files, regional_hourly_files)
)))
regional_objects <- new.env(parent = emptyenv())
load_regional <- function(name) {
  if (!regional_available) return(NULL)
  path <- if (name %in% names(regional_cache_files)) {
    regional_cache_files[[name]]
  } else if (grepl("^hourly_[0-9]{4}$", name)) {
    file.path("regional_hourly", paste0("year_", sub("^hourly_", "", name), ".rds"))
  } else return(NULL)
  if (!file.exists(file.path(cache_dir, path))) return(NULL)
  if (!exists(name, envir = regional_objects, inherits = FALSE)) {
    assign(
      name, readRDS(file.path(cache_dir, path)),
      envir = regional_objects
    )
  }
  get(name, envir = regional_objects, inherits = FALSE)
}

site_choices <- stats::setNames(
  station_index$aqs_site_id,
  paste0(
    station_index$site_name, " · ", station_index$data_source,
    ifelse(station_index$active, "", " · historical")
  )
)
default_site <- tceq_dashboard_default_site(station_index)
default_bundle <- load_bundle(default_site)
default_dates <- tceq_dashboard_default_dates(default_bundle)
anchor_events <- event_index[event_index$event_type == "multi_day", , drop = FALSE]
anchor_events$event_key <- pm25_event_key(
  anchor_events$aqs_site_id, anchor_events$event_id
)
anchor_names <- station_index$site_name[
  match(anchor_events$aqs_site_id, station_index$aqs_site_id)
]
anchor_choices <- stats::setNames(
  anchor_events$event_key,
  paste0(
    anchor_names, " · ", anchor_events$data_source, " · ",
    anchor_events$start_date, " to ", anchor_events$end_date
  )
)
default_anchor <- if (length(anchor_choices)) unname(anchor_choices[[1]]) else ""

theme <- bslib::bs_theme(
  version = 5, bootswatch = "flatly", primary = "#16697A",
  secondary = "#6B7280", success = "#287D5B", warning = "#D97706"
)

metric_box <- function(title, output_id, showcase = NULL) {
  bslib::value_box(
    title = title,
    value = shiny::textOutput(output_id, inline = TRUE),
    showcase = showcase,
    theme = "light"
  )
}

ui <- bslib::page_sidebar(
  title = shiny::div(
    class = "app-title",
    shiny::span("DFW PM2.5 Monitor Explorer"),
    shiny::tags$small(
      "TCEQ regulatory monitors and City of Dallas AQMesh community sensors"
    )
  ),
  theme = theme,
  fillable = TRUE,
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet", type = "text/css", href = "styles.css?v=regional-ui-3"
    ),
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('tceq-set-disabled', function(message) {\n",
      "  var element = document.getElementById(message.id);\n",
      "  if (element) element.disabled = Boolean(message.disabled);\n",
      "});\n",
      "(function() {\n",
      "  var overlays = {}, legends = {};\n",
      "  var palette = ['#440154','#414487','#2A788E','#22A884','#7AD151','#FDE725'];\n",
      "  function hexToRgb(hex) { return [parseInt(hex.slice(1,3),16), parseInt(hex.slice(3,5),16), parseInt(hex.slice(5,7),16)]; }\n",
      "  function color(value, upper) {\n",
      "    var t = Math.max(0, Math.min(1, value / upper));\n",
      "    var p = t * (palette.length - 1), i = Math.min(palette.length - 2, Math.floor(p)), f = p - i;\n",
      "    var a = hexToRgb(palette[i]), b = hexToRgb(palette[i + 1]);\n",
      "    return [Math.round(a[0]+f*(b[0]-a[0])), Math.round(a[1]+f*(b[1]-a[1])), Math.round(a[2]+f*(b[2]-a[2])), 190];\n",
      "  }\n",
      "  function surfaceColor(value, message) {\n",
      "    if (message.categorical === 'contribution') {\n",
      "      if (value === 1) return [15,118,110,205];\n",
      "      if (value === 2) return [124,58,237,205];\n",
      "      if (value === 3) return [217,119,6,215];\n",
      "      return [0,0,0,0];\n",
      "    }\n",
      "    return color(value, message.upper);\n",
      "  }\n",
      "  function legendRow(container, colorValue, label, markerClass) {\n",
      "    var row = L.DomUtil.create('div', 'pm25-legend-row', container);\n",
      "    var swatch = L.DomUtil.create('span', markerClass || 'pm25-legend-swatch', row);\n",
      "    if (colorValue) swatch.style.background = colorValue;\n",
      "    var text = L.DomUtil.create('span', '', row); text.textContent = label;\n",
      "  }\n",
      "  function updateLegend(map, message) {\n",
      "    if (legends[message.id]) map.removeControl(legends[message.id]);\n",
      "    var control = L.control({position: 'bottomright'});\n",
      "    control.onAdd = function() {\n",
      "      var div = L.DomUtil.create('div', 'pm25-surface-legend');\n",
      "      var title = L.DomUtil.create('div', 'pm25-legend-title', div);\n",
      "      title.textContent = message.legend.title;\n",
      "      if (message.legend.type === 'categorical') {\n",
      "        legendRow(div, '#0F766E', 'TCEQ only');\n",
      "        legendRow(div, '#7C3AED', 'AQMesh only');\n",
      "        legendRow(div, '#D97706', 'Both networks');\n",
      "      } else {\n",
      "        L.DomUtil.create('div', 'pm25-legend-gradient', div);\n",
      "        var labels = L.DomUtil.create('div', 'pm25-legend-scale', div);\n",
      "        [message.legend.low, message.legend.mid, message.legend.high].forEach(function(label) {\n",
      "          var span = L.DomUtil.create('span', '', labels); span.textContent = label;\n",
      "        });\n",
      "      }\n",
      "      var blank = L.DomUtil.create('div', 'pm25-legend-note', div);\n",
      "      blank.textContent = 'Blank area = unsupported';\n",
      "      var sensorTitle = L.DomUtil.create('div', 'pm25-legend-subtitle', div);\n",
      "      sensorTitle.textContent = 'Sensor values';\n",
      "      if (message.legend.sensors.indexOf('TCEQ') >= 0)\n",
      "        legendRow(div, '#0F766E', 'TCEQ', 'pm25-legend-marker pm25-legend-marker-tceq');\n",
      "      if (message.legend.sensors.indexOf('Dallas AQMesh') >= 0)\n",
      "        legendRow(div, '#7C3AED', 'Dallas AQMesh', 'pm25-legend-marker pm25-legend-marker-aqmesh');\n",
      "      L.DomEvent.disableClickPropagation(div); L.DomEvent.disableScrollPropagation(div);\n",
      "      return div;\n",
      "    };\n",
      "    control.addTo(map); legends[message.id] = control;\n",
      "  }\n",
      "  function drawSurface(message, attempt) {\n",
      "    var widget = HTMLWidgets.find('#' + message.id);\n",
      "    if (!widget || !widget.getMap) {\n",
      "      if (attempt < 20) setTimeout(function() { drawSurface(message, attempt + 1); }, 100);\n",
      "      return;\n",
      "    }\n",
      "    var map = widget.getMap();\n",
      "    if (overlays[message.id]) map.removeLayer(overlays[message.id]);\n",
      "    var canvas = document.createElement('canvas');\n",
      "    canvas.width = message.cols; canvas.height = message.rows;\n",
      "    var context = canvas.getContext('2d'), image = context.createImageData(message.cols, message.rows);\n",
      "    for (var i = 0; i < message.values.length; i++) {\n",
      "      var value = message.values[i], rgba = Number.isFinite(value) ? surfaceColor(value, message) : [0,0,0,0];\n",
      "      image.data[i*4] = rgba[0]; image.data[i*4+1] = rgba[1]; image.data[i*4+2] = rgba[2]; image.data[i*4+3] = rgba[3];\n",
      "    }\n",
      "    context.putImageData(image, 0, 0);\n",
      "    var bounds = [[message.bounds.south, message.bounds.west],[message.bounds.north, message.bounds.east]];\n",
      "    overlays[message.id] = L.imageOverlay(canvas.toDataURL('image/png'), bounds, {opacity: 0.72, interactive: false}).addTo(map);\n",
      "    updateLegend(map, message);\n",
      "    if (message.fit) map.fitBounds(bounds, {padding:[8,8]});\n",
      "  }\n",
      "  Shiny.addCustomMessageHandler('pm25-surface-frame', function(message) { drawSurface(message, 0); });\n",
      "  Shiny.addCustomMessageHandler('pm25-sidebar-top', function(message) {\n",
      "    setTimeout(function() {\n",
      "      var sidebar = document.querySelector('.bslib-sidebar-layout > .sidebar');\n",
      "      if (sidebar) sidebar.scrollTop = 0;\n",
      "    }, 60);\n",
      "  });\n",
      "  Shiny.addCustomMessageHandler('pm25-start-animation', function(message) {\n",
      "    function start(attempt) {\n",
      "      var button = document.querySelector('.slider-animate-button[data-target-id=\"' + message.id + '\"]');\n",
      "      if (!button) { if (attempt < 20) setTimeout(function() { start(attempt + 1); }, 100); return; }\n",
      "      if (!button.classList.contains('playing')) button.click();\n",
      "    }\n",
      "    setTimeout(function() { start(0); }, 80);\n",
      "  });\n",
      "  Shiny.addCustomMessageHandler('pm25-map-resize', function(ids) {\n",
      "    setTimeout(function() { ids.forEach(function(id) { var w=HTMLWidgets.find('#'+id); if(w&&w.getMap) w.getMap().invalidateSize(); }); }, 120);\n",
      "  });\n",
      "})();"
    ))
  ),
  sidebar = bslib::sidebar(
    width = 370,
    shiny::conditionalPanel(
      condition = "input.dashboard_tabs != 'regional'",
      shiny::selectInput(
        "data_source", "Monitoring network",
        choices = c(
          "Combined" = "Combined", "TCEQ" = "TCEQ",
          "Dallas AQMesh" = "Dallas AQMesh"
        ), selected = "Combined"
      ),
      shiny::selectInput(
        "site", "Monitoring site", choices = site_choices, selected = default_site
      ),
      shiny::uiOutput("site_details"),
      leaflet::leafletOutput("site_map", height = 310),
      shiny::tags$label(
        class = "date-window-label", `for` = "date_window_days", "Date window"
      ),
      shiny::div(
        class = "date-window-controls",
        shiny::actionButton(
          "date_previous", NULL, icon = shiny::icon("chevron-left"),
          title = "Move to the preceding window", `aria-label` = "Earlier period"
        ),
        shiny::selectInput(
          "date_window_days", NULL,
          choices = c(
            "7 days" = "7", "14 days" = "14", "30 days" = "30",
            "60 days" = "60", "90 days" = "90"
          ), selected = "30", width = "100%"
        ),
        shiny::actionButton(
          "date_next", NULL, icon = shiny::icon("chevron-right"),
          title = "Move to the following window", `aria-label` = "Later period"
        ),
        shiny::actionButton(
          "date_latest", "Latest", title = "Return to the latest available data"
        )
      ),
      shiny::dateRangeInput(
        "date_range", "Exact dates", start = default_dates[1], end = default_dates[2],
        min = tceq_dashboard_date_bounds(default_bundle)[1],
        max = tceq_dashboard_date_bounds(default_bundle)[2],
        format = "yyyy-mm-dd", separator = " to "
      ),
      shiny::uiOutput("date_window_summary"),
      shiny::helpText("The temporal heatmap is limited to 90 days."),
      shiny::uiOutput("cache_status"),
      shiny::div(
        class = "download-row",
        shiny::downloadButton("download_hourly", "Download visible hours"),
        shiny::downloadButton("download_events", "Download site events")
      )
    ),
    shiny::conditionalPanel(
      condition = "input.dashboard_tabs == 'regional'",
      shiny::conditionalPanel(
        condition = "input.regional_tabs == 'Event alignment'",
        shiny::selectInput(
          "regional_event_network", "Anchor network",
          choices = c(
            "Both networks" = "All", "TCEQ" = "TCEQ",
            "Dallas AQMesh" = "Dallas AQMesh"
          ), selected = "All"
        ),
        shiny::div(
          class = "regional-event-filter-grid",
          shiny::selectInput(
            "regional_event_min_days", "Minimum span",
            choices = c(
              "2+ days" = "2", "3+ days" = "3", "5+ days" = "5",
              "7+ days" = "7", "14+ days" = "14"
            ), selected = "2"
          ),
          shiny::selectInput(
            "regional_event_min_sites", "Minimum aligned sites",
            choices = c(
              "Any" = "1", "2+ sites" = "2", "3+ sites" = "3",
              "5+ sites" = "5", "10+ sites" = "10"
            ), selected = "1"
          )
        ),
        shiny::selectizeInput(
          "regional_anchor", "Anchor multi-day event", choices = anchor_choices,
          selected = default_anchor, options = list(maxOptions = 5000)
        ),
        shiny::uiOutput("regional_event_filter_status")
      ),
      shiny::dateRangeInput(
        "regional_dates", "Regional date window",
        start = default_dates[1], end = default_dates[2],
        min = min(station_index$first_date), max = max(station_index$last_date),
        format = "yyyy-mm-dd", separator = " to "
      ),
      shiny::conditionalPanel(
        condition = "input.regional_tabs == 'Spatial surface'",
        shiny::div(
          class = "regional-spatial-controls",
          shiny::selectInput(
            "regional_map_view", "Surface network",
            choices = c("Combined", "TCEQ", "Dallas AQMesh", "Disagreement"),
            selected = "Combined"
          ),
          shiny::selectInput(
            "regional_metric", "Map time scale",
            choices = c(
              "Daily average" = "daily_avg", "Daily maximum" = "daily_max",
              "Hourly" = "hourly"
            ), selected = "daily_avg"
          ),
          shiny::uiOutput("regional_frame_control"),
          shiny::selectInput(
            "regional_layer", "Map layer",
            choices = c(
              "PM2.5 concentration" = "concentration",
              "Nearby sensor count" = "support_count",
              "Nearest sensor distance" = "nearest_distance",
              "Contributing network" = "network_contribution"
            ), selected = "concentration"
          ),
          shiny::checkboxInput(
            "regional_adaptive", "Adaptive scale for static frame", FALSE
          ),
          shiny::checkboxInput(
            "regional_compare", "Compare TCEQ and AQMesh maps", FALSE
          ),
          shiny::selectInput(
            "regional_speed", "Animation speed",
            choices = c("Slow" = "1800", "Normal" = "1000", "Fast" = "500"),
            selected = "1000"
          ),
          shiny::actionButton(
            "regional_generate", "Generate & play animation",
            icon = shiny::icon("play")
          ),
          shiny::uiOutput("regional_animation_status")
        )
      ),
      shiny::uiOutput("regional_cache_status"),
      shiny::conditionalPanel(
        condition = "input.regional_tabs == 'Event alignment'",
        shiny::div(
          class = "download-row",
          shiny::downloadButton("download_regional_reach", "Download reach"),
          shiny::downloadButton("download_regional_overlap", "Download overlaps")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.regional_tabs == 'Spatial surface'",
        shiny::div(
          class = "download-row",
          shiny::downloadButton("download_regional_grid", "Download current grid")
        )
      )
    )
  ),
  bslib::navset_card_tab(
    id = "dashboard_tabs",
    bslib::nav_panel(
      "Overview",
      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        metric_box("Valid coverage", "kpi_coverage"),
        metric_box("Latest valid date", "kpi_latest"),
        metric_box("Maximum", "kpi_max"),
        metric_box("95th percentile", "kpi_p95"),
        metric_box("Short spikes", "kpi_spikes"),
        metric_box("Multi-day events", "kpi_multiday")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Daily PM2.5 in the selected period"),
        plotly::plotlyOutput("overview_daily_plot", height = "480px")
      ),
      shiny::p(
        class = "method-note",
        "Daily statistics are recalculated from the site composite. Event-eligible days require at least 18 valid hours."
      )
    ),
    bslib::nav_panel(
      "Hourly heatmap",
      shiny::div(
        class = "heatmap-controls",
        shiny::checkboxInput(
          "adaptive_heatmap", "Use an adaptive color scale", value = FALSE
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Hourly PM2.5 by date and hour"),
        plotly::plotlyOutput("heatmap_plot", height = "700px")
      ),
      shiny::p(class = "method-note", shiny::textOutput("heatmap_caption", inline = TRUE))
    ),
    bslib::nav_panel(
      "Events",
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            shiny::div(
              class = "card-header-controls",
              shiny::span("Identified events"),
              shiny::selectInput(
                "event_type", NULL,
                choices = c(
                  "All event types" = "all", "Short-term spikes" = "short_spike",
                  "Multi-day particulate events" = "multi_day"
                ), selected = "all", width = "250px"
              )
            )
          ),
          DT::DTOutput("event_table")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Events by year"),
          plotly::plotlyOutput("event_annual_plot", height = "360px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Selected event: hourly detail"),
        shiny::uiOutput("event_details"),
        shiny::selectizeInput(
          "raw_series", "Optional raw report-table overlays",
          choices = character(), selected = character(), multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Composite only")
        ),
        shiny::p(
          class = "method-note",
          paste(
            "Raw series are diagnostic overlays only; TCEQ report-table identifiers are month-local,",
            "while Dallas AQMesh pod identifiers and revision counts are retained explicitly.",
            "Event labels remain composite-based."
          )
        ),
        plotly::plotlyOutput("event_plot", height = "560px")
      )
    ),
    bslib::nav_panel(
      "Regional analysis", value = "regional",
      bslib::navset_card_tab(
        id = "regional_tabs",
        bslib::nav_panel(
          "Event alignment",
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            metric_box("Peak balanced reach", "regional_kpi_reach"),
            metric_box("Directly overlapping sites", "regional_kpi_sites"),
            metric_box("Peak affected date", "regional_kpi_date"),
            metric_box("Coverage at peak", "regional_kpi_coverage")
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Multi-day event alignment across sensors"),
            plotly::plotlyOutput("regional_alignment_plot", height = "620px")
          ),
          bslib::layout_columns(
            col_widths = c(7, 5),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Daily observed reach"),
              shiny::uiOutput("regional_coverage_badge"),
              plotly::plotlyOutput("regional_reach_plot", height = "420px")
            ),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("Direct and span-only overlaps"),
              DT::DTOutput("regional_overlap_table")
            )
          ),
          shiny::p(
            class = "method-note",
            "Balanced reach averages within-network percentages when at least three sites in a network are eligible. It is descriptive and is not a regulatory determination."
          )
        ),
        bslib::nav_panel(
          "Spatial surface",
          shiny::uiOutput("regional_surface_summary"),
          shiny::conditionalPanel(
            condition = "!input.regional_compare",
            bslib::card(
              full_screen = TRUE,
              fill = FALSE,
              class = "regional-map-card",
              bslib::card_header("Exploratory regional PM2.5 surface"),
              leaflet::leafletOutput("regional_surface_map", height = "690px")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.regional_compare",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE, fill = FALSE,
                class = "regional-map-card",
                bslib::card_header("TCEQ surface"),
                leaflet::leafletOutput("regional_tceq_map", height = "620px")
              ),
              bslib::card(
                full_screen = TRUE, fill = FALSE,
                class = "regional-map-card",
                bslib::card_header("Dallas AQMesh surface"),
                leaflet::leafletOutput("regional_aqmesh_map", height = "620px")
              )
            )
          ),
          shiny::p(
            class = "method-note",
            paste(
              "Surfaces use inverse-distance weighting only where sensor support is adequate.",
              "Blank areas are intentionally not extrapolated. AQMesh and Combined views are exploratory and non-regulatory."
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      "Patterns",
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Typical PM2.5 by hour"),
          plotly::plotlyOutput("diurnal_plot", height = "390px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Typical PM2.5 by month"),
          plotly::plotlyOutput("seasonal_plot", height = "390px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Full site history"),
        plotly::plotlyOutput("long_term_plot", height = "500px")
      )
    ),
    bslib::nav_panel(
      "Data quality",
      shiny::uiOutput("quality_summary"),
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Annual coverage and composite source"),
          plotly::plotlyOutput("quality_plot", height = "430px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Raw hourly quality flags"),
          DT::DTOutput("flag_table")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Computed versus source-reported daily average"),
        plotly::plotlyOutput("daily_difference_plot", height = "430px")
      ),
      shiny::uiOutput("source_method_note")
    )
  )
)

server <- tceq_dashboard_server(
  station_index = station_index,
  event_index = event_index,
  load_bundle = load_bundle,
  cache_manifest = cache_manifest,
  cache_stale = cache_stale,
  load_regional = load_regional,
  regional_available = regional_available
)

shiny::shinyApp(ui, server)
