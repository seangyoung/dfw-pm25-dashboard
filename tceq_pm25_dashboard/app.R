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

      parent <- dirname(here)
      if (identical(parent, here)) break
      here <- parent
    }
  }

  stop(
    "Could not locate the dashboard repository root. Open the repository in ",
    "RStudio or set the working directory to dfw-pm25-dashboard before running ",
    "the dashboard."
  )
}

source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
root <- .find_tceq_project_root(source_file, getwd())
app_dir <- file.path(root, "tceq_pm25_dashboard")
source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "tceq_pm25_dashboard.R"))
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
configured_cache_dir <- tceq_dashboard_cache_dir(cfg)
bundled_cache_dir <- file.path(app_dir, "deploy_cache")
cache_is_complete <- function(path) {
  all(file.exists(file.path(
    path, c("station_index.rds", "event_index.rds", "cache_manifest.json")
  )))
}
cache_dir <- if (cache_is_complete(configured_cache_dir)) {
  configured_cache_dir
} else if (cache_is_complete(bundled_cache_dir)) {
  bundled_cache_dir
} else {
  configured_cache_dir
}
if (!cache_is_complete(cache_dir)) {
  stop(
    "The DFW PM2.5 dashboard cache is missing. Run from the project root:\n",
    "Rscript --vanilla scripts/08c_prepare_tceq_dfw_pm25_dashboard.R\n",
    "or restore the versioned tceq_pm25_dashboard/deploy_cache directory."
  )
}

station_index <- readRDS(file.path(cache_dir, "station_index.rds"))
event_index <- readRDS(file.path(cache_dir, "event_index.rds"))
cache_manifest <- jsonlite::read_json(
  file.path(cache_dir, "cache_manifest.json"), simplifyVector = TRUE
)
raw_manifest_path <- file.path(
  tceq_dashboard_raw_dir(root), "download_manifest.csv"
)
cache_stale <- tceq_cache_is_stale(cache_manifest, raw_manifest_path)

max_cached_sites <- suppressWarnings(as.integer(Sys.getenv(
  "TCEQ_DASHBOARD_CACHE_SITES", unset = "3"
)))
if (length(max_cached_sites) != 1L || is.na(max_cached_sites) ||
    max_cached_sites < 1L) {
  max_cached_sites <- 3L
}
load_bundle <- tceq_dashboard_bundle_loader(cache_dir, max_cached_sites)

site_choices <- stats::setNames(
  station_index$aqs_site_id,
  paste0(
    station_index$site_name, " · ", station_index$aqs_site_id,
    ifelse(station_index$active, "", " · historical")
  )
)
default_site <- tceq_dashboard_default_site(station_index)
default_bundle <- load_bundle(default_site)
default_dates <- tceq_dashboard_default_dates(default_bundle)

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
      "TCEQ hourly monitoring data · parameter 88101 with 88502 fallback · local standard time"
    )
  ),
  theme = theme,
  fillable = TRUE,
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('tceq-set-disabled', function(message) {\n",
      "  var element = document.getElementById(message.id);\n",
      "  if (element) element.disabled = Boolean(message.disabled);\n",
      "});"
    ))
  ),
  sidebar = bslib::sidebar(
    width = 370,
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
        ),
        selected = "30", width = "100%"
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
      "date_range", "Exact dates",
      start = default_dates[1], end = default_dates[2],
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
          "Raw report-table identifiers are valid only within their source month and are not treated as stable instrument identities. Event labels remain composite-based."
        ),
        plotly::plotlyOutput("event_plot", height = "560px")
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
        bslib::card_header("Computed versus TCEQ-reported daily average"),
        plotly::plotlyOutput("daily_difference_plot", height = "430px")
      ),
      shiny::p(
        class = "method-note",
        paste(
          "Parameter 88101 is preferred for every site-hour; acceptable parameter 88502 is used only when 88101 is unavailable.",
          "Negative and character-coded cells remain in the raw cache with explicit flags and are excluded from numeric calculations.",
          "TCEQ notes that current monitoring data are unofficial until certified."
        )
      )
    )
  )
)

server <- tceq_dashboard_server(
  station_index = station_index,
  event_index = event_index,
  load_bundle = load_bundle,
  cache_manifest = cache_manifest,
  cache_stale = cache_stale
)

shiny::shinyApp(ui, server)
