tceq_dashboard_plot_config <- function(plot) {
  plotly::config(
    plot, displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
  )
}

tceq_dashboard_default_site <- function(station_index) {
  candidates <- station_index[station_index$active %in% TRUE, , drop = FALSE]
  if (!nrow(candidates)) candidates <- station_index
  candidates <- candidates[order(-as.numeric(candidates$last_date), candidates$site_name), ]
  as.character(candidates$aqs_site_id[1])
}

tceq_dashboard_site_choices <- function(station_index) {
  stats::setNames(
    station_index$aqs_site_id,
    paste0(
      station_index$site_name, " · ", station_index$data_source,
      ifelse(station_index$active, "", " · historical")
    )
  )
}

tceq_dashboard_default_dates <- function(bundle, days = 30L) {
  bounds <- tceq_dashboard_date_bounds(bundle)
  tceq_dashboard_window_dates(bounds[1], bounds[2], days, bounds[2])
}

tceq_dashboard_date_bounds <- function(bundle) {
  dates <- as.Date(bundle$hourly$date)
  if (!length(dates) || all(is.na(dates))) {
    stop("The site bundle does not contain usable hourly dates.")
  }
  finite <- dates[is.finite(bundle$hourly$composite_pm25_ug_m3)]
  last_date <- if (length(finite)) max(finite, na.rm = TRUE) else max(dates, na.rm = TRUE)
  as.Date(c(min(dates, na.rm = TRUE), last_date))
}

tceq_dashboard_window_dates <- function(
    first_date, last_date, days = 30L, end_date = last_date) {
  first_date <- as.Date(first_date)[1]
  last_date <- as.Date(last_date)[1]
  end_date <- as.Date(end_date)[1]
  days <- suppressWarnings(as.integer(days)[1])
  if (anyNA(c(first_date, last_date, end_date)) || first_date > last_date) {
    stop("Date-window bounds must be valid dates in chronological order.")
  }
  if (is.na(days) || days < 1L || days > 90L) {
    stop("Date-window length must be between 1 and 90 days.")
  }

  coverage_days <- as.integer(last_date - first_date) + 1L
  span_days <- min(days, coverage_days)
  end_date <- min(max(end_date, first_date), last_date)
  start_date <- end_date - (span_days - 1L)
  if (start_date < first_date) {
    start_date <- first_date
    end_date <- first_date + (span_days - 1L)
  }
  as.Date(c(start_date, end_date))
}

tceq_dashboard_shift_dates <- function(dates, first_date, last_date, direction) {
  dates <- as.Date(dates)
  if (length(dates) != 2L || anyNA(dates) || dates[1] > dates[2]) {
    stop("The current date window must contain a valid start and end date.")
  }
  direction <- suppressWarnings(as.integer(direction)[1])
  if (is.na(direction) || !direction %in% c(-1L, 1L)) {
    stop("Date-window direction must be -1 or 1.")
  }
  span_days <- as.integer(dates[2] - dates[1]) + 1L
  tceq_dashboard_window_dates(
    first_date, last_date, span_days,
    dates[2] + direction * span_days
  )
}

tceq_dashboard_event_table <- function(events) {
  if (!nrow(events)) {
    return(tibble::tibble(
      Event = character(), Type = character(), Start = character(), End = character(),
      `Hours / days` = character(), `Peak hourly` = double(),
      `Peak daily max` = double(), `Highest daily avg` = double(),
      `QA note` = character()
    ))
  }
  if (!"data_source" %in% names(events)) events$data_source <- NA_character_
  events |>
    dplyr::transmute(
      Event = .data$event_id,
      Type = dplyr::recode(
        .data$event_type,
        short_spike = "Short-term spike", multi_day = "Multi-day particulate event"
      ),
      Start = ifelse(
        .data$event_type == "short_spike",
        format(.data$start_lstd, "%Y-%m-%d %H:00 local", tz = "UTC"),
        as.character(.data$start_date)
      ),
      End = ifelse(
        .data$event_type == "short_spike",
        format(.data$end_lstd, "%Y-%m-%d %H:00 local", tz = "UTC"),
        as.character(.data$end_date)
      ),
      `Hours / days` = ifelse(
        .data$event_type == "short_spike",
        paste0(.data$duration_hours, " h"), paste0(.data$span_days, " d")
      ),
      `Peak hourly` = round(.data$peak_hourly_ug_m3, 1),
      `Peak daily max` = round(.data$peak_daily_max_ug_m3, 1),
      `Highest daily avg` = round(.data$max_daily_avg_ug_m3, 1),
      `QA note` = dplyr::case_when(
        .data$data_source == "Dallas AQMesh" ~
          "Non-regulatory Dallas AQMesh screening data",
        .data$parameter_fallback & .data$qa_fallback ~ "Includes 88502 parameter fallback and QA-fallback values",
        .data$parameter_fallback ~ "Includes 88502 parameter-fallback values",
        .data$qa_fallback ~ "Includes QA-fallback values",
        .data$coverage_warning ~ "Some event days have fewer than 24 valid hours",
        TRUE ~ ""
      )
    )
}

tceq_dashboard_raw_choices <- function(raw) {
  if (!nrow(raw)) return(character())
  if ("data_source" %in% names(raw) &&
      any(raw$data_source == "Dallas AQMesh", na.rm = TRUE)) {
    choices <- raw |>
      dplyr::group_by(.data$raw_series_key, .data$pod_serial_number) |>
      dplyr::summarise(
        particleprotocol_version = tceq_collapse_values(
          .data$particleprotocol_version
        ),
        reading_status_normalized = tceq_collapse_values(
          .data$reading_status_normalized
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$pod_serial_number) |>
      dplyr::mutate(
        label = sprintf(
          "AQMesh pod %s · particle protocol %s · status %s",
          .data$pod_serial_number,
          dplyr::coalesce(as.character(.data$particleprotocol_version), "not reported"),
          dplyr::coalesce(.data$reading_status_normalized, "not reported")
        )
      )
    return(stats::setNames(choices$raw_series_key, choices$label))
  }
  choices <- raw |>
    dplyr::distinct(
      .data$raw_series_key, .data$source_file, .data$report_block, .data$poc,
      .data$parameter_code, .data$regulatory_qa
    ) |>
    dplyr::arrange(.data$source_file, .data$poc, .data$report_block) |>
    dplyr::mutate(
      qa_label = dplyr::case_when(
        .data$regulatory_qa %in% TRUE ~ "regulatory QA",
        .data$regulatory_qa %in% FALSE ~ "non-regulatory QA",
        TRUE ~ "QA status unavailable"
      ),
      label = sprintf(
        "%s · parameter %s · POC %s · report table %s · %s",
        sub("^cams_[0-9]+_|_pm25\\.csv$", "", .data$source_file),
        .data$parameter_code, .data$poc, .data$report_block, .data$qa_label
      )
    )
  stats::setNames(choices$raw_series_key, choices$label)
}

tceq_dashboard_yearly_quality <- function(bundle) {
  hourly <- bundle$hourly
  if (!nrow(hourly)) return(tibble::tibble())
  first_date <- min(hourly$date)
  last_date <- max(hourly$date)
  years <- seq.int(
    as.integer(format(first_date, "%Y")), as.integer(format(last_date, "%Y"))
  )
  purrr::map_dfr(years, function(year) {
    year_start <- max(first_date, as.Date(sprintf("%d-01-01", year)))
    year_end <- min(last_date, as.Date(sprintf("%d-12-31", year)))
    expected <- (as.integer(year_end - year_start) + 1L) * 24L
    x <- hourly[hourly$date >= year_start & hourly$date <= year_end, , drop = FALSE]
    is_aqmesh <- !is.null(bundle$metadata) &&
      "data_source" %in% names(bundle$metadata) &&
      identical(as.character(bundle$metadata$data_source[1]), "Dallas AQMesh")
    primary <- if (is_aqmesh) {
      sum(is.finite(x$composite_pm25_ug_m3), na.rm = TRUE)
    } else {
      sum(
        is.finite(x$composite_pm25_ug_m3) &
          x$selected_parameter_code == "88101", na.rm = TRUE
      )
    }
    acceptable <- if (is_aqmesh) 0L else sum(
      is.finite(x$composite_pm25_ug_m3) &
        x$selected_parameter_code == "88502", na.rm = TRUE
    )
    tibble::tibble(
      year = year, expected_hours = expected, preferred_hours = primary,
      fallback_hours = acceptable,
      missing_hours = max(0L, expected - primary - acceptable),
      valid_percent = 100 * (primary + acceptable) / expected,
      preferred_label = if (is_aqmesh) "Valid AQMesh scaled PM2.5" else "Parameter 88101",
      fallback_label = if (is_aqmesh) "Other valid values" else "Parameter 88502 fallback"
    )
  })
}

tceq_dashboard_server <- function(
    station_index, event_index, load_bundle, cache_manifest = list(),
    cache_stale = FALSE, load_regional = function(name) NULL,
    regional_available = FALSE) {
  if (!"data_source" %in% names(station_index)) {
    station_index$data_source <- "TCEQ"
    station_index$network_label <- "Texas Commission on Environmental Quality"
    station_index$source_station_id <- station_index$aqs_site_id
    station_index$time_label <- "LST"
  }
  if (!"data_source" %in% names(event_index)) {
    event_index <- dplyr::left_join(
      event_index,
      station_index |>
        dplyr::distinct(.data$aqs_site_id, .data$data_source),
      by = "aqs_site_id"
    )
  }
  force(station_index)
  force(event_index)
  force(load_bundle)
  force(cache_manifest)
  force(cache_stale)
  force(load_regional)
  force(regional_available)

  function(input, output, session) {
    rv <- shiny::reactiveValues(
      site = tceq_dashboard_default_site(station_index),
      window_days = 30L,
      pending_date_range = NULL,
      regional_animation = NULL
    )

    available_stations <- shiny::reactive({
      requested <- input$data_source
      if (is.null(requested) || identical(requested, "Combined")) {
        station_index
      } else {
        station_index[station_index$data_source == requested, , drop = FALSE]
      }
    })
    selected_site <- shiny::reactive(rv$site)
    site_record <- shiny::reactive({
      station_index[station_index$aqs_site_id == selected_site(), , drop = FALSE]
    })
    site_bundle <- shiny::reactive(load_bundle(selected_site()))
    date_bounds <- shiny::reactive(tceq_dashboard_date_bounds(site_bundle()))

    input_date_range <- function() {
      dates <- input$date_range
      if (is.null(dates) || length(dates) != 2L) return(NULL)
      dates <- suppressWarnings(as.Date(dates))
      if (anyNA(dates) || dates[1] > dates[2]) return(NULL)
      dates
    }

    remember_current_window <- function() {
      dates <- input_date_range()
      if (is.null(dates)) return(invisible(NULL))
      days <- as.integer(dates[2] - dates[1]) + 1L
      if (days >= 1L && days <= 90L) rv$window_days <- days
      invisible(NULL)
    }

    update_date_window <- function(dates, bounds = date_bounds()) {
      dates <- as.Date(dates)
      rv$pending_date_range <- as.character(dates)
      shiny::updateDateRangeInput(
        session, "date_range", start = dates[1], end = dates[2],
        min = bounds[1], max = bounds[2]
      )
      invisible(dates)
    }

    shiny::observeEvent(input$data_source, {
      stations <- available_stations()
      shiny::req(nrow(stations) > 0L)
      selected <- rv$site
      if (!selected %in% stations$aqs_site_id) {
        remember_current_window()
        selected <- tceq_dashboard_default_site(stations)
        rv$site <- selected
      }
      shiny::updateSelectInput(
        session, "site", choices = tceq_dashboard_site_choices(stations),
        selected = selected
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$site, {
      if (!is.null(input$site) && input$site %in% available_stations()$aqs_site_id) {
        if (!identical(input$site, rv$site)) remember_current_window()
        rv$site <- input$site
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$site_map_marker_click, {
      clicked <- input$site_map_marker_click$id
      if (!is.null(clicked) && clicked %in% available_stations()$aqs_site_id) {
        if (!identical(clicked, rv$site)) remember_current_window()
        rv$site <- clicked
        shiny::updateSelectInput(session, "site", selected = clicked)
      }
    })

    shiny::observeEvent(selected_site(), {
      bundle <- site_bundle()
      bounds <- tceq_dashboard_date_bounds(bundle)
      default_dates <- tceq_dashboard_default_dates(bundle, rv$window_days)
      shiny::updateSelectInput(session, "site", selected = selected_site())
      update_date_window(default_dates, bounds)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$date_range, {
      dates <- input_date_range()
      if (is.null(dates)) return()
      incoming <- as.character(dates)
      if (!is.null(rv$pending_date_range) &&
          identical(incoming, rv$pending_date_range)) {
        rv$pending_date_range <- NULL
      } else {
        rv$pending_date_range <- NULL
        days <- as.integer(dates[2] - dates[1]) + 1L
        if (days >= 1L && days <= 90L) rv$window_days <- days
      }

      days <- as.integer(dates[2] - dates[1]) + 1L
      presets <- c(7L, 14L, 30L, 60L, 90L)
      choices <- stats::setNames(as.character(presets), paste(presets, "days"))
      selected <- as.character(days)
      if (!days %in% presets) {
        choices <- c(choices, stats::setNames("custom", paste0("Custom (", days, " days)")))
        selected <- "custom"
      }
      shiny::updateSelectInput(
        session, "date_window_days", choices = choices, selected = selected
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$date_window_days, {
      if (is.null(input$date_window_days) || input$date_window_days == "custom") return()
      days <- suppressWarnings(as.integer(input$date_window_days))
      if (is.na(days) || !days %in% c(7L, 14L, 30L, 60L, 90L)) return()
      rv$window_days <- days
      dates <- input_date_range()
      anchor <- if (is.null(dates)) date_bounds()[2] else dates[2]
      update_date_window(tceq_dashboard_window_dates(
        date_bounds()[1], date_bounds()[2], days, anchor
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$date_previous, {
      update_date_window(tceq_dashboard_shift_dates(
        selected_dates(), date_bounds()[1], date_bounds()[2], -1L
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$date_next, {
      update_date_window(tceq_dashboard_shift_dates(
        selected_dates(), date_bounds()[1], date_bounds()[2], 1L
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$date_latest, {
      update_date_window(tceq_dashboard_default_dates(
        site_bundle(), rv$window_days
      ))
    }, ignoreInit = TRUE)

    selected_dates <- shiny::reactive({
      bundle <- site_bundle()
      if (is.null(input$date_range) || length(input$date_range) != 2L) {
        return(tceq_dashboard_default_dates(bundle))
      }
      dates <- tryCatch(
        tceq_validate_heatmap_range(input$date_range[1], input$date_range[2]),
        error = function(e) e
      )
      range_message <- if (inherits(dates, "error")) conditionMessage(dates) else ""
      shiny::validate(shiny::need(!inherits(dates, "error"), range_message))
      shiny::validate(shiny::need(
        dates[1] >= min(bundle$hourly$date) && dates[2] <= max(bundle$hourly$date),
        "The selected dates must fall within this site's data coverage."
      ))
      dates
    })

    output$date_window_summary <- shiny::renderUI({
      dates <- selected_dates()
      bounds <- date_bounds()
      pretty_date <- function(x) sub(" 0", " ", format(x, "%b %d, %Y"), fixed = TRUE)
      days <- as.integer(dates[2] - dates[1]) + 1L
      shiny::div(
        class = "date-window-summary",
        shiny::div(sprintf(
          "Showing %s–%s · %d %s",
          pretty_date(dates[1]), pretty_date(dates[2]), days,
          if (days == 1L) "day" else "days"
        )),
        shiny::div(
          class = "date-window-available",
          sprintf(
            "Available %s–%s",
            pretty_date(bounds[1]), pretty_date(bounds[2])
          )
        )
      )
    })

    shiny::observe({
      dates <- selected_dates()
      bounds <- date_bounds()
      session$sendCustomMessage("tceq-set-disabled", list(
        id = "date_previous", disabled = dates[1] <= bounds[1]
      ))
      session$sendCustomMessage("tceq-set-disabled", list(
        id = "date_next", disabled = dates[2] >= bounds[2]
      ))
      session$sendCustomMessage("tceq-set-disabled", list(
        id = "date_latest", disabled = dates[2] >= bounds[2]
      ))
    })

    filtered_hourly <- shiny::reactive({
      dates <- selected_dates()
      site_bundle()$hourly |>
        dplyr::filter(.data$date >= dates[1], .data$date <= dates[2])
    })
    filtered_daily <- shiny::reactive({
      dates <- selected_dates()
      site_bundle()$daily |>
        dplyr::filter(.data$date >= dates[1], .data$date <= dates[2])
    })
    site_events <- shiny::reactive({
      events <- event_index[event_index$aqs_site_id == selected_site(), , drop = FALSE]
      requested <- input$event_type
      if (!is.null(requested) && requested != "all") {
        events <- events[events$event_type == requested, , drop = FALSE]
      }
      events[order(events$start_lstd, decreasing = TRUE), , drop = FALSE]
    })
    event_display <- shiny::reactive(tceq_dashboard_event_table(site_events()))
    selected_event <- shiny::reactive({
      events <- site_events()
      if (!nrow(events)) return(NULL)
      row <- input$event_table_rows_selected
      if (is.null(row) || !length(row) || row[1] > nrow(events)) row <- 1L
      events[row[1], , drop = FALSE]
    })

    output$cache_status <- shiny::renderUI({
      if (isTRUE(cache_stale)) {
        return(shiny::div(
          class = "cache-alert",
          shiny::strong("The dashboard cache is older than the raw download manifest."),
          shiny::div("Run: Rscript --vanilla scripts/08i_update_dfw_pm25_dashboard.R")
        ))
      }
      generated <- cache_manifest$generated_at_utc
      if (is.null(generated)) generated <- "unknown"
      shiny::div(class = "cache-note", paste("Cache prepared", generated))
    })

    output$site_map <- leaflet::renderLeaflet({
      selected <- selected_site()
      locations <- available_stations()
      network_colors <- ifelse(
        locations$data_source == "TCEQ", "#16697A", "#7C3AED"
      )
      marker_colors <- ifelse(
        locations$aqs_site_id == selected, "#D97706", network_colors
      )
      marker_radius <- ifelse(locations$aqs_site_id == selected, 10, 7)
      identity_line <- ifelse(
        locations$data_source == "TCEQ",
        paste0("AQS ", locations$aqs_site_id, " · CAMS ", locations$cams_id),
        as.character(locations$source_station_id)
      )
      popup <- sprintf(
        "<strong>%s</strong><br>%s<br>%s<br>%s<br>%s to %s",
        htmltools::htmlEscape(locations$site_name),
        htmltools::htmlEscape(locations$data_source),
        htmltools::htmlEscape(identity_line),
        htmltools::htmlEscape(ifelse(locations$active, "Active", "Historical")),
        locations$first_date, locations$last_date
      )
      focus <- locations[locations$aqs_site_id == selected, , drop = FALSE]
      leaflet::leaflet(locations) |>
        leaflet::addTiles(
          urlTemplate = paste0(
            "https://server.arcgisonline.com/ArcGIS/rest/services/",
            "Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
          ),
          attribution = paste0(
            "Tiles &copy; Esri, HERE, Garmin, OpenStreetMap contributors, ",
            "and the GIS user community"
          ),
          options = leaflet::tileOptions(maxZoom = 16L)
        ) |>
        leaflet::addTiles(
          urlTemplate = paste0(
            "https://server.arcgisonline.com/ArcGIS/rest/services/",
            "Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"
          ),
          options = leaflet::tileOptions(maxZoom = 16L)
        ) |>
        leaflet::addCircleMarkers(
          lng = ~longitude, lat = ~latitude, layerId = ~aqs_site_id,
          radius = marker_radius, color = marker_colors, weight = 2,
          fillColor = marker_colors,
          fillOpacity = ifelse(locations$active, 0.88, 0.42),
          popup = popup, label = ~site_name
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors = c("#16697A", "#7C3AED", "#D97706"),
          labels = c("TCEQ", "Dallas AQMesh", "Selected"), opacity = 0.9,
          title = "Monitoring network"
        ) |>
        leaflet::setView(focus$longitude[1], focus$latitude[1], zoom = 9)
    })

    output$site_details <- shiny::renderUI({
      x <- site_record()
      identity <- if (x$data_source == "TCEQ") {
        paste("AQS", x$aqs_site_id, "· CAMS", x$cams_id)
      } else {
        pod <- if ("pod_serial_number" %in% names(x)) {
          as.character(x$pod_serial_number)
        } else "not reported"
        paste(x$source_station_id, "· pod", pod)
      }
      shiny::tagList(
        shiny::div(class = "selected-site-name", x$site_name),
        shiny::div(class = "site-meta", paste(x$data_source, "·", identity)),
        shiny::div(class = "site-meta", paste0(x$city, " · ", x$county, " County")),
        shiny::div(
          class = "site-meta",
          paste(x$first_date, "to", x$last_date, "·", x$time_label)
        )
      )
    })

    selected_values <- shiny::reactive({
      x <- filtered_hourly()$composite_pm25_ug_m3
      x[is.finite(x)]
    })
    output$kpi_coverage <- shiny::renderText({
      h <- filtered_hourly()
      sprintf("%.1f%%", 100 * sum(is.finite(h$composite_pm25_ug_m3)) /
                (as.integer(diff(selected_dates())) + 1L) / 24)
    })
    output$kpi_latest <- shiny::renderText({
      finite <- site_bundle()$hourly$date[
        is.finite(site_bundle()$hourly$composite_pm25_ug_m3)
      ]
      if (length(finite)) as.character(max(finite)) else "No valid values"
    })
    output$kpi_max <- shiny::renderText({
      x <- selected_values()
      if (length(x)) sprintf("%.1f µg/m³", max(x)) else "—"
    })
    output$kpi_p95 <- shiny::renderText({
      x <- selected_values()
      if (length(x)) sprintf("%.1f µg/m³", stats::quantile(x, 0.95)) else "—"
    })
    output$kpi_spikes <- shiny::renderText({
      dates <- selected_dates()
      sum(
        site_bundle()$events$event_type == "short_spike" &
          site_bundle()$events$start_date <= dates[2] &
          site_bundle()$events$end_date >= dates[1]
      )
    })
    output$kpi_multiday <- shiny::renderText({
      dates <- selected_dates()
      sum(
        site_bundle()$events$event_type == "multi_day" &
          site_bundle()$events$start_date <= dates[2] &
          site_bundle()$events$end_date >= dates[1]
      )
    })

    output$overview_daily_plot <- plotly::renderPlotly({
      d <- filtered_daily()
      shiny::validate(shiny::need(nrow(d), "No daily summaries in this period."))
      p <- plotly::plot_ly()
      p <- plotly::add_lines(
        p, data = d, x = ~date, y = ~computed_daily_avg_ug_m3,
        name = "Daily average", line = list(color = "#16697A", width = 2),
        text = ~sprintf(
          "%s<br>Average: %.1f µg/m³<br>Valid hours: %d",
          date, computed_daily_avg_ug_m3, valid_hours
        ), hoverinfo = "text"
      )
      p <- plotly::add_lines(
        p, data = d, x = ~date, y = ~computed_daily_max_ug_m3,
        name = "Daily maximum", line = list(color = "#D97706", width = 1.5),
        text = ~sprintf(
          "%s<br>Maximum: %.1f µg/m³<br>Valid hours: %d",
          date, computed_daily_max_ug_m3, valid_hours
        ), hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Date"),
        yaxis = list(title = "PM2.5 (µg/m³)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(t = 55)
      )
      tceq_dashboard_plot_config(p)
    })

    heatmap_data <- shiny::reactive({
      dates <- selected_dates()
      tceq_complete_heatmap(site_bundle()$hourly, dates[1], dates[2]) |>
        dplyr::mutate(
          date_label = factor(
            as.character(.data$date),
            levels = as.character(seq(dates[1], dates[2], by = "day"))
          ),
          hour_label = factor(.data$hour_lstd, levels = sprintf("%02d:00", 0:23)),
          hover = sprintf(
            "%s · %s site local time<br>PM2.5: %s<br>Selection: %s<br>Contributors: %s%s",
            .data$date, .data$hour_lstd,
            ifelse(
              is.finite(.data$composite_pm25_ug_m3),
              sprintf("%.1f µg/m³", .data$composite_pm25_ug_m3), "missing"
            ),
            gsub("_", " ", .data$composite_selection),
            dplyr::coalesce(as.character(.data$contributing_records), "0"),
            ifelse(
              nzchar(.data$value_flag_summary),
              paste0("<br>Flags: ", .data$value_flag_summary), ""
            )
          )
        )
    })

    output$heatmap_plot <- plotly::renderPlotly({
      h <- heatmap_data()
      adaptive <- isTRUE(input$adaptive_heatmap)
      upper <- if (adaptive && any(is.finite(h$composite_pm25_ug_m3))) {
        max(35, stats::quantile(h$composite_pm25_ug_m3, 0.99, na.rm = TRUE))
      } else 50
      g <- ggplot2::ggplot(
        h, ggplot2::aes(
          x = .data$hour_label, y = .data$date_label,
          fill = .data$composite_pm25_ug_m3, text = .data$hover
        )
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c(
          option = "C", limits = c(0, upper), oob = scales::squish,
          na.value = "#D1D5DB", name = "PM2.5\n(µg/m³)"
        ) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::labs(x = "Hour (site local time)", y = "Date") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "right"
        )
      p <- plotly::ggplotly(g, tooltip = "text")
      p <- plotly::layout(p, margin = list(l = 90, r = 70, b = 75, t = 20))
      tceq_dashboard_plot_config(p)
    })

    output$heatmap_caption <- shiny::renderText({
      dates <- selected_dates()
      scale_text <- if (isTRUE(input$adaptive_heatmap)) {
        "Adaptive scale uses at least 35 µg/m³ and the selected period's 99th percentile."
      } else {
        "Fixed scale is capped visually at 50 µg/m³; hover shows uncapped values."
      }
      paste(
        as.integer(dates[2] - dates[1]) + 1L, "days ·", scale_text,
        "Gray cells are missing or quality-coded, not zero."
      )
    })

    output$event_table <- DT::renderDT({
      display <- event_display()
      DT::datatable(
        display,
        rownames = FALSE, filter = "top", selection = "single",
        escape = TRUE,
        options = list(
          pageLength = 10, scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE)),
          order = list(list(2, "desc"))
        )
      ) |>
        DT::formatRound(c("Peak hourly", "Peak daily max", "Highest daily avg"), 1)
    })

    output$event_annual_plot <- plotly::renderPlotly({
      x <- site_bundle()$events |>
        dplyr::count(.data$event_year, .data$event_type, name = "events") |>
        dplyr::mutate(type = dplyr::recode(
          .data$event_type,
          short_spike = "Short-term spike", multi_day = "Multi-day event"
        ))
      shiny::validate(shiny::need(nrow(x), "No events were identified at this site."))
      p <- plotly::plot_ly(
        x, x = ~event_year, y = ~events, color = ~type, type = "bar",
        colors = c("#16697A", "#D97706"),
        text = ~sprintf("%d · %s<br>%d events", event_year, type, events),
        hoverinfo = "text"
      )
      p <- plotly::layout(
        p, barmode = "group", xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Identified events", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(t = 55)
      )
      tceq_dashboard_plot_config(p)
    })

    event_context <- shiny::reactive({
      event <- selected_event()
      if (is.null(event)) return(NULL)
      padding <- if (event$event_type == "short_spike") 6L else 24L
      start <- event$start_lstd - padding * 3600
      end <- event$end_lstd + padding * 3600
      list(event = event, start = start, end = end)
    })

    shiny::observeEvent(event_context(), {
      context <- event_context()
      if (is.null(context)) {
        shiny::updateSelectizeInput(
          session, "raw_series", choices = character(),
          selected = character(), server = TRUE
        )
        return()
      }
      raw <- site_bundle()$raw_hourly |>
        dplyr::filter(
          .data$datetime_lstd >= context$start,
          .data$datetime_lstd <= context$end
        )
      choices <- tceq_dashboard_raw_choices(raw)
      selected <- intersect(shiny::isolate(input$raw_series), unname(choices))
      shiny::updateSelectizeInput(
        session, "raw_series", choices = choices, selected = selected,
        server = TRUE
      )
    }, ignoreNULL = FALSE)

    output$event_details <- shiny::renderUI({
      event <- selected_event()
      if (is.null(event)) return(shiny::p("No event is available for this selection."))
      type <- if (event$event_type == "short_spike") {
        "Short-term spike"
      } else "Multi-day particulate event"
      shiny::div(
        class = "event-detail",
        shiny::strong(type),
        shiny::span(
          if (event$event_type == "short_spike") {
            paste(
              format(event$start_lstd, "%Y-%m-%d %H:00", tz = "UTC"), "to",
              format(event$end_lstd, "%Y-%m-%d %H:00 site local time", tz = "UTC"),
              "·", event$duration_hours,
              if (event$duration_hours == 1L) "hour" else "hours"
            )
          } else {
            paste(event$start_date, "to", event$end_date, "·", event$span_days, "days")
          }
        ),
        if (event$parameter_fallback) {
          shiny::span(class = "qa-warning", "Includes parameter 88502 fallback values")
        },
        if (event$qa_fallback) shiny::span(
          class = "qa-warning",
          if (site_record()$data_source == "Dallas AQMesh") {
            "Dallas AQMesh values are non-regulatory screening measurements"
          } else "Includes QA-fallback values"
        )
      )
    })

    output$event_plot <- plotly::renderPlotly({
      context <- event_context()
      shiny::validate(shiny::need(!is.null(context), "Select an event to display."))
      event <- context$event
      hourly <- site_bundle()$hourly |>
        dplyr::filter(
          .data$datetime_lstd >= context$start,
          .data$datetime_lstd <= context$end
        )
      p <- plotly::plot_ly()
      p <- plotly::add_lines(
        p, data = hourly, x = ~datetime_lstd, y = ~composite_pm25_ug_m3,
        name = "Site composite", line = list(color = "#16697A", width = 2.5),
        text = ~sprintf(
          "%s site local time<br>Composite: %.1f µg/m³<br>Parameter: %s<br>Contributors: %d<br>%s",
          format(datetime_lstd, "%Y-%m-%d %H:00", tz = "UTC"),
          composite_pm25_ug_m3, selected_parameter_code, contributing_records,
          ifelse(
            qa_fallback, "QA fallback",
            ifelse(qa_status_unavailable, "QA status unavailable", "QA-preferred record")
          )
        ), hoverinfo = "text", connectgaps = FALSE
      )
      selected_raw <- input$raw_series
      if (!is.null(selected_raw) && length(selected_raw)) {
        raw <- site_bundle()$raw_hourly |>
          dplyr::filter(
            .data$datetime_lstd >= context$start,
            .data$datetime_lstd <= context$end,
            .data$raw_series_key %in% selected_raw
          )
        for (series in unique(raw$raw_series_key)) {
          one <- raw[raw$raw_series_key == series, , drop = FALSE]
          label <- names(tceq_dashboard_raw_choices(one))[1]
          p <- plotly::add_lines(
            p, data = one, x = ~datetime_lstd, y = ~pm25_ug_m3,
            name = label, line = list(width = 1, dash = "dot"),
            text = ~sprintf(
              "%s site local time<br>Raw: %s<br>Parameter %s · POC %s · table %s<br>Flag: %s",
              format(datetime_lstd, "%Y-%m-%d %H:00", tz = "UTC"),
              ifelse(is.finite(pm25_ug_m3), sprintf("%.1f µg/m³", pm25_ug_m3), pm25_raw),
              parameter_code, poc, report_block,
              ifelse(is.na(value_flag), "none", value_flag)
            ), hoverinfo = "text", connectgaps = FALSE
          )
        }
      }
      shapes <- list()
      annotations <- list()
      if (event$event_type == "short_spike") {
        shapes <- list(
          list(
            type = "rect", xref = "x", yref = "paper",
            x0 = event$start_lstd, x1 = event$end_lstd + 3600,
            y0 = 0, y1 = 1, fillcolor = "rgba(217,119,6,0.16)", line = list(width = 0)
          ),
          list(
            type = "line", xref = "paper", yref = "y", x0 = 0, x1 = 1,
            y0 = 35, y1 = 35, line = list(color = "#B45309", dash = "dash", width = 1.5)
          )
        )
      } else {
        event_days <- site_bundle()$daily |>
          dplyr::filter(.data$date >= event$start_date, .data$date <= event$end_date)
        core <- strsplit(event$core_dates, "|", fixed = TRUE)[[1]]
        trough <- strsplit(event$trough_dates, "|", fixed = TRUE)[[1]]
        shapes <- lapply(seq_len(nrow(event_days)), function(i) {
          day <- event_days$date[i]
          fill <- if (as.character(day) %in% core) {
            "rgba(217,119,6,0.13)"
          } else if (as.character(day) %in% trough) {
            "rgba(107,114,128,0.12)"
          } else "rgba(0,0,0,0)"
          list(
            type = "rect", xref = "x", yref = "paper",
            x0 = tceq_lstd_datetime(day, 0L),
            x1 = tceq_lstd_datetime(day, 23L) + 3600,
            y0 = 0, y1 = 1, fillcolor = fill, line = list(width = 0)
          )
        })
        annotations <- lapply(seq_len(nrow(event_days)), function(i) {
          day <- event_days[i, ]
          list(
            x = tceq_lstd_datetime(day$date, 12L), y = 1.02, xref = "x", yref = "paper",
            text = sprintf(
              "%s<br>max %.1f · avg %.1f · n=%d",
              format(day$date, "%b %d"), day$computed_daily_max_ug_m3,
              day$computed_daily_avg_ug_m3, day$valid_hours
            ), showarrow = FALSE, font = list(size = 10)
          )
        })
      }
      p <- plotly::layout(
        p, xaxis = list(title = "Date and hour (site local time)"),
        yaxis = list(title = "PM2.5 (µg/m³)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.18),
        shapes = shapes, annotations = annotations,
        margin = list(t = if (event$event_type == "multi_day") 95 else 60)
      )
      tceq_dashboard_plot_config(p)
    })

    output$diurnal_plot <- plotly::renderPlotly({
      x <- site_bundle()$hourly |>
        dplyr::filter(is.finite(.data$composite_pm25_ug_m3)) |>
        dplyr::group_by(.data$hour) |>
        dplyr::summarise(
          q25 = stats::quantile(.data$composite_pm25_ug_m3, 0.25),
          median = stats::median(.data$composite_pm25_ug_m3),
          q75 = stats::quantile(.data$composite_pm25_ug_m3, 0.75),
          .groups = "drop"
        )
      p <- plotly::plot_ly(x, x = ~hour)
      p <- plotly::add_ribbons(
        p, ymin = ~q25, ymax = ~q75, name = "Interquartile range",
        fillcolor = "rgba(22,105,122,0.18)", line = list(color = "transparent"),
        hoverinfo = "skip"
      )
      p <- plotly::add_lines(
        p, y = ~median, name = "Median", line = list(color = "#16697A", width = 2.5),
        text = ~sprintf("%02d:00 site local time<br>Median: %.1f µg/m³<br>IQR: %.1f–%.1f", hour, median, q25, q75),
        hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Hour (site local time)", dtick = 2),
        yaxis = list(title = "PM2.5 (µg/m³)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 55)
      )
      tceq_dashboard_plot_config(p)
    })

    output$seasonal_plot <- plotly::renderPlotly({
      x <- site_bundle()$hourly |>
        dplyr::filter(is.finite(.data$composite_pm25_ug_m3)) |>
        dplyr::mutate(month = as.integer(format(.data$date, "%m"))) |>
        dplyr::group_by(.data$month) |>
        dplyr::summarise(
          q25 = stats::quantile(.data$composite_pm25_ug_m3, 0.25),
          median = stats::median(.data$composite_pm25_ug_m3),
          q75 = stats::quantile(.data$composite_pm25_ug_m3, 0.75), .groups = "drop"
        )
      x$month_name <- factor(month.abb[x$month], levels = month.abb)
      p <- plotly::plot_ly(x, x = ~month_name)
      p <- plotly::add_ribbons(
        p, ymin = ~q25, ymax = ~q75, name = "Interquartile range",
        fillcolor = "rgba(217,119,6,0.18)", line = list(color = "transparent"),
        hoverinfo = "skip"
      )
      p <- plotly::add_lines(
        p, y = ~median, name = "Median", line = list(color = "#D97706", width = 2.5),
        text = ~sprintf("%s<br>Median: %.1f µg/m³<br>IQR: %.1f–%.1f", month_name, median, q25, q75),
        hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Month"),
        yaxis = list(title = "PM2.5 (µg/m³)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 55)
      )
      tceq_dashboard_plot_config(p)
    })

    output$long_term_plot <- plotly::renderPlotly({
      d <- site_bundle()$daily
      p <- plotly::plot_ly()
      p <- plotly::add_lines(
        p, data = d, x = ~date, y = ~computed_daily_avg_ug_m3,
        name = "Daily average", line = list(color = "#16697A", width = 1),
        hovertemplate = "%{x|%Y-%m-%d}<br>Average %{y:.1f} µg/m³<extra></extra>"
      )
      p <- plotly::add_lines(
        p, data = d, x = ~date, y = ~computed_daily_max_ug_m3,
        name = "Daily maximum", line = list(color = "#D97706", width = 0.8),
        hovertemplate = "%{x|%Y-%m-%d}<br>Maximum %{y:.1f} µg/m³<extra></extra>"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Date", rangeslider = list(visible = TRUE)),
        yaxis = list(title = "PM2.5 (µg/m³)", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 55)
      )
      tceq_dashboard_plot_config(p)
    })

    yearly_quality <- shiny::reactive(tceq_dashboard_yearly_quality(site_bundle()))
    output$quality_plot <- plotly::renderPlotly({
      x <- yearly_quality() |>
        tidyr::pivot_longer(
          c("preferred_hours", "fallback_hours", "missing_hours"),
          names_to = "category", values_to = "hours"
        ) |>
        dplyr::mutate(
          percent = 100 * .data$hours / .data$expected_hours,
          category_label = dplyr::case_when(
            .data$category == "preferred_hours" ~ .data$preferred_label,
            .data$category == "fallback_hours" ~ .data$fallback_label,
            TRUE ~ "Missing / flagged"
          )
        )
      p <- plotly::plot_ly(
        x, x = ~year, y = ~percent, color = ~category_label, type = "bar",
        colors = c("#16697A", "#D97706", "#9CA3AF"),
        text = ~sprintf("%d · %s<br>%.1f%% (%s hours)", year, category_label, percent, format(hours, big.mark = ",")),
        hoverinfo = "text"
      )
      p <- plotly::layout(
        p, barmode = "stack", xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Percent of expected site-hours", range = c(0, 100)),
        legend = list(orientation = "h", x = 0, y = 1.18), margin = list(t = 70)
      )
      tceq_dashboard_plot_config(p)
    })

    output$quality_summary <- shiny::renderUI({
      raw <- site_bundle()$raw_hourly
      hourly <- site_bundle()$hourly
      finite <- is.finite(hourly$composite_pm25_ug_m3)
      negative <- sum(raw$value_flag == "NEG", na.rm = TRUE)
      character_flags <- sum(!is.na(raw$value_flag) & raw$value_flag != "NEG")
      fallback <- sum(hourly$qa_fallback & finite)
      parameter_fallback <- sum(hourly$parameter_fallback & finite)
      if (site_record()$data_source == "Dallas AQMesh") {
        revisions <- if ("revision_count" %in% names(raw)) {
          sum(pmax(raw$revision_count - 1L, 0L), na.rm = TRUE)
        } else 0L
        excluded_status <- sum(grepl("^STATUS_", raw$value_flag), na.rm = TRUE)
        return(shiny::div(
          class = "quality-summary",
          shiny::span(shiny::strong(format(sum(finite), big.mark = ",")), " valid composite hours"),
          shiny::span(shiny::strong(format(revisions, big.mark = ",")), " superseded source revisions"),
          shiny::span(shiny::strong(format(excluded_status, big.mark = ",")), " status-excluded latest records"),
          shiny::span(shiny::strong(format(negative, big.mark = ",")), " negative latest records"),
          shiny::span(shiny::strong(format(character_flags, big.mark = ",")), " flagged latest records")
        ))
      }
      shiny::div(
        class = "quality-summary",
        shiny::span(shiny::strong(format(sum(finite), big.mark = ",")), " valid composite hours"),
        shiny::span(
          shiny::strong(format(parameter_fallback, big.mark = ",")),
          " parameter-88502 fallback hours"
        ),
        shiny::span(shiny::strong(format(fallback, big.mark = ",")), " QA-fallback hours"),
        shiny::span(shiny::strong(format(negative, big.mark = ",")), " negative raw cells"),
        shiny::span(shiny::strong(format(character_flags, big.mark = ",")), " character-coded raw cells")
      )
    })

    output$flag_table <- DT::renderDT({
      flags <- site_bundle()$raw_hourly |>
        dplyr::filter(!is.na(.data$value_flag), nzchar(.data$value_flag)) |>
        dplyr::count(.data$value_flag, name = "Raw cells", sort = TRUE) |>
        dplyr::rename(`Source flag` = "value_flag")
      if (!nrow(flags)) flags <- data.frame(`Source flag` = character(), `Raw cells` = integer())
      DT::datatable(
        flags, rownames = FALSE, options = list(pageLength = 10, dom = "tip"),
        selection = "none"
      )
    })

    output$daily_difference_plot <- plotly::renderPlotly({
      d <- site_bundle()$daily |>
        dplyr::filter(is.finite(.data$computed_minus_reported_avg_ug_m3))
      shiny::validate(shiny::need(nrow(d), "No comparable reported daily averages are available."))
      p <- plotly::plot_ly(
        d, x = ~date, y = ~computed_minus_reported_avg_ug_m3,
        type = "scatter", mode = "markers", marker = list(color = "#16697A", size = 5),
        text = ~sprintf(
          "%s<br>Composite minus reported average: %+.2f µg/m³<br>Reported tables: %d",
          date, computed_minus_reported_avg_ug_m3, reported_daily_records
        ), hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Date"),
        yaxis = list(title = "Computed composite − median reported average (µg/m³)", zeroline = TRUE),
        margin = list(t = 20)
      )
      tceq_dashboard_plot_config(p)
    })

    output$source_method_note <- shiny::renderUI({
      if (site_record()$data_source == "Dallas AQMesh") {
        shiny::p(
          class = "method-note",
          paste(
            "Dallas AQMesh uses the City's scaled PM2.5 field and the latest append-date revision for each pod-hour.",
            "Sentinel, negative, and fault-status values remain traceable in the raw archive but are excluded from calculations.",
            "These community-sensor values are non-regulatory, unverified screening data and are not NAAQS determinations."
          )
        )
      } else {
        shiny::p(
          class = "method-note",
          paste(
            "TCEQ parameter 88101 is preferred for every site-hour; parameter 88502 is used only when 88101 is unavailable.",
            "Negative and character-coded cells remain in the raw cache with explicit flags and are excluded from calculations.",
            "TCEQ notes that current monitoring data are unofficial until certified."
          )
        )
      }
    })

    regional_events <- event_index[event_index$event_type == "multi_day", , drop = FALSE]
    regional_events$event_key <- pm25_event_key(
      regional_events$aqs_site_id, regional_events$event_id
    )

    regional_anchor <- shiny::reactive({
      shiny::req(nrow(regional_events) > 0L)
      key <- input$regional_anchor
      shiny::req(length(key) == 1L, !is.na(key), nzchar(key))
      row <- match(key, regional_events$event_key)
      shiny::req(!is.na(row))
      regional_events[row, , drop = FALSE]
    })

    shiny::observeEvent(regional_anchor(), {
      event <- regional_anchor()
      start <- as.Date(event$start_date) - 2L
      end <- as.Date(event$end_date) + 2L
      if (as.integer(end - start) + 1L > 90L) end <- start + 89L
      shiny::updateDateRangeInput(
        session, "regional_dates", start = start, end = end,
        min = min(station_index$first_date), max = max(station_index$last_date)
      )
    }, ignoreInit = FALSE)

    regional_dates <- shiny::reactive({
      dates <- suppressWarnings(as.Date(input$regional_dates))
      shiny::validate(shiny::need(
        length(dates) == 2L && !anyNA(dates) && dates[1] <= dates[2],
        "Select a valid regional date range."
      ))
      shiny::validate(shiny::need(
        as.integer(dates[2] - dates[1]) + 1L <= 90L,
        "Regional daily views are limited to 90 days."
      ))
      dates
    })

    shiny::observeEvent(input$regional_metric, {
      if (!identical(input$regional_metric, "hourly")) return()
      dates <- suppressWarnings(as.Date(input$regional_dates))
      if (length(dates) != 2L || anyNA(dates)) return()
      if (as.integer(dates[2] - dates[1]) + 1L > 7L) {
        shiny::updateDateRangeInput(
          session, "regional_dates", start = dates[1], end = dates[1] + 6L
        )
      }
    }, ignoreInit = TRUE)

    regional_daily <- shiny::reactive({
      shiny::validate(shiny::need(
        isTRUE(regional_available),
        "Regional caches are unavailable. Run scripts/08i_update_dfw_pm25_dashboard.R."
      ))
      load_regional("daily")
    })
    regional_reach <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(regional_available), "Regional cache unavailable."))
      load_regional("reach")
    })
    regional_links <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(regional_available), "Regional cache unavailable."))
      load_regional("links")
    })

    regional_event_filter_data <- shiny::reactive({
      shiny::req(
        identical(input$dashboard_tabs, "regional"),
        identical(input$regional_tabs, "Event alignment")
      )
      links <- regional_links()
      aligned <- links |>
        dplyr::filter(.data$relationship == "shared_core") |>
        dplyr::group_by(.data$anchor_event_key) |>
        dplyr::summarise(
          aligned_other_sites = dplyr::n_distinct(.data$other_site_id),
          .groups = "drop"
        )
      regional_events |>
        dplyr::left_join(
          station_index |>
            dplyr::select("aqs_site_id", "site_name") |>
            dplyr::distinct(),
          by = "aqs_site_id"
        ) |>
        dplyr::left_join(aligned, by = c("event_key" = "anchor_event_key")) |>
        dplyr::mutate(
          aligned_other_sites = dplyr::coalesce(.data$aligned_other_sites, 0L),
          aligned_sites = .data$aligned_other_sites + 1L
        )
    })

    regional_filtered_events <- shiny::reactive({
      data <- regional_event_filter_data()
      network <- input$regional_event_network
      minimum_days <- suppressWarnings(as.integer(input$regional_event_min_days))
      minimum_sites <- suppressWarnings(as.integer(input$regional_event_min_sites))
      if (length(minimum_days) != 1L || is.na(minimum_days)) minimum_days <- 2L
      if (length(minimum_sites) != 1L || is.na(minimum_sites)) minimum_sites <- 1L
      if (length(network) == 1L && !is.na(network) && network != "All") {
        data <- data[data$data_source == network, , drop = FALSE]
      }
      data |>
        dplyr::filter(
          .data$span_days >= minimum_days,
          .data$aligned_sites >= minimum_sites
        ) |>
        dplyr::arrange(dplyr::desc(.data$start_date), .data$site_name)
    })

    shiny::observeEvent(
      list(
        input$dashboard_tabs, input$regional_tabs,
        input$regional_event_network, input$regional_event_min_days,
        input$regional_event_min_sites
      ),
      {
        if (!identical(input$dashboard_tabs, "regional") ||
            !identical(input$regional_tabs, "Event alignment")) return()
        data <- regional_filtered_events()
        choices <- if (nrow(data)) {
          stats::setNames(
            data$event_key,
            paste0(
              data$site_name, " · ", data$data_source, " · ",
              data$start_date, " to ", data$end_date, " · ",
              data$span_days, " days · ", data$aligned_sites, " aligned sites"
            )
          )
        } else character()
        current <- shiny::isolate(input$regional_anchor)
        selected <- if (length(current) == 1L && current %in% unname(choices)) {
          current
        } else if (length(choices)) unname(choices[[1]]) else character()
        shiny::updateSelectizeInput(
          session, "regional_anchor", choices = choices,
          selected = selected, server = FALSE
        )
      }, ignoreInit = FALSE
    )

    output$regional_event_filter_status <- shiny::renderUI({
      data <- regional_filtered_events()
      total <- nrow(regional_event_filter_data())
      shiny::p(
        class = "method-note regional-filter-status",
        paste0(
          "Showing ", nrow(data), " of ", total, " MDPEs. Aligned sites include ",
          "the anchor plus sites whose existing MDPE shares a core date."
        )
      )
    })
    regional_spatial <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(regional_available), "Regional cache unavailable."))
      load_regional("spatial")
    })
    regional_validation <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(regional_available), "Regional cache unavailable."))
      load_regional("validation")
    })

    regional_window_daily <- shiny::reactive({
      dates <- regional_dates()
      regional_daily() |>
        dplyr::filter(.data$date >= dates[1], .data$date <= dates[2])
    })
    regional_window_reach <- shiny::reactive({
      dates <- regional_dates()
      regional_reach() |>
        dplyr::filter(.data$date >= dates[1], .data$date <= dates[2])
    })
    regional_anchor_links <- shiny::reactive({
      links <- regional_links()
      if (is.null(links) || !nrow(links)) return(links)
      links[links$anchor_event_key == regional_anchor()$event_key, , drop = FALSE]
    })

    regional_alignment_data <- shiny::reactive({
      dates <- regional_dates()
      data <- regional_window_daily() |>
        dplyr::mutate(status = dplyr::case_when(
          .data$event_core ~ "Core day",
          .data$event_trough ~ "Trough day",
          .data$event_active ~ "Event span",
          .data$event_eligible ~ "Eligible non-event",
          .data$valid_hours > 0L ~ "Incomplete",
          TRUE ~ "Missing"
        ))
      grid <- tidyr::crossing(
        aqs_site_id = station_index$aqs_site_id,
        date = seq(dates[1], dates[2], by = "day")
      ) |>
        dplyr::left_join(
          data |>
            dplyr::select(
              "aqs_site_id", "date", "status",
              "computed_daily_avg_ug_m3", "computed_daily_max_ug_m3",
              "valid_hours"
            ),
          by = c("aqs_site_id", "date")
        ) |>
        dplyr::left_join(
          station_index |>
            dplyr::select(
              "aqs_site_id", "site_name", "data_source"
            ),
          by = "aqs_site_id"
        ) |>
        dplyr::mutate(
          status = dplyr::coalesce(.data$status, "Missing"),
          site_label = paste0(
            ifelse(.data$data_source == "TCEQ", "TCEQ · ", "AQMesh · "),
            .data$site_name
          ),
          hover = sprintf(
            "%s<br>%s<br>%s<br>Average: %s<br>Maximum: %s<br>Valid hours: %s",
            .data$site_label, .data$date, .data$status,
            ifelse(
              is.finite(.data$computed_daily_avg_ug_m3),
              sprintf("%.1f µg/m³", .data$computed_daily_avg_ug_m3), "missing"
            ),
            ifelse(
              is.finite(.data$computed_daily_max_ug_m3),
              sprintf("%.1f µg/m³", .data$computed_daily_max_ug_m3), "missing"
            ),
            dplyr::coalesce(as.character(.data$valid_hours), "0")
          )
        )
      site_levels <- station_index |>
        dplyr::arrange(.data$data_source, .data$site_name) |>
        dplyr::transmute(label = paste0(
          ifelse(.data$data_source == "TCEQ", "TCEQ · ", "AQMesh · "),
          .data$site_name
        )) |>
        dplyr::pull(.data$label)
      grid |>
        dplyr::mutate(
          status = factor(
            .data$status,
            levels = c(
              "Missing", "Incomplete", "Eligible non-event", "Event span",
              "Trough day", "Core day"
            )
          ),
          site_label = factor(.data$site_label, levels = rev(site_levels))
        )
    })

    output$regional_alignment_plot <- plotly::renderPlotly({
      data <- regional_alignment_data()
      shiny::validate(shiny::need(nrow(data), "No daily records in this window."))
      colors <- c(
        "Missing" = "#D1D5DB", "Incomplete" = "#9CA3AF",
        "Eligible non-event" = "#F3F4F6", "Event span" = "#93C5FD",
        "Trough day" = "#F59E0B", "Core day" = "#B91C1C"
      )
      g <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = .data$date, y = .data$site_label, fill = .data$status,
          text = .data$hover
        )
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
        ggplot2::labs(x = "Date", y = NULL, fill = "Daily status") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 8),
          legend.position = "top"
        )
      p <- plotly::ggplotly(g, tooltip = "text")
      p <- plotly::layout(p, margin = list(l = 210, r = 20, t = 45, b = 55))
      tceq_dashboard_plot_config(p)
    })

    output$regional_reach_plot <- plotly::renderPlotly({
      reach <- regional_window_reach()
      shiny::validate(shiny::need(nrow(reach), "No reach summaries in this window."))
      p <- plotly::plot_ly()
      p <- plotly::add_lines(
        p, data = reach, x = ~date, y = ~balanced_participation_percent,
        name = "Balanced reach", line = list(color = "#16697A", width = 3),
        text = ~sprintf(
          "%s<br>Balanced reach: %s<br>Affected: %d of %d<br>%s",
          date,
          ifelse(
            is.finite(balanced_participation_percent),
            sprintf("%.1f%%", balanced_participation_percent), "unavailable"
          ), affected_sites, eligible_sites, coverage_label
        ), hoverinfo = "text"
      )
      p <- plotly::add_lines(
        p, data = reach, x = ~date, y = ~affected_percent_tceq,
        name = "TCEQ", line = list(color = "#0F766E", dash = "dot"),
        hoverinfo = "x+y"
      )
      p <- plotly::add_lines(
        p, data = reach, x = ~date, y = ~affected_percent_aqmesh,
        name = "AQMesh", line = list(color = "#7C3AED", dash = "dot"),
        hoverinfo = "x+y"
      )
      p <- plotly::add_bars(
        p, data = reach, x = ~date, y = ~event_active_sites,
        name = "Event-active sites (core + trough)", yaxis = "y2",
        marker = list(color = "rgba(245, 158, 11, 0.28)"),
        text = ~sprintf(
          "%s<br>Event-active sites: %d<br>Core-day sites: %d",
          date, event_active_sites, affected_sites
        ), hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Date"),
        yaxis = list(title = "Affected eligible sites (%)", range = c(0, 100)),
        yaxis2 = list(
          title = "Event-active site count", overlaying = "y", side = "right",
          rangemode = "tozero", showgrid = FALSE
        ),
        barmode = "overlay",
        legend = list(orientation = "h", x = 0, y = 1.14),
        margin = list(t = 55, r = 65)
      )
      tceq_dashboard_plot_config(p)
    })

    output$regional_coverage_badge <- shiny::renderUI({
      peak <- regional_peak()
      if (is.null(peak) || peak$represented_networks != 1L) return(NULL)
      shiny::span(
        class = "single-network-badge",
        paste0("Single-network coverage · ", peak$coverage_label)
      )
    })

    regional_overlap_display <- shiny::reactive({
      links <- regional_anchor_links()
      if (is.null(links) || !nrow(links)) return(tibble::tibble())
      links |>
        dplyr::transmute(
          Relationship = dplyr::recode(
            .data$relationship,
            shared_core = "Shared core date", span_only = "Span only"
          ),
          Network = .data$other_data_source,
          Site = .data$other_site_name,
          `Event start` = .data$other_start_date,
          `Event end` = .data$other_end_date,
          `Shared core days` = .data$shared_core_days,
          `Span overlap days` = .data$span_overlap_days,
          `Start lag days` = .data$start_lag_days,
          `Distance km` = .data$distance_km,
          `Peak hourly` = .data$peak_hourly_ug_m3,
          `Peak daily max` = .data$peak_daily_max_ug_m3,
          `Highest daily avg` = .data$max_daily_avg_ug_m3,
          `Coverage warning` = .data$coverage_warning
        )
    })

    output$regional_overlap_table <- DT::renderDT({
      display <- regional_overlap_display()
      DT::datatable(
        display, rownames = FALSE, filter = "top", selection = "single",
        options = list(pageLength = 8, scrollX = TRUE)
      ) |>
        DT::formatRound(
          c("Distance km", "Peak hourly", "Peak daily max", "Highest daily avg"), 1
        )
    })

    regional_peak <- shiny::reactive({
      reach <- regional_window_reach()
      if (!nrow(reach) || all(!is.finite(reach$balanced_participation_percent))) {
        return(NULL)
      }
      reach[which.max(reach$balanced_participation_percent), , drop = FALSE]
    })
    output$regional_kpi_reach <- shiny::renderText({
      peak <- regional_peak()
      if (is.null(peak)) "Unavailable" else sprintf(
        "%.1f%%", peak$balanced_participation_percent
      )
    })
    output$regional_kpi_sites <- shiny::renderText({
      links <- regional_anchor_links()
      if (is.null(links) || !nrow(links)) return("0")
      length(unique(links$other_site_id[links$relationship == "shared_core"]))
    })
    output$regional_kpi_date <- shiny::renderText({
      peak <- regional_peak()
      if (is.null(peak)) "—" else as.character(peak$date)
    })
    output$regional_kpi_coverage <- shiny::renderText({
      peak <- regional_peak()
      if (is.null(peak)) "Insufficient" else paste0(
        peak$affected_sites, " of ", peak$eligible_sites, " · ", peak$coverage_label
      )
    })

    regional_hourly_window <- shiny::reactive({
      dates <- regional_dates()
      index <- load_regional("hourly_index")
      years <- seq.int(
        as.integer(format(dates[1] - 1L, "%Y")),
        as.integer(format(dates[2] + 1L, "%Y"))
      )
      years <- intersect(years, index$years$year)
      shards <- lapply(years, function(year) load_regional(paste0("hourly_", year)))
      shards <- Filter(Negate(is.null), shards)
      if (!length(shards)) return(NULL)
      list(
        timestamp_utc = do.call(c, lapply(shards, `[[`, "timestamp_utc")),
        site_ids = index$site_ids,
        data_source = index$data_source,
        values = do.call(rbind, lapply(shards, `[[`, "values"))
      )
    })

    regional_frame_sequence <- shiny::reactive({
      dates <- regional_dates()
      if (identical(input$regional_metric, "hourly")) {
        shiny::validate(shiny::need(
          as.integer(dates[2] - dates[1]) + 1L <= 7L,
          "Hourly mapping is limited to seven days."
        ))
        hourly <- regional_hourly_window()
        shiny::validate(shiny::need(!is.null(hourly), "No hourly data in this window."))
        local_dates <- as.Date(format(
          hourly$timestamp_utc, tz = "America/Chicago", usetz = FALSE
        ))
        selected <- which(local_dates >= dates[1] & local_dates <= dates[2])
        return(hourly$timestamp_utc[head(selected, 168L)])
      }
      seq(dates[1], dates[2], by = "day")
    })

    output$regional_frame_control <- shiny::renderUI({
      frames <- regional_frame_sequence()
      if (!length(frames)) return(shiny::helpText("No frames are available."))
      value <- shiny::isolate(input$regional_frame)
      if (length(value) != 1L || is.na(value) ||
          value < 1L || value > length(frames)) value <- 1L
      interval <- suppressWarnings(as.integer(input$regional_speed))
      if (length(interval) != 1L || is.na(interval)) interval <- 1000L
      shiny::sliderInput(
        "regional_frame", "Animation frame", min = 1L, max = length(frames),
        value = value, step = 1L,
        animate = shiny::animationOptions(
          interval = interval,
          loop = FALSE, playButton = "Play", pauseButton = "Pause"
        )
      )
    })

    regional_frame_index <- shiny::reactive({
      frames <- regional_frame_sequence()
      index <- suppressWarnings(as.integer(input$regional_frame))
      if (length(index) != 1L || is.na(index) ||
          index < 1L || index > length(frames)) index <- 1L
      index
    })

    regional_frame_values <- function(frame) {
      metric <- input$regional_metric
      if (identical(metric, "hourly")) {
        hourly <- regional_hourly_window()
        row <- match(as.numeric(frame), as.numeric(hourly$timestamp_utc))
        values <- if (is.na(row)) rep(NA_real_, length(hourly$site_ids)) else
          hourly$values[row, ]
        return(stats::setNames(as.numeric(values), hourly$site_ids))
      }
      field <- if (identical(metric, "daily_max")) {
        "computed_daily_max_ug_m3"
      } else "computed_daily_avg_ug_m3"
      data <- regional_daily()
      data <- data[data$date == as.Date(frame), , drop = FALSE]
      values <- data[[field]]
      values[!data$event_eligible] <- NA_real_
      stats::setNames(values, data$aqs_site_id)
    }

    regional_surface_key <- shiny::reactive({
      paste(
        input$regional_metric, input$regional_map_view,
        paste(regional_dates(), collapse = ":"), sep = "|"
      )
    })

    shiny::observeEvent(input$regional_generate, {
      frames <- regional_frame_sequence()
      shiny::validate(shiny::need(length(frames), "No frames are available."))
      spatial <- regional_spatial()
      rv$regional_animation <- NULL
      session$sendCustomMessage(
        "tceq-set-disabled", list(id = "regional_generate", disabled = TRUE)
      )
      shiny::updateActionButton(
        session, "regional_generate", label = "Generating animation…",
        icon = shiny::icon("spinner", class = "fa-spin")
      )
      on.exit({
        session$sendCustomMessage(
          "tceq-set-disabled", list(id = "regional_generate", disabled = FALSE)
        )
        shiny::updateActionButton(
          session, "regional_generate", label = "Generate & play animation",
          icon = shiny::icon("play")
        )
      }, add = TRUE)
      surfaces <- vector("list", length(frames))
      shiny::withProgress(message = "Generating regional animation", value = 0, {
        for (i in seq_along(frames)) {
          surfaces[[i]] <- pm25_interpolate_surface(
            regional_frame_values(frames[[i]]), spatial,
            view = input$regional_map_view
          )
          shiny::incProgress(1 / length(frames))
        }
      })
      rv$regional_animation <- list(
        key = regional_surface_key(), frames = surfaces
      )
      shiny::updateSliderInput(session, "regional_frame", value = 1L)
      if (length(frames) > 1L) {
        session$onFlushed(function() {
          session$sendCustomMessage(
            "pm25-start-animation", list(id = "regional_frame")
          )
        }, once = TRUE)
      }
      shiny::showNotification(
        paste(length(frames), "frames ready; playback started."),
        type = "message", duration = 4
      )
    }, ignoreInit = TRUE)

    output$regional_animation_status <- shiny::renderUI({
      frames <- regional_frame_sequence()
      cached <- rv$regional_animation
      ready <- !is.null(cached) &&
        identical(cached$key, regional_surface_key()) &&
        length(cached$frames) == length(frames)
      if (ready) {
        return(shiny::div(
          class = "cache-note",
          paste(
            length(frames),
            "frames are cached. Use Play, Pause, or the frame slider to review them."
          )
        ))
      }
      shiny::div(
        class = "cache-note",
        paste(
          "The current frame is shown now. Generate the",
          length(frames), "frame animation to cache the sequence and start playback."
        )
      )
    })

    regional_surface <- shiny::reactive({
      frames <- regional_frame_sequence()
      shiny::req(length(frames))
      index <- regional_frame_index()
      cached <- rv$regional_animation
      if (!is.null(cached) && identical(cached$key, regional_surface_key()) &&
          length(cached$frames) >= index) {
        return(cached$frames[[index]])
      }
      pm25_interpolate_surface(
        regional_frame_values(frames[[index]]), regional_spatial(),
        view = input$regional_map_view
      )
    })

    regional_map_base <- function(network = NULL) {
      spatial <- regional_spatial()
      locations <- station_index
      if (!is.null(network)) {
        locations <- locations[locations$data_source == network, , drop = FALSE]
      }
      map <- leaflet::leaflet(locations) |>
        leaflet::addTiles(
          urlTemplate = paste0(
            "https://server.arcgisonline.com/ArcGIS/rest/services/",
            "Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
          ),
          attribution = paste0(
            "Tiles &copy; Esri, HERE, Garmin, OpenStreetMap contributors, ",
            "and the GIS user community"
          ),
          options = leaflet::tileOptions(maxZoom = 16L)
        )
      for (line in split(spatial$boundary_lines, spatial$boundary_lines$group)) {
        map <- leaflet::addPolylines(
          map, data = line, lng = ~longitude, lat = ~latitude,
          color = "#4B5563", weight = 1, opacity = 0.65,
          options = leaflet::pathOptions(interactive = FALSE)
        )
      }
      map |>
        leaflet::addCircleMarkers(
          data = locations, lng = ~longitude, lat = ~latitude,
          radius = ifelse(locations$data_source == "TCEQ", 7, 4),
          color = ifelse(locations$data_source == "TCEQ", "#0F766E", "#7C3AED"),
          fillColor = ifelse(
            locations$data_source == "TCEQ", "#0F766E", "#7C3AED"
          ),
          fillOpacity = 0.9, weight = 2, group = "measurements",
          label = ~paste(site_name, "·", data_source)
        ) |>
        leaflet::fitBounds(
          spatial$bounds[["west"]], spatial$bounds[["south"]],
          spatial$bounds[["east"]], spatial$bounds[["north"]]
        )
    }

    output$regional_surface_map <- leaflet::renderLeaflet({
      regional_map_base()
    })
    output$regional_tceq_map <- leaflet::renderLeaflet({
      regional_map_base("TCEQ")
    })
    output$regional_aqmesh_map <- leaflet::renderLeaflet({
      regional_map_base("Dallas AQMesh")
    })

    regional_surface_payload <- function(surface, view, id) {
      spatial <- regional_spatial()
      values <- surface$value
      supported <- surface$supported
      upper <- if (identical(view, "Disagreement")) 20 else 50
      categorical <- NULL
      adaptive_active <- FALSE
      if (identical(input$regional_layer, "support_count")) {
        values <- if (identical(view, "TCEQ")) surface$tceq$nearby_count else
          if (identical(view, "Dallas AQMesh")) surface$aqmesh$nearby_count else
            surface$nearby_count
        supported <- spatial$grid$inside_region
        upper <- 8
      } else if (identical(input$regional_layer, "nearest_distance")) {
        values <- if (identical(view, "TCEQ")) surface$tceq$nearest_m else
          if (identical(view, "Dallas AQMesh")) surface$aqmesh$nearest_m else
            surface$nearest_m
        values <- values / 1000
        supported <- spatial$grid$inside_region & is.finite(values)
        upper <- 25
      } else if (identical(input$regional_layer, "network_contribution")) {
        values <- if (identical(view, "TCEQ")) {
          ifelse(surface$tceq$supported, 1, NA_real_)
        } else if (identical(view, "Dallas AQMesh")) {
          ifelse(surface$aqmesh$supported, 2, NA_real_)
        } else match(
          surface$contribution, c("TCEQ only", "AQMesh only", "Both networks")
        )
        supported <- spatial$grid$inside_region & is.finite(values)
        upper <- 3
        categorical <- "contribution"
      } else if (isTRUE(input$regional_adaptive) &&
                 (is.null(rv$regional_animation) ||
                    !identical(rv$regional_animation$key, regional_surface_key())) &&
                 any(is.finite(values))) {
        upper <- max(10, stats::quantile(values, 0.99, na.rm = TRUE))
        adaptive_active <- TRUE
      }
      values[!supported | !spatial$grid$inside_region] <- NA_real_
      rows <- length(spatial$y_values)
      columns <- length(spatial$x_values)
      canvas <- rep(NA_real_, rows * columns)
      cell <- (spatial$grid$row - 1L) * columns + spatial$grid$column
      canvas[cell] <- values
      sensors <- if (identical(view, "TCEQ")) {
        "TCEQ"
      } else if (identical(view, "Dallas AQMesh")) {
        "Dallas AQMesh"
      } else c("TCEQ", "Dallas AQMesh")
      legend <- if (identical(input$regional_layer, "network_contribution")) {
        list(type = "categorical", title = "Contributing network", sensors = sensors)
      } else {
        title <- if (identical(input$regional_layer, "support_count")) {
          "Nearby valid sensors"
        } else if (identical(input$regional_layer, "nearest_distance")) {
          "Nearest valid sensor (km)"
        } else if (identical(view, "Disagreement")) {
          "Absolute network difference (µg/m³)"
        } else "PM2.5 (µg/m³)"
        high <- if (identical(input$regional_layer, "concentration") &&
                    !adaptive_active) {
          paste0(format(round(upper, 1), trim = TRUE), "+")
        } else format(round(upper, 1), trim = TRUE)
        list(
          type = "continuous", title = title, low = "0",
          mid = format(round(upper / 2, 1), trim = TRUE), high = high,
          sensors = sensors
        )
      }
      list(
        id = id, rows = rows, cols = columns, values = canvas,
        upper = unname(upper), categorical = categorical,
        bounds = as.list(spatial$bounds), fit = FALSE, legend = legend
      )
    }

    update_regional_markers <- function(map_id, values, network = NULL) {
      locations <- station_index
      if (!is.null(network)) {
        locations <- locations[locations$data_source == network, , drop = FALSE]
      }
      locations$value <- unname(values[match(locations$aqs_site_id, names(values))])
      label <- sprintf(
        "%s · %s<br>PM2.5: %s",
        htmltools::htmlEscape(locations$site_name),
        htmltools::htmlEscape(locations$data_source),
        ifelse(
          is.finite(locations$value),
          sprintf("%.1f µg/m³", locations$value), "missing"
        )
      )
      leaflet::leafletProxy(map_id, session = session, data = locations) |>
        leaflet::clearGroup("measurements") |>
        leaflet::addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ifelse(locations$data_source == "TCEQ", 7, 4),
          color = ifelse(locations$data_source == "TCEQ", "#0F766E", "#7C3AED"),
          fillColor = ifelse(
            locations$data_source == "TCEQ", "#0F766E", "#7C3AED"
          ),
          fillOpacity = ifelse(is.finite(locations$value), 0.95, 0.28),
          weight = 2, group = "measurements", label = lapply(label, htmltools::HTML)
        )
    }

    shiny::observe({
      shiny::req(identical(input$dashboard_tabs, "regional"))
      shiny::req(identical(input$regional_tabs, "Spatial surface"))
      surface <- regional_surface()
      frames <- regional_frame_sequence()
      values <- regional_frame_values(frames[[regional_frame_index()]])
      if (isTRUE(input$regional_compare)) {
        tceq_surface <- surface
        tceq_surface$value <- surface$tceq$value
        tceq_surface$supported <- surface$tceq$supported
        aqmesh_surface <- surface
        aqmesh_surface$value <- surface$aqmesh$value
        aqmesh_surface$supported <- surface$aqmesh$supported
        session$sendCustomMessage(
          "pm25-surface-frame",
          regional_surface_payload(tceq_surface, "TCEQ", "regional_tceq_map")
        )
        session$sendCustomMessage(
          "pm25-surface-frame",
          regional_surface_payload(
            aqmesh_surface, "Dallas AQMesh", "regional_aqmesh_map"
          )
        )
        update_regional_markers("regional_tceq_map", values, "TCEQ")
        update_regional_markers(
          "regional_aqmesh_map", values, "Dallas AQMesh"
        )
      } else {
        session$sendCustomMessage(
          "pm25-surface-frame",
          regional_surface_payload(
            surface, input$regional_map_view, "regional_surface_map"
          )
        )
        update_regional_markers("regional_surface_map", values)
      }
    })

    shiny::observeEvent(
      list(input$regional_tabs, input$regional_compare, input$dashboard_tabs),
      {
        session$sendCustomMessage(
          "pm25-map-resize",
          c("regional_surface_map", "regional_tceq_map", "regional_aqmesh_map")
        )
        session$sendCustomMessage("pm25-sidebar-top", list())
      }, ignoreInit = TRUE
    )

    regional_modeled_reach <- shiny::reactive({
      if (identical(input$regional_metric, "hourly") ||
          identical(input$regional_map_view, "Disagreement")) return(NA_real_)
      frame <- as.Date(regional_frame_sequence()[[regional_frame_index()]])
      data <- regional_daily()
      data <- data[data$date == frame, , drop = FALSE]
      mean_values <- data$computed_daily_avg_ug_m3
      max_values <- data$computed_daily_max_ug_m3
      mean_values[!data$event_eligible] <- NA_real_
      max_values[!data$event_eligible] <- NA_real_
      names(mean_values) <- names(max_values) <- data$aqs_site_id
      mean_surface <- pm25_interpolate_surface(
        mean_values, regional_spatial(), input$regional_map_view
      )
      max_surface <- pm25_interpolate_surface(
        max_values, regional_spatial(), input$regional_map_view
      )
      pm25_surface_modeled_reach(mean_surface, max_surface)
    })

    output$regional_surface_summary <- shiny::renderUI({
      frames <- regional_frame_sequence()
      frame <- frames[[regional_frame_index()]]
      surface <- regional_surface()
      label <- if (identical(input$regional_metric, "hourly")) {
        paste0(
          format(frame, "%Y-%m-%d %H:%M %Z", tz = "America/Chicago"),
          " · ", format(frame, "%Y-%m-%d %H:%M UTC", tz = "UTC")
        )
      } else as.character(as.Date(frame))
      validation <- regional_validation()
      metric_name <- if (identical(input$regional_metric, "hourly")) {
        "hourly"
      } else if (identical(input$regional_metric, "daily_max")) {
        "computed_daily_max_ug_m3"
      } else "computed_daily_avg_ug_m3"
      diagnostic <- validation[validation$metric == metric_name, , drop = FALSE]
      validation_text <- if (nrow(diagnostic)) paste(vapply(
        seq_len(nrow(diagnostic)), function(i) {
          if (diagnostic$observations[i] > 0L && is.finite(diagnostic$mae[i])) {
            paste0(
              diagnostic$data_source[i], " MAE ", sprintf("%.1f", diagnostic$mae[i]),
              " µg/m³ (n=", diagnostic$observations[i], ")"
            )
          } else paste0(diagnostic$data_source[i], ": no supported held-out predictions")
        }, character(1)
      ), collapse = " · ") else "Validation unavailable for this frame type"
      modeled <- regional_modeled_reach()
      shiny::div(
        class = "regional-summary",
        shiny::strong(label),
        shiny::span(paste0(
          "Valid sensors: ", surface$valid_tceq, " TCEQ + ",
          surface$valid_aqmesh, " AQMesh"
        )),
        shiny::span(paste0(
          "Supported grid cells: ", format(sum(surface$supported), big.mark = ",")
        )),
        if (is.finite(modeled)) shiny::span(sprintf(
          "Modeled supported area meeting both daily thresholds: %.1f%%", modeled
        )),
        if (identical(input$regional_layer, "network_contribution")) shiny::span(
          "Contribution colors: TCEQ teal · AQMesh purple · both networks amber"
        ),
        shiny::span(class = "validation-note", validation_text)
      )
    })

    output$regional_cache_status <- shiny::renderUI({
      if (!isTRUE(regional_available)) {
        return(shiny::div(
          class = "cache-alert",
          "Regional cache missing. Run the one-command dashboard updater."
        ))
      }
      shiny::div(
        class = "cache-note",
        "Regional data load only when this section is opened."
      )
    })

    output$download_regional_reach <- shiny::downloadHandler(
      filename = function() paste0(
        "dfw_pm25_regional_reach_", regional_dates()[1], "_",
        regional_dates()[2], ".csv"
      ),
      content = function(file) readr::write_csv(regional_window_reach(), file, na = "")
    )
    output$download_regional_overlap <- shiny::downloadHandler(
      filename = function() paste0(
        gsub("[^A-Za-z0-9_-]", "_", regional_anchor()$event_key),
        "_overlaps.csv"
      ),
      content = function(file) readr::write_csv(regional_anchor_links(), file, na = "")
    )
    output$download_regional_grid <- shiny::downloadHandler(
      filename = function() {
        frame <- regional_frame_sequence()[[regional_frame_index()]]
        stamp <- if (inherits(frame, "POSIXt")) {
          format(frame, "%Y%m%dT%H%MZ", tz = "UTC")
        } else as.character(as.Date(frame))
        name <- paste0("dfw_pm25_grid_", input$regional_map_view, "_", stamp, ".csv")
        gsub("[^A-Za-z0-9_.-]", "_", name)
      },
      content = function(file) {
        surface <- regional_surface()
        spatial <- regional_spatial()
        frame <- regional_frame_sequence()[[regional_frame_index()]]
        grid <- tibble::tibble(
          frame_utc = if (inherits(frame, "POSIXt")) {
            format(frame, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
          } else paste0(as.Date(frame), " (daily Central-date aggregate)"),
          metric = input$regional_metric,
          surface_view = input$regional_map_view,
          longitude = spatial$grid$longitude,
          latitude = spatial$grid$latitude,
          inside_region = spatial$grid$inside_region,
          supported = surface$supported,
          interpolated_pm25_ug_m3 = surface$value,
          tceq_pm25_ug_m3 = surface$tceq$value,
          aqmesh_pm25_ug_m3 = surface$aqmesh$value,
          absolute_network_disagreement = surface$disagreement,
          nearby_sensor_count = surface$nearby_count,
          nearest_sensor_km = surface$nearest_m / 1000,
          network_contribution = surface$contribution
        ) |>
          dplyr::filter(.data$inside_region)
        readr::write_csv(grid, file, na = "")
      }
    )

    output$download_hourly <- shiny::downloadHandler(
      filename = function() paste0(
        selected_site(), "_pm25_", selected_dates()[1], "_", selected_dates()[2], ".csv"
      ),
      content = function(file) {
        readr::write_csv(filtered_hourly(), file, na = "")
      }
    )
    output$download_events <- shiny::downloadHandler(
      filename = function() paste0(selected_site(), "_pm25_events.csv"),
      content = function(file) {
        readr::write_csv(site_bundle()$events, file, na = "")
      }
    )
  }
}
