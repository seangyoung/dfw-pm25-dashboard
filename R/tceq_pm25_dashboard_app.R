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
  events |>
    dplyr::transmute(
      Event = .data$event_id,
      Type = dplyr::recode(
        .data$event_type,
        short_spike = "Short-term spike", multi_day = "Multi-day particulate event"
      ),
      Start = ifelse(
        .data$event_type == "short_spike",
        format(.data$start_lstd, "%Y-%m-%d %H:00 LST", tz = "UTC"),
        as.character(.data$start_date)
      ),
      End = ifelse(
        .data$event_type == "short_spike",
        format(.data$end_lstd, "%Y-%m-%d %H:00 LST", tz = "UTC"),
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
        .data$qa_fallback ~ "Includes QA-fallback values",
        .data$coverage_warning ~ "Some event days have fewer than 24 valid hours",
        TRUE ~ ""
      )
    )
}

tceq_dashboard_raw_choices <- function(raw) {
  if (!nrow(raw)) return(character())
  choices <- raw |>
    dplyr::distinct(
      .data$raw_series_key, .data$source_file, .data$report_block, .data$poc,
      .data$regulatory_qa
    ) |>
    dplyr::arrange(.data$source_file, .data$poc, .data$report_block) |>
    dplyr::mutate(
      qa_label = dplyr::case_when(
        .data$regulatory_qa %in% TRUE ~ "regulatory QA",
        .data$regulatory_qa %in% FALSE ~ "non-regulatory QA",
        TRUE ~ "QA status unavailable"
      ),
      label = sprintf(
        "%s · POC %s · report table %s · %s",
        sub("^cams_[0-9]+_|_pm25\\.csv$", "", .data$source_file),
        .data$poc, .data$report_block, .data$qa_label
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
    regulatory <- sum(
      is.finite(x$composite_pm25_ug_m3) & !x$qa_fallback, na.rm = TRUE
    )
    fallback <- sum(
      is.finite(x$composite_pm25_ug_m3) & x$qa_fallback, na.rm = TRUE
    )
    tibble::tibble(
      year = year, expected_hours = expected, regulatory_hours = regulatory,
      fallback_hours = fallback,
      missing_hours = max(0L, expected - regulatory - fallback),
      valid_percent = 100 * (regulatory + fallback) / expected
    )
  })
}

tceq_dashboard_server <- function(
    station_index, event_index, load_bundle, cache_manifest = list(),
    cache_stale = FALSE) {
  force(station_index)
  force(event_index)
  force(load_bundle)
  force(cache_manifest)
  force(cache_stale)

  function(input, output, session) {
    rv <- shiny::reactiveValues(
      site = tceq_dashboard_default_site(station_index),
      window_days = 30L,
      pending_date_range = NULL
    )

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

    shiny::observeEvent(input$site, {
      if (!is.null(input$site) && input$site %in% station_index$aqs_site_id) {
        if (!identical(input$site, rv$site)) remember_current_window()
        rv$site <- input$site
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$site_map_marker_click, {
      clicked <- input$site_map_marker_click$id
      if (!is.null(clicked) && clicked %in% station_index$aqs_site_id) {
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
          shiny::div("Run: Rscript --vanilla scripts/08c_prepare_tceq_dfw_pm25_dashboard.R")
        ))
      }
      generated <- cache_manifest$generated_at_utc
      if (is.null(generated)) generated <- "unknown"
      shiny::div(class = "cache-note", paste("Cache prepared", generated))
    })

    output$site_map <- leaflet::renderLeaflet({
      selected <- selected_site()
      locations <- station_index
      marker_colors <- ifelse(
        locations$aqs_site_id == selected, "#D97706",
        ifelse(locations$active, "#16697A", "#6B7280")
      )
      marker_radius <- ifelse(locations$aqs_site_id == selected, 10, 7)
      popup <- sprintf(
        "<strong>%s</strong><br>AQS %s<br>CAMS %s<br>%s<br>%s to %s",
        htmltools::htmlEscape(locations$site_name),
        htmltools::htmlEscape(locations$aqs_site_id),
        locations$cams_id,
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
          fillColor = marker_colors, fillOpacity = 0.85,
          popup = popup, label = ~site_name
        ) |>
        leaflet::addLegend(
          position = "bottomright", colors = c("#16697A", "#6B7280"),
          labels = c("Active", "Historical"), opacity = 0.9,
          title = "Site status"
        ) |>
        leaflet::setView(focus$longitude[1], focus$latitude[1], zoom = 9)
    })

    output$site_details <- shiny::renderUI({
      x <- site_record()
      shiny::tagList(
        shiny::div(class = "selected-site-name", x$site_name),
        shiny::div(class = "site-meta", paste("AQS", x$aqs_site_id, "· CAMS", x$cams_id)),
        shiny::div(class = "site-meta", paste0(x$city, " · ", x$county, " County")),
        shiny::div(class = "site-meta", paste(x$first_date, "to", x$last_date, "· LST"))
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
            "%s · %s LST<br>PM2.5: %s<br>Selection: %s<br>Contributors: %s%s",
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
        ggplot2::labs(x = "Hour (LST)", y = "Date") +
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
              format(event$end_lstd, "%Y-%m-%d %H:00 LST", tz = "UTC"),
              "·", event$duration_hours,
              if (event$duration_hours == 1L) "hour" else "hours"
            )
          } else {
            paste(event$start_date, "to", event$end_date, "·", event$span_days, "days")
          }
        ),
        if (event$qa_fallback) shiny::span(class = "qa-warning", "Includes QA-fallback values")
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
          "%s LST<br>Composite: %.1f µg/m³<br>Contributors: %d<br>%s",
          format(datetime_lstd, "%Y-%m-%d %H:00", tz = "UTC"),
          composite_pm25_ug_m3, contributing_records,
          ifelse(qa_fallback, "QA fallback", "Regulatory-QA preferred")
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
              "%s LST<br>Raw: %s<br>POC %d · table %d<br>Flag: %s",
              format(datetime_lstd, "%Y-%m-%d %H:00", tz = "UTC"),
              ifelse(is.finite(pm25_ug_m3), sprintf("%.1f µg/m³", pm25_ug_m3), pm25_raw),
              poc, report_block, ifelse(is.na(value_flag), "none", value_flag)
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
        p, xaxis = list(title = "Date and hour (LST)"),
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
        text = ~sprintf("%02d:00 LST<br>Median: %.1f µg/m³<br>IQR: %.1f–%.1f", hour, median, q25, q75),
        hoverinfo = "text"
      )
      p <- plotly::layout(
        p, xaxis = list(title = "Hour (LST)", dtick = 2),
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
          c("regulatory_hours", "fallback_hours", "missing_hours"),
          names_to = "category", values_to = "hours"
        ) |>
        dplyr::mutate(
          percent = 100 * .data$hours / .data$expected_hours,
          category = factor(
            .data$category,
            levels = c("regulatory_hours", "fallback_hours", "missing_hours"),
            labels = c("Regulatory-QA preferred", "QA fallback", "Missing / flagged")
          )
        )
      p <- plotly::plot_ly(
        x, x = ~year, y = ~percent, color = ~category, type = "bar",
        colors = c("#16697A", "#D97706", "#9CA3AF"),
        text = ~sprintf("%d · %s<br>%.1f%% (%s hours)", year, category, percent, format(hours, big.mark = ",")),
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
      shiny::div(
        class = "quality-summary",
        shiny::span(shiny::strong(format(sum(finite), big.mark = ",")), " valid composite hours"),
        shiny::span(shiny::strong(format(fallback, big.mark = ",")), " QA-fallback hours"),
        shiny::span(shiny::strong(format(negative, big.mark = ",")), " negative raw cells"),
        shiny::span(shiny::strong(format(character_flags, big.mark = ",")), " character-coded raw cells")
      )
    })

    output$flag_table <- DT::renderDT({
      flags <- site_bundle()$raw_hourly |>
        dplyr::filter(!is.na(.data$value_flag), nzchar(.data$value_flag)) |>
        dplyr::count(.data$value_flag, name = "Raw cells", sort = TRUE) |>
        dplyr::rename(`TCEQ flag` = "value_flag")
      if (!nrow(flags)) flags <- data.frame(`TCEQ flag` = character(), `Raw cells` = integer())
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
