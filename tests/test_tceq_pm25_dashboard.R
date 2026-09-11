# Deterministic unit checks for the TCEQ DFW PM2.5 dashboard data layer.

site_fixture <- paste(
  "<html><body><h1>Test Site C999 Site Photographs</h1><ul>",
  "<li>EPA site number: 48-439-0999</li><li>State: Texas</li>",
  "<li>County: Tarrant</li><li>City: Fort Worth</li>",
  "<li>Address: 1 Test Street</li>",
  "<li>Latitude: 32 degrees North (+32.750000 degrees)</li>",
  "<li>Longitude: 97 degrees West (-97.250000 degrees)</li>",
  "<li>Elevation: 100 m</li>",
  "<li>Real-time monitoring since: Monday, January 1, 2001</li>",
  "<li>Current status: Active</li></ul></body></html>"
)
parsed_site <- tceq_parse_site_metadata(site_fixture, 999L, "48_439_0999")
stopifnot(
  nrow(parsed_site) == 1L,
  parsed_site$latitude == 32.75,
  parsed_site$longitude == -97.25,
  isTRUE(parsed_site$active)
)

raw_composite_test <- tibble::tibble(
  aqs_site_id = "48_439_1053",
  site_name = "California Parkway North",
  date = as.Date("2024-01-01"),
  hour = c(0L, 0L, 0L, 1L, 1L, 2L),
  hour_lstd = sprintf("%02d:00", c(0L, 0L, 0L, 1L, 1L, 2L)),
  pm25_raw = c("10", "14", "100", "20", "24", "NEG"),
  pm25_ug_m3 = c(10, 14, 100, 20, 24, NA),
  value_flag = c(NA, NA, NA, NA, NA, "NEG"),
  poc = c(2L, 2L, 3L, 2L, 3L, 2L),
  source_file = "test.csv",
  report_block = c(1L, 2L, 3L, 1L, 2L, 1L),
  regulatory_qa = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)
composite_test <- tceq_build_hourly_composite(raw_composite_test)
stopifnot(
  composite_test$composite_pm25_ug_m3[composite_test$hour == 0L] == 12,
  composite_test$contributing_records[composite_test$hour == 0L] == 2L,
  !composite_test$qa_fallback[composite_test$hour == 0L],
  composite_test$composite_pm25_ug_m3[composite_test$hour == 1L] == 22,
  composite_test$qa_fallback[composite_test$hour == 1L],
  is.na(composite_test$composite_pm25_ug_m3[composite_test$hour == 2L]),
  composite_test$value_flag_summary[composite_test$hour == 2L] == "NEG"
)

make_hourly_test <- function(values, start = "2024-01-01 00:00:00") {
  datetime <- as.POSIXct(start, tz = "UTC") + seq_along(values) * 3600 - 3600
  tibble::tibble(
    aqs_site_id = "test_site",
    site_name = "Test site",
    date = as.Date(datetime, tz = "UTC"),
    hour = as.integer(format(datetime, "%H", tz = "UTC")),
    hour_lstd = format(datetime, "%H:00", tz = "UTC"),
    datetime_lstd = datetime,
    composite_pm25_ug_m3 = values,
    contributing_records = ifelse(is.finite(values), 1L, 0L),
    qa_fallback = FALSE,
    total_records = 1L,
    available_records = ifelse(is.finite(values), 1L, 0L),
    has_regulatory_value = is.finite(values),
    value_flag_summary = ifelse(is.finite(values), "", "LST"),
    composite_min_ug_m3 = values,
    composite_max_ug_m3 = values,
    composite_spread_ug_m3 = 0,
    contributor_pocs = "1",
    contributor_sources = "test.csv::1::1",
    composite_selection = ifelse(is.finite(values), "regulatory_qa", "missing")
  )
}

spike_hourly <- make_hourly_test(c(rep(36, 5), 35, rep(36, 6), 10, 42))
spikes <- tceq_detect_short_spikes(spike_hourly)
stopifnot(
  nrow(spikes) == 2L,
  identical(spikes$duration_hours, c(5L, 1L)),
  all(spikes$peak_hourly_ug_m3 == c(36, 42))
)

missing_break <- tceq_detect_short_spikes(make_hourly_test(c(40, 40, NA, 40)))
stopifnot(identical(missing_break$duration_hours, c(2L, 1L)))
midnight_spike <- tceq_detect_short_spikes(
  make_hourly_test(c(40, 41, 42), "2024-01-01 22:00:00")
)
stopifnot(nrow(midnight_spike) == 1L, midnight_spike$span_days == 2L)

make_daily_test <- function(maximum, average, valid = rep(24L, length(maximum)),
                            start = as.Date("2024-01-01")) {
  dates <- start + seq_along(maximum) - 1L
  daily <- tibble::tibble(
    aqs_site_id = "test_site", date = dates, valid_hours = valid,
    represented_hours = 24L, computed_daily_max_ug_m3 = maximum,
    computed_daily_avg_ug_m3 = average, computed_daily_std_ug_m3 = 1,
    qa_fallback_hours = 0L, contributing_records = valid,
    event_eligible = valid >= 18L
  )
  hourly <- purrr::map2_dfr(dates, average, function(date, value) {
    make_hourly_test(rep(value, 24), paste(date, "00:00:00"))
  })
  list(daily = daily, hourly = hourly)
}

bridged <- make_daily_test(c(30, 18, 31), c(21, 15, 22))
bridged_events <- tceq_detect_multiday_events(bridged$daily, bridged$hourly)
stopifnot(
  nrow(bridged_events) == 1L,
  bridged_events$span_days == 3L,
  bridged_events$trough_dates == "2024-01-02"
)

two_troughs <- make_daily_test(c(30, 18, 19, 31), c(21, 15, 16, 22))
stopifnot(nrow(tceq_detect_multiday_events(two_troughs$daily, two_troughs$hourly)) == 0L)
incomplete_trough <- make_daily_test(c(30, 18, 31), c(21, 15, 22), c(24L, 17L, 24L))
stopifnot(nrow(tceq_detect_multiday_events(
  incomplete_trough$daily, incomplete_trough$hourly
)) == 0L)
threshold_equal <- make_daily_test(c(25, 26), c(21, 20))
stopifnot(nrow(tceq_detect_multiday_events(
  threshold_equal$daily, threshold_equal$hourly
)) == 0L)
exact_coverage <- make_daily_test(c(30, 31), c(21, 22), c(18L, 18L))
stopifnot(nrow(tceq_detect_multiday_events(
  exact_coverage$daily, exact_coverage$hourly
)) == 1L)

loader_reads <- new.env(parent = emptyenv())
loader_reads$n <- integer()
bounded_loader <- tceq_dashboard_bundle_loader(
  "unused", max_entries = 2L,
  reader = function(cache_dir, site_id) {
    loader_reads$n[site_id] <- dplyr::coalesce(loader_reads$n[site_id], 0L) + 1L
    list(site_id = site_id)
  }
)
stopifnot(
  bounded_loader("site_a")$site_id == "site_a",
  bounded_loader("site_b")$site_id == "site_b",
  bounded_loader("site_c")$site_id == "site_c",
  bounded_loader("site_b")$site_id == "site_b",
  bounded_loader("site_a")$site_id == "site_a",
  loader_reads$n["site_a"] == 2L,
  loader_reads$n["site_b"] == 1L,
  loader_reads$n["site_c"] == 1L
)

heatmap_test <- tceq_complete_heatmap(
  make_hourly_test(10),
  as.Date("2024-01-01"), as.Date("2024-01-03")
)
stopifnot(nrow(heatmap_test) == 72L, length(unique(heatmap_test$hour)) == 24L)
too_long <- try(tceq_validate_heatmap_range(
  as.Date("2024-01-01"), as.Date("2024-04-01")
), silent = TRUE)
stopifnot(inherits(too_long, "try-error"))

# Server-level checks use compact in-memory site bundles; no external cache or
# live TCEQ request is required by the project test suite.
source(file.path(find_repo_root(), "R", "tceq_pm25_dashboard_app.R"))
server_hourly_a <- make_hourly_test(c(40, 41, rep(10, 46)))
server_hourly_b <- make_hourly_test(rep(12, 48))
make_server_bundle <- function(site_id, hourly) {
  hourly$aqs_site_id <- site_id
  daily <- hourly |>
    dplyr::group_by(.data$aqs_site_id, .data$date) |>
    dplyr::summarise(
      valid_hours = sum(is.finite(.data$composite_pm25_ug_m3)),
      represented_hours = dplyr::n(),
      computed_daily_max_ug_m3 = max(.data$composite_pm25_ug_m3),
      computed_daily_avg_ug_m3 = mean(.data$composite_pm25_ug_m3),
      computed_daily_std_ug_m3 = stats::sd(.data$composite_pm25_ug_m3),
      qa_fallback_hours = 0L, contributing_records = dplyr::n(),
      event_eligible = TRUE, .groups = "drop"
    )
  raw <- hourly |>
    dplyr::transmute(
      aqs_site_id = .data$aqs_site_id, date = .data$date, hour = .data$hour,
      datetime_lstd = .data$datetime_lstd, pm25_ug_m3 = .data$composite_pm25_ug_m3,
      pm25_raw = as.character(.data$composite_pm25_ug_m3), value_flag = NA_character_,
      source_file = "cams_0001_2024-01_pm25.csv", report_block = 1L, poc = 1L,
      regulatory_qa = TRUE,
      raw_series_key = tceq_raw_series_key(.data$source_file, .data$report_block, .data$poc)
    )
  list(
    hourly = hourly, daily = daily, raw_hourly = raw,
    events = tceq_build_event_index(hourly, daily)
  )
}
server_bundles <- list(
  site_a = make_server_bundle("site_a", server_hourly_a),
  site_b = make_server_bundle("site_b", server_hourly_b)
)
server_stations <- tibble::tibble(
  aqs_site_id = c("site_a", "site_b"), site_name = c("A site", "B site"),
  active = c(TRUE, TRUE), last_date = as.Date(c("2024-01-02", "2024-01-02")),
  first_date = as.Date(c("2024-01-01", "2024-01-01")),
  cams_id = c(1L, 2L), city = "Test", county = "Test",
  latitude = c(32.7, 32.8), longitude = c(-97.3, -97.2)
)
server_events <- dplyr::bind_rows(
  server_bundles$site_a$events, server_bundles$site_b$events
)
shiny::testServer(
  tceq_dashboard_server(
    server_stations, server_events,
    load_bundle = function(site_id) server_bundles[[site_id]]
  ),
  {
    stopifnot(selected_site() == "site_a")
    session$setInputs(site = "site_b")
    session$flushReact()
    stopifnot(selected_site() == "site_b", nrow(filtered_hourly()) == 48L)
    session$setInputs(site_map_marker_click = list(id = "site_a"))
    session$flushReact()
    stopifnot(selected_site() == "site_a", nrow(site_events()) == 1L)
    session$setInputs(event_type = "multi_day")
    session$flushReact()
    stopifnot(nrow(site_events()) == 0L)
    session$setInputs(event_type = "short_spike", date_range = as.Date(c(
      "2024-01-01", "2024-01-01"
    )))
    session$flushReact()
    stopifnot(nrow(filtered_hourly()) == 24L, !is.null(selected_event()))
    raw_key <- server_bundles$site_a$raw_hourly$raw_series_key[1]
    session$setInputs(raw_series = raw_key)
    session$flushReact()
    stopifnot(identical(input$raw_series, raw_key))
    session$flushReact()
    stopifnot(identical(input$raw_series, raw_key))
  }
)
