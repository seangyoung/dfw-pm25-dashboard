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
  parameter_code = "88101",
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

# Parameter 88101 is the primary site-hour measurement. Parameter 88502 is used
# only when the primary parameter has no finite value for that site-hour.
parameter_fallback_test <- raw_composite_test[0, ] |>
  dplyr::bind_rows(tibble::tibble(
    aqs_site_id = "48_439_1053",
    site_name = "California Parkway North",
    date = as.Date("2024-01-02"),
    hour = c(0L, 0L, 1L),
    hour_lstd = c("00:00", "00:00", "01:00"),
    pm25_raw = c("10", "100", "20"),
    pm25_ug_m3 = c(10, 100, 20),
    value_flag = NA_character_,
    poc = c(1L, 3L, 3L),
    source_file = c("primary.csv", "acceptable.csv", "acceptable.csv"),
    report_block = 1L,
    parameter_code = c("88101", "88502", "88502"),
    regulatory_qa = c(TRUE, NA, NA)
  ))
parameter_composite <- tceq_build_hourly_composite(parameter_fallback_test)
stopifnot(
  parameter_composite$composite_pm25_ug_m3[parameter_composite$hour == 0L] == 10,
  parameter_composite$selected_parameter_code[parameter_composite$hour == 0L] == "88101",
  !parameter_composite$parameter_fallback[parameter_composite$hour == 0L],
  parameter_composite$composite_pm25_ug_m3[parameter_composite$hour == 1L] == 20,
  parameter_composite$selected_parameter_code[parameter_composite$hour == 1L] == "88502",
  parameter_composite$parameter_fallback[parameter_composite$hour == 1L],
  parameter_composite$qa_status_unavailable[parameter_composite$hour == 1L],
  !parameter_composite$qa_fallback[parameter_composite$hour == 1L]
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
    parameter_fallback = FALSE,
    qa_status_unavailable = FALSE,
    total_records = 1L,
    available_records = ifelse(is.finite(values), 1L, 0L),
    has_regulatory_value = is.finite(values),
    value_flag_summary = ifelse(is.finite(values), "", "LST"),
    composite_min_ug_m3 = values,
    composite_max_ug_m3 = values,
    composite_spread_ug_m3 = 0,
    contributor_pocs = "1",
    contributor_sources = "test.csv::1::1",
    selected_parameter_code = ifelse(is.finite(values), "88101", NA_character_),
    composite_selection = ifelse(is.finite(values), "88101_qa_preferred", "missing")
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
    qa_fallback_hours = 0L, parameter_fallback_hours = 0L,
    contributing_records = valid,
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

# Cross-sensor reach is network-balanced, so the denser AQMesh network does not
# automatically dominate the primary regional percentage.
regional_reach_fixture <- dplyr::bind_rows(
  tibble::tibble(
    date = as.Date("2024-05-21"), data_source = "TCEQ",
    event_eligible = TRUE, threshold_core = c(TRUE, TRUE, TRUE),
    event_active = TRUE
  ),
  tibble::tibble(
    date = as.Date("2024-05-21"), data_source = "Dallas AQMesh",
    event_eligible = TRUE,
    threshold_core = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    event_active = c(TRUE, rep(FALSE, 5))
  )
)
regional_reach_test <- pm25_daily_reach(regional_reach_fixture)
stopifnot(
  nrow(regional_reach_test) == 1L,
  abs(regional_reach_test$balanced_participation_percent - (100 + 100 / 6) / 2) < 1e-8,
  abs(regional_reach_test$raw_affected_percent - 100 * 4 / 9) < 1e-8,
  regional_reach_test$coverage_label == "Both networks"
)

# Direct overlap links do not chain A to C merely because both overlap B.
regional_station_fixture <- tibble::tibble(
  aqs_site_id = c("a", "b", "c"), site_name = c("A", "B", "C"),
  data_source = c("TCEQ", "Dallas AQMesh", "TCEQ"),
  latitude = c(32.7, 32.8, 32.9), longitude = c(-97.1, -97.0, -96.9)
)
regional_event_fixture <- tibble::tibble(
  aqs_site_id = c("a", "b", "c"), event_id = c("e1", "e2", "e3"),
  event_type = "multi_day", data_source = regional_station_fixture$data_source,
  start_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04")),
  end_date = as.Date(c("2024-01-03", "2024-01-05", "2024-01-06")),
  core_dates = c(
    "2024-01-01|2024-01-02", "2024-01-02|2024-01-04",
    "2024-01-04|2024-01-05"
  ),
  trough_dates = c("2024-01-03", "2024-01-03", "2024-01-06"),
  peak_hourly_ug_m3 = c(40, 42, 44), peak_daily_max_ug_m3 = c(35, 36, 37),
  max_daily_avg_ug_m3 = c(22, 23, 24), coverage_warning = FALSE,
  qa_fallback = FALSE, parameter_fallback = FALSE
)
regional_link_test <- pm25_build_regional_event_links(
  regional_event_fixture, regional_station_fixture
)
links_from_a <- regional_link_test[
  regional_link_test$anchor_event_key == "a::e1", , drop = FALSE
]
stopifnot(
  nrow(links_from_a) == 1L,
  links_from_a$other_event_key == "b::e2",
  links_from_a$relationship == "shared_core",
  links_from_a$shared_core_days == 1L
)

# UTC rows remain distinct through a repeated local fall-back hour.
regional_hourly_fixture <- tibble::tibble(
  aqs_site_id = rep("a", 2),
  datetime_utc = as.POSIXct(
    c("2024-11-03 06:00:00", "2024-11-03 07:00:00"), tz = "UTC"
  ),
  composite_pm25_ug_m3 = c(10, 12)
)
regional_hourly_test <- pm25_build_regional_hourly(
  regional_hourly_fixture, regional_station_fixture[1, , drop = FALSE]
)
stopifnot(
  nrow(regional_hourly_test$index$years) == 1L,
  length(regional_hourly_test$shards[["2024"]]$timestamp_utc) == 2L,
  identical(
    as.numeric(regional_hourly_test$shards[["2024"]]$values[, 1]), c(10, 12)
  )
)

# IDW requires supported geometry and blends network surfaces by local support.
regional_spatial_fixture <- list(
  grid = tibble::tibble(
    x = 0, y = 0.5, longitude = -97, latitude = 32.75,
    inside_region = TRUE, row = 1L, column = 1L
  ),
  x_values = 0, y_values = 0.5,
  station_ids = c("t1", "t2", "t3", "q1", "q2", "q3"),
  station_network = c(rep("TCEQ", 3), rep("Dallas AQMesh", 3)),
  station_x = c(-1000, 1000, 0, -1200, 1200, 0),
  station_y = c(0, 0, 2000, -200, -200, 2200),
  distances_m = matrix(
    sqrt((c(-1000, 1000, 0, -1200, 1200, 0) - 0)^2 +
      (c(0, 0, 2000, -200, -200, 2200) - 500)^2),
    nrow = 1
  )
)
regional_surface_test <- pm25_interpolate_surface(
  c(t1 = 10, t2 = 20, t3 = 30, q1 = 30, q2 = 40, q3 = 50),
  regional_spatial_fixture
)
stopifnot(
  regional_surface_test$supported,
  is.finite(regional_surface_test$combined),
  regional_surface_test$combined > regional_surface_test$tceq$value,
  regional_surface_test$combined < regional_surface_test$aqmesh$value,
  regional_surface_test$contribution == "Both networks"
)
insufficient_surface <- pm25_interpolate_surface(
  c(t1 = 10, t2 = 20), regional_spatial_fixture, view = "TCEQ"
)
stopifnot(!insufficient_surface$supported, is.na(insufficient_surface$value))

validation_daily_fixture <- tibble::tibble(
  aqs_site_id = regional_spatial_fixture$station_ids,
  date = as.Date("2024-07-01"),
  computed_daily_avg_ug_m3 = c(10, 20, 30, 30, 40, 50),
  computed_daily_max_ug_m3 = c(15, 25, 35, 35, 45, 55)
)
validation_station_fixture <- tibble::tibble(
  aqs_site_id = regional_spatial_fixture$station_ids,
  data_source = regional_spatial_fixture$station_network
)
validation_summary_test <- pm25_surface_validation(
  validation_daily_fixture, validation_station_fixture,
  regional_spatial_fixture, daily_frame_limit = 1L
)
stopifnot(
  nrow(validation_summary_test) == 4L,
  all(c("mae", "rmse", "bias", "observations") %in%
    names(validation_summary_test)),
  all(validation_summary_test$observations >= 0L)
)

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

app_server_source <- readLines(
  file.path(find_repo_root(), "R", "tceq_pm25_dashboard_app.R"),
  warn = FALSE
)
stopifnot(
  any(grepl("World_Light_Gray_Base", app_server_source, fixed = TRUE)),
  any(grepl("World_Light_Gray_Reference", app_server_source, fixed = TRUE)),
  !any(grepl("CartoDB.Positron", app_server_source, fixed = TRUE))
)
public_build_script <- file.path(find_repo_root(), "scripts", "build_shinylive.R")
if (file.exists(public_build_script)) {
  build_source <- readLines(public_build_script, warn = FALSE)
  stopifnot(
    file.exists(file.path(find_repo_root(), "deployment", "startup.css")),
    file.exists(file.path(find_repo_root(), "deployment", "startup.js")),
    any(grepl('fetch("./app.json?v=%s")', build_source, fixed = TRUE)),
    any(grepl("versioned_assets", build_source, fixed = TRUE))
  )
}

# Server-level checks use compact in-memory site bundles; no external cache or
# live TCEQ request is required by the project test suite.
source(file.path(find_repo_root(), "R", "tceq_pm25_dashboard_app.R"))
window_bounds <- as.Date(c("2024-01-01", "2024-06-30"))
latest_30 <- tceq_dashboard_window_dates(
  window_bounds[1], window_bounds[2], 30L, window_bounds[2]
)
stopifnot(
  identical(latest_30, as.Date(c("2024-06-01", "2024-06-30"))),
  identical(
    tceq_dashboard_shift_dates(latest_30, window_bounds[1], window_bounds[2], -1L),
    as.Date(c("2024-05-02", "2024-05-31"))
  ),
  identical(
    tceq_dashboard_shift_dates(
      as.Date(c("2024-01-01", "2024-01-30")),
      window_bounds[1], window_bounds[2], -1L
    ),
    as.Date(c("2024-01-01", "2024-01-30"))
  ),
  identical(
    tceq_dashboard_window_dates(
      as.Date("2024-01-01"), as.Date("2024-01-10"), 30L,
      as.Date("2024-01-10")
    ),
    as.Date(c("2024-01-01", "2024-01-10"))
  )
)
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
      parameter_code = "88101", parameter_name = "PM-2.5 (Local Conditions)",
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
  latitude = c(32.7, 32.8), longitude = c(-97.3, -97.2),
  data_source = c("TCEQ", "Dallas AQMesh"),
  network_label = c("TCEQ", "City of Dallas AQMesh"),
  source_station_id = c("site_a", "Dallas location 1"),
  time_label = c("LST", "local time")
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
    session$setInputs(data_source = "Dallas AQMesh")
    session$flushReact()
    stopifnot(selected_site() == "site_b", nrow(available_stations()) == 1L)
    session$setInputs(data_source = "Combined")
    session$flushReact()
    stopifnot(nrow(available_stations()) == 2L)
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

# Regional server logic stays lazy and preserves direct anchor-event comparison.
regional_anchor_event <- server_bundles$site_a$events[1, , drop = FALSE] |>
  dplyr::mutate(
    event_id = "MULTI_site_a_20240101",
    event_type = "multi_day",
    start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-02"),
    span_days = 2L, duration_hours = 48,
    core_dates = "2024-01-01|2024-01-02", trough_dates = ""
  )
regional_server_events <- dplyr::bind_rows(server_events, regional_anchor_event)
regional_server_daily <- dplyr::bind_rows(
  tibble::tibble(
    aqs_site_id = "site_a", date = as.Date(c("2024-01-01", "2024-01-02")),
    data_source = "TCEQ", valid_hours = 24L,
    computed_daily_max_ug_m3 = c(42, 38),
    computed_daily_avg_ug_m3 = c(24, 22), qa_fallback_hours = 0L,
    parameter_fallback_hours = 0L, event_eligible = TRUE,
    threshold_core = TRUE, event_active = TRUE, event_core = TRUE,
    event_trough = FALSE
  ),
  tibble::tibble(
    aqs_site_id = "site_b", date = as.Date(c("2024-01-01", "2024-01-02")),
    data_source = "Dallas AQMesh", valid_hours = 24L,
    computed_daily_max_ug_m3 = c(20, 18),
    computed_daily_avg_ug_m3 = c(12, 11), qa_fallback_hours = 0L,
    parameter_fallback_hours = 0L, event_eligible = TRUE,
    threshold_core = FALSE, event_active = FALSE, event_core = FALSE,
    event_trough = FALSE
  )
)
regional_server_reach <- pm25_daily_reach(regional_server_daily)
regional_server_links <- tibble::tibble(
  anchor_event_key = "site_a::MULTI_site_a_20240101",
  relationship = "shared_core", other_data_source = "Dallas AQMesh",
  other_site_name = "B site", other_site_id = "site_b", other_event_id = "b-event",
  other_start_date = as.Date("2024-01-02"), other_end_date = as.Date("2024-01-03"),
  shared_core_dates = "2024-01-02", shared_core_days = 1L,
  span_overlap_days = 1L, start_lag_days = 1L, distance_km = 12,
  peak_hourly_ug_m3 = 40, peak_daily_max_ug_m3 = 35,
  max_daily_avg_ug_m3 = 22, coverage_warning = "", qa_fallback = FALSE,
  parameter_fallback = FALSE
)
regional_loader <- function(name) switch(
  name,
  daily = regional_server_daily,
  reach = regional_server_reach,
  links = regional_server_links,
  spatial = regional_spatial_fixture,
  NULL
)
shiny::testServer(
  tceq_dashboard_server(
    server_stations, regional_server_events,
    load_bundle = function(site_id) server_bundles[[site_id]],
    load_regional = regional_loader, regional_available = TRUE
  ),
  {
    session$setInputs(
      dashboard_tabs = "regional",
      regional_tabs = "Event alignment",
      regional_anchor = "site_a::MULTI_site_a_20240101",
      regional_dates = as.Date(c("2024-01-01", "2024-01-02")),
      regional_event_network = "All", regional_event_min_days = "2",
      regional_event_min_sites = "2",
      regional_metric = "daily_avg", regional_map_view = "Combined",
      regional_layer = "concentration"
    )
    session$flushReact()
    stopifnot(
      regional_anchor()$event_id == "MULTI_site_a_20240101",
      nrow(regional_window_daily()) == 4L,
      nrow(regional_window_reach()) == 2L,
      nrow(regional_anchor_links()) == 1L,
      regional_anchor_links()$relationship == "shared_core",
      nrow(regional_filtered_events()) == 1L,
      regional_filtered_events()$aligned_sites == 2L,
      regional_frame_index() == 1L
    )
    session$setInputs(regional_tabs = "Spatial surface", regional_generate = 1L)
    session$flushReact()
    stopifnot(
      identical(rv$regional_animation$key, regional_surface_key()),
      length(rv$regional_animation$frames) == 2L
    )
  }
)
