# Deterministic checks for Dallas AQMesh revision, QA, station, and DST logic.

epoch_ms <- function(x) as.numeric(as.POSIXct(x, tz = "UTC")) * 1000

revision_fixture <- tibble::tibble(
  OBJECTID = c(1, 2, 3, 4),
  location_name = "Fretz Park",
  pod_serial_number = 2451148L,
  project_time_interval_start = c(
    epoch_ms("2024-11-03 06:00:00"), epoch_ms("2024-11-03 06:00:00"),
    epoch_ms("2024-11-03 07:00:00"), epoch_ms("2024-11-03 08:00:00")
  ),
  project_time_interval_end = c(
    epoch_ms("2024-11-03 07:00:00"), epoch_ms("2024-11-03 07:00:00"),
    epoch_ms("2024-11-03 08:00:00"), epoch_ms("2024-11-03 09:00:00")
  ),
  append_date = c(
    epoch_ms("2024-11-04 00:00:00"), epoch_ms("2024-11-05 00:00:00"),
    epoch_ms("2024-11-05 00:00:00"), epoch_ms("2024-11-05 00:00:00")
  ),
  pm2_5_prescale = c(39, 40, 42, -999),
  pm2_5_scaled = c(39, 40, 42, -999),
  pm10_scaled = c(59, 60, 64, -999),
  no2_scaled = c(9, 10, 12, -999),
  no2_units = "ppb",
  no2_state = "Reading",
  o3_scaled = c(29, 30, 34, 1500),
  o3_units = "ppb",
  so2_scaled = c(4, 5, 6, -999),
  so2_units = "ppb",
  co_scaled = c(0.3, 0.4, 500, -999),
  co_units = c("ppm", "ppm", "ppb", "ppb"),
  tvoc_scaled = c(10, 20, 30, -999),
  tvoc_units = "ppb",
  humidity = c(49, 50, 54, 55),
  temperature_f = c(67, 68, 71.6, 70),
  reading_status = c("OK", "OK", "NA", "OK"),
  particleprotocol_version = "test",
  pod_latitude = 32.9,
  pod_longitude = -96.8
)

latest_fixture <- aqmesh_latest_revisions(revision_fixture)
stopifnot(
  nrow(latest_fixture) == 3L,
  latest_fixture$pm2_5_scaled[1] == 40,
  latest_fixture$revision_count[1] == 2L
)

clean_fixture <- aqmesh_clean_latest_records(latest_fixture)
stopifnot(
  all(clean_fixture$aqs_site_id == "DAL_AQMESH_FRETZ_PARK"),
  identical(clean_fixture$hour, c(1L, 1L, 2L)),
  is.na(clean_fixture$pm25_ug_m3[3]),
  clean_fixture$value_flag[3] == "SENTINEL",
  clean_fixture$pm10_ug_m3[1] == 60,
  clean_fixture$no2_ppb[1] == 10,
  clean_fixture$o3_ppb[1] == 30,
  clean_fixture$so2_ppb[1] == 5,
  clean_fixture$co_ppm[1] == 0.4,
  clean_fixture$co_ppm[2] == 0.5,
  clean_fixture$co_flag[3] == "SENTINEL",
  clean_fixture$tvoc_ppb[1] == 20,
  clean_fixture$relative_humidity_percent[1] == 50,
  clean_fixture$temperature_c[1] == 20,
  clean_fixture$o3_flag[3] == "OUT_OF_RANGE_HIGH"
)

hourly_utc_fixture <- aqmesh_build_utc_composite(clean_fixture)
display_fixture <- aqmesh_build_display_composite(hourly_utc_fixture)
stopifnot(
  nrow(hourly_utc_fixture) == 3L,
  nrow(display_fixture) == 2L,
  display_fixture$dst_fold_count[display_fixture$hour == 1L] == 2L,
  display_fixture$composite_pm25_ug_m3[display_fixture$hour == 1L] == 41,
  display_fixture$composite_pm10_ug_m3[display_fixture$hour == 1L] == 62,
  display_fixture$composite_no2_ppb[display_fixture$hour == 1L] == 11,
  display_fixture$composite_o3_ppb[display_fixture$hour == 1L] == 32,
  display_fixture$composite_so2_ppb[display_fixture$hour == 1L] == 5.5,
  display_fixture$composite_co_ppm[display_fixture$hour == 1L] == 0.45,
  display_fixture$composite_tvoc_ppb[display_fixture$hour == 1L] == 25,
  display_fixture$composite_temperature_c[display_fixture$hour == 1L] == 21,
  display_fixture$composite_relative_humidity_percent[
    display_fixture$hour == 1L
  ] == 52
)

complete_fixture <- aqmesh_complete_display_hours(display_fixture)
stopifnot(
  nrow(complete_fixture) == 24L,
  length(unique(complete_fixture$hour)) == 24L,
  sum(is.finite(complete_fixture$composite_pm25_ug_m3)) == 1L,
  sum(is.finite(complete_fixture$composite_pm10_ug_m3)) == 1L,
  sum(is.finite(complete_fixture$composite_no2_ppb)) == 1L,
  sum(is.finite(complete_fixture$composite_so2_ppb)) == 1L,
  sum(is.finite(complete_fixture$composite_co_ppm)) == 1L,
  sum(is.finite(complete_fixture$composite_tvoc_ppb)) == 1L
)

quality_fixture <- tceq_dashboard_yearly_quality(list(
  metadata = tibble::tibble(data_source = "Dallas AQMesh"),
  hourly = complete_fixture
))
stopifnot(
  quality_fixture$preferred_label == "Valid AQMesh scaled PM2.5",
  quality_fixture$preferred_hours == 1L
)

dst_spikes <- tceq_detect_short_spikes(hourly_utc_fixture)
stopifnot(nrow(dst_spikes) == 1L, dst_spikes$duration_hours == 2L)

fault_fixture <- revision_fixture[1, ]
fault_fixture$OBJECTID <- 10L
fault_fixture$reading_status <- "Deliquescence"
fault_fixture$pm2_5_scaled <- 55
fault_clean <- aqmesh_clean_latest_records(aqmesh_latest_revisions(fault_fixture))
stopifnot(
  is.na(fault_clean$pm25_ug_m3),
  fault_clean$value_flag == "STATUS_DELIQUESCENCE"
)

station_fixture <- tibble::tibble(
  DallasStationName = "South Central Park (Joppa Neighborhood)",
  location_name = "South Central Park",
  pod_serial_number = 2450954L,
  location_number = 2808L,
  Station_status = "Offline",
  Group_Type = "Group A",
  geometry.x = -96.744099,
  geometry.y = 32.717309
)
station_clean <- aqmesh_station_metadata(station_fixture)
stopifnot(
  station_clean$aqs_site_id == "DAL_AQMESH_SOUTH_CENTRAL_PARK",
  station_clean$site_name == "South Central Park (Joppa Neighborhood)",
  !station_clean$active
)

stopifnot(
  aqmesh_canonical_location("Fish Trap Lake  Park") == "Fish Trap Lake Park",
  aqmesh_canonical_location("Willis Wintrers Park") == "Willis Winters Park",
  aqmesh_location_class("Characterisation 2810") == "characterisation",
  aqmesh_location_class("Pod 2451142") == "commissioning"
)
