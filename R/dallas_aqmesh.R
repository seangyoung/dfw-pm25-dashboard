# Reusable acquisition and preparation helpers for the City of Dallas AQMesh
# hourly archive. The ArcGIS layer stores timestamps as UTC epoch milliseconds.
# Raw responses are retained by the acquisition script; analytical records use
# the latest published revision for each location, pod, and UTC hour.

aqmesh_hourly_layer_url <- function() {
  paste0(
    "https://services2.arcgis.com/rwnOSbfKSwyTBcwN/arcgis/rest/services/",
    "AqmeshHrlyDataWLoc/FeatureServer/0"
  )
}

aqmesh_station_layer_url <- function() {
  paste0(
    "https://services2.arcgis.com/rwnOSbfKSwyTBcwN/arcgis/rest/services/",
    "AQ_Mesh_Air_Monitor_Stations/FeatureServer/0"
  )
}

aqmesh_raw_dir <- function(cfg) {
  file.path(cfg$paths$raw, "dallas_aqmesh_hourly")
}

aqmesh_import_dir <- function(cfg) {
  file.path(cfg$paths$interim, "dallas_aqmesh_hourly")
}

pm25_dashboard_cache_dir <- function(cfg) {
  file.path(cfg$paths$derived, "dfw_pm25_dashboard")
}

pm25_combined_cache_is_stale <- function(cache_manifest, repo_root, cfg) {
  expected <- c(
    tceq = as.character(cache_manifest$tceq$raw_manifest_sha256),
    dallas_aqmesh = as.character(
      cache_manifest$dallas_aqmesh$raw_download_manifest_sha256
    )
  )
  paths <- c(
    tceq = file.path(
      tceq_dashboard_raw_dir(repo_root), "download_manifest.csv"
    ),
    dallas_aqmesh = file.path(aqmesh_raw_dir(cfg), "download_manifest.csv")
  )
  available <- nzchar(expected) & !is.na(expected) & file.exists(paths)
  if (!any(available)) return(NA)
  actual <- vapply(paths[available], sha256_file, character(1))
  any(actual != expected[available])
}

aqmesh_epoch_milliseconds <- function(x) {
  as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01", tz = "UTC")
}

aqmesh_wall_datetime <- function(datetime_utc, timezone = "America/Chicago") {
  as.POSIXct(
    format(datetime_utc, "%Y-%m-%d %H:%M:%S", tz = timezone),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
}

aqmesh_normalize_status <- function(x) {
  value <- tolower(trimws(as.character(x)))
  value[is.na(value) | value %in% c("", "na", "n/a", "null")] <- "not_reported"
  value
}

aqmesh_canonical_location <- function(x) {
  value <- trimws(gsub("[[:space:]]+", " ", as.character(x)))
  aliases <- c(
    "Fish Trap Lake" = "Fish Trap Lake Park",
    "Fish Trap Lake Park" = "Fish Trap Lake Park",
    "W Dallas Multipurpose Center" = "West Dallas Multipurpose Center",
    "Larry Johnson Rec Center" = "Larry Johnson Recreation Center",
    "Willis Wintrers Park" = "Willis Winters Park",
    "fretz Park" = "Fretz Park"
  )
  matched <- match(value, names(aliases))
  value[!is.na(matched)] <- unname(aliases[matched[!is.na(matched)]])
  value
}

aqmesh_location_class <- function(location_name) {
  value <- trimws(as.character(location_name))
  dplyr::case_when(
    is.na(value) | !nzchar(value) ~ "missing_location",
    grepl("^Characterisation ", value, ignore.case = TRUE) ~ "characterisation",
    grepl("^Pod [0-9]+$", value, ignore.case = TRUE) ~ "commissioning",
    TRUE ~ "field_site"
  )
}

aqmesh_site_key <- function(location_name) {
  slug <- tolower(aqmesh_canonical_location(location_name))
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("^_+|_+$", "", slug)
  paste0("DAL_AQMESH_", toupper(slug))
}

aqmesh_parse_features <- function(path_or_text) {
  require_packages(c("jsonlite", "tibble"))
  parsed <- jsonlite::fromJSON(path_or_text, flatten = TRUE)
  if (!is.null(parsed$error)) {
    stop("ArcGIS response error: ", parsed$error$message)
  }
  features <- parsed$features
  if (is.null(features) || !nrow(features)) return(tibble::tibble())
  attribute_names <- grep("^attributes\\.", names(features), value = TRUE)
  geometry_names <- grep("^geometry\\.", names(features), value = TRUE)
  result <- features[c(attribute_names, geometry_names)]
  names(result) <- sub("^attributes\\.", "", names(result))
  tibble::as_tibble(result)
}

aqmesh_latest_revisions <- function(records) {
  require_packages(c("dplyr", "tibble"))
  required <- c(
    "location_name", "pod_serial_number", "project_time_interval_start",
    "append_date", "OBJECTID"
  )
  tceq_assert_columns(records, required, "Dallas AQMesh records")
  records |>
    dplyr::mutate(
      revision_location = dplyr::coalesce(
        trimws(as.character(.data$location_name)), "<missing>"
      ),
      revision_pod = dplyr::coalesce(
        as.character(.data$pod_serial_number), "<missing>"
      ),
      revision_time = as.numeric(.data$project_time_interval_start),
      revision_append = as.numeric(.data$append_date),
      revision_objectid = as.numeric(.data$OBJECTID)
    ) |>
    dplyr::group_by(
      .data$revision_location, .data$revision_pod, .data$revision_time
    ) |>
    dplyr::arrange(
      .data$revision_append, .data$revision_objectid,
      .by_group = TRUE, na.last = TRUE
    ) |>
    dplyr::mutate(
      revision_count = dplyr::n(),
      selected_revision = dplyr::row_number() == dplyr::n()
    ) |>
    dplyr::filter(.data$selected_revision) |>
    dplyr::ungroup() |>
    dplyr::select(
      -"revision_location", -"revision_pod", -"revision_time",
      -"revision_append", -"revision_objectid", -"selected_revision"
    )
}

aqmesh_clean_latest_records <- function(records) {
  require_packages(c("dplyr", "tibble"))
  required <- c(
    "OBJECTID", "location_name", "pod_serial_number",
    "project_time_interval_start", "project_time_interval_end", "append_date",
    "pm2_5_prescale", "pm2_5_scaled", "reading_status", "particleprotocol_version",
    "pod_latitude", "pod_longitude"
  )
  tceq_assert_columns(records, required, "Latest Dallas AQMesh records")
  x <- records |>
    dplyr::mutate(
      raw_location_name = as.character(.data$location_name),
      canonical_location_name = aqmesh_canonical_location(.data$location_name),
      location_class = aqmesh_location_class(.data$location_name),
      aqs_site_id = aqmesh_site_key(.data$canonical_location_name),
      site_name = .data$canonical_location_name,
      datetime_utc = aqmesh_epoch_milliseconds(.data$project_time_interval_start),
      interval_end_utc = aqmesh_epoch_milliseconds(.data$project_time_interval_end),
      append_datetime_utc = aqmesh_epoch_milliseconds(.data$append_date),
      datetime_lstd = aqmesh_wall_datetime(.data$datetime_utc),
      date = as.Date(format(.data$datetime_utc, "%Y-%m-%d", tz = "America/Chicago")),
      hour = as.integer(format(.data$datetime_utc, "%H", tz = "America/Chicago")),
      hour_lstd = sprintf("%02d:00", .data$hour),
      reading_status_normalized = aqmesh_normalize_status(.data$reading_status),
      pm25_raw = as.character(.data$pm2_5_scaled),
      pm25_scaled_raw = suppressWarnings(as.numeric(.data$pm2_5_scaled)),
      pm25_prescale_raw = suppressWarnings(as.numeric(.data$pm2_5_prescale)),
      value_flag = dplyr::case_when(
        is.finite(.data$pm25_scaled_raw) & .data$pm25_scaled_raw <= -900 ~ "SENTINEL",
        is.finite(.data$pm25_scaled_raw) & .data$pm25_scaled_raw < 0 ~ "NEG",
        .data$reading_status_normalized %in% c(
          "deliq", "deliquescence", "other fault zero"
        ) ~ paste0("STATUS_", toupper(gsub(" ", "_", .data$reading_status_normalized))),
        !is.finite(.data$pm25_scaled_raw) ~ "MISSING",
        TRUE ~ NA_character_
      ),
      valid_pm25 = is.finite(.data$pm25_scaled_raw) &
        .data$pm25_scaled_raw >= 0 &
        !.data$reading_status_normalized %in% c(
          "deliq", "deliquescence", "other fault zero"
        ),
      pm25_ug_m3 = dplyr::if_else(
        .data$valid_pm25, .data$pm25_scaled_raw, NA_real_
      ),
      regulatory_qa = FALSE,
      parameter_code = "AQMESH_PM25_SCALED",
      poc = suppressWarnings(as.integer(.data$pod_serial_number)),
      report_block = NA_integer_,
      source_file = paste0(
        "dallas_aqmesh_",
        format(.data$datetime_utc, "%Y-%m", tz = "UTC"), ".json.gz"
      ),
      raw_series_key = paste0("AQMESH::", .data$pod_serial_number)
    )
  x
}

aqmesh_station_metadata <- function(station_features) {
  require_packages(c("dplyr", "tibble"))
  required <- c(
    "DallasStationName", "location_name", "pod_serial_number", "location_number",
    "Station_status", "Group_Type", "geometry.x", "geometry.y"
  )
  tceq_assert_columns(station_features, required, "Dallas AQMesh station layer")
  station_features |>
    dplyr::transmute(
      aqs_site_id = aqmesh_site_key(
        aqmesh_canonical_location(.data$location_name)
      ),
      source_station_id = dplyr::if_else(
        is.na(.data$location_number),
        paste0("AQMesh placeholder ", .data$pod_serial_number),
        paste0("Dallas location ", .data$location_number)
      ),
      site_name = as.character(.data$DallasStationName),
      location_name = as.character(.data$location_name),
      location_number = suppressWarnings(as.integer(.data$location_number)),
      pod_serial_number = suppressWarnings(as.integer(.data$pod_serial_number)),
      latitude = as.numeric(.data$geometry.y),
      longitude = as.numeric(.data$geometry.x),
      current_status = as.character(.data$Station_status),
      active = tolower(.data$current_status) == "active",
      station_group = as.character(.data$Group_Type),
      city = "Dallas",
      county = "Dallas",
      state = "Texas",
      data_source = "Dallas AQMesh",
      network_label = "City of Dallas AQMesh",
      time_basis = "America/Chicago local civil time",
      time_label = "local time",
      metadata_url = aqmesh_station_layer_url(),
      metadata_retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ) |>
    dplyr::distinct(.data$aqs_site_id, .keep_all = TRUE)
}

aqmesh_build_utc_composite <- function(clean_records, field_only = TRUE) {
  require_packages(c("dplyr", "tibble"))
  x <- clean_records
  if (field_only) x <- dplyr::filter(x, .data$location_class == "field_site")
  keys <- c(
    "aqs_site_id", "site_name", "date", "hour", "hour_lstd",
    "datetime_lstd", "datetime_utc"
  )
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      total_records = dplyr::n(),
      available_records = sum(is.finite(.data$pm25_ug_m3)),
      composite_pm25_ug_m3 = tceq_safe_median(.data$pm25_ug_m3),
      contributing_records = sum(is.finite(.data$pm25_ug_m3)),
      composite_min_ug_m3 = tceq_safe_min(.data$pm25_ug_m3),
      composite_max_ug_m3 = tceq_safe_max(.data$pm25_ug_m3),
      composite_spread_ug_m3 = .data$composite_max_ug_m3 - .data$composite_min_ug_m3,
      contributor_pocs = tceq_collapse_values(.data$pod_serial_number),
      contributor_sources = tceq_collapse_values(.data$raw_series_key),
      value_flag_summary = tceq_collapse_values(.data$value_flag),
      revision_count = sum(.data$revision_count, na.rm = TRUE),
      reading_status_summary = tceq_collapse_values(.data$reading_status_normalized),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      selected_parameter_code = dplyr::if_else(
        is.finite(.data$composite_pm25_ug_m3), "AQMESH_PM25_SCALED", NA_character_
      ),
      parameter_fallback = FALSE,
      qa_fallback = is.finite(.data$composite_pm25_ug_m3),
      qa_status_unavailable = FALSE,
      has_regulatory_value = FALSE,
      composite_selection = dplyr::if_else(
        is.finite(.data$composite_pm25_ug_m3),
        "aqmesh_scaled_latest_revision", "missing"
      ),
      data_source = "Dallas AQMesh"
    ) |>
    dplyr::arrange(.data$datetime_utc)
}

aqmesh_build_display_composite <- function(hourly_utc) {
  require_packages(c("dplyr", "tibble"))
  keys <- c("aqs_site_id", "site_name", "date", "hour", "hour_lstd", "datetime_lstd")
  hourly_utc |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      dst_fold_count = dplyr::n_distinct(.data$datetime_utc),
      datetime_utc = min(.data$datetime_utc),
      total_records = sum(.data$total_records, na.rm = TRUE),
      available_records = sum(.data$available_records, na.rm = TRUE),
      composite_pm25_ug_m3 = tceq_safe_median(.data$composite_pm25_ug_m3),
      contributing_records = sum(.data$contributing_records, na.rm = TRUE),
      composite_min_ug_m3 = tceq_safe_min(.data$composite_min_ug_m3),
      composite_max_ug_m3 = tceq_safe_max(.data$composite_max_ug_m3),
      composite_spread_ug_m3 = .data$composite_max_ug_m3 - .data$composite_min_ug_m3,
      contributor_pocs = tceq_collapse_values(.data$contributor_pocs),
      contributor_sources = tceq_collapse_values(.data$contributor_sources),
      value_flag_summary = tceq_collapse_values(.data$value_flag_summary),
      reading_status_summary = tceq_collapse_values(.data$reading_status_summary),
      revision_count = sum(.data$revision_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      selected_parameter_code = dplyr::if_else(
        is.finite(.data$composite_pm25_ug_m3), "AQMESH_PM25_SCALED", NA_character_
      ),
      parameter_fallback = FALSE,
      qa_fallback = is.finite(.data$composite_pm25_ug_m3),
      qa_status_unavailable = FALSE,
      has_regulatory_value = FALSE,
      composite_selection = dplyr::case_when(
        !is.finite(.data$composite_pm25_ug_m3) ~ "missing",
        .data$dst_fold_count > 1L ~ "aqmesh_scaled_dst_fold_median",
        TRUE ~ "aqmesh_scaled_latest_revision"
      ),
      data_source = "Dallas AQMesh"
    ) |>
    dplyr::arrange(.data$date, .data$hour)
}

aqmesh_complete_display_hours <- function(hourly_display) {
  require_packages(c("dplyr", "tidyr", "tibble"))
  if (!nrow(hourly_display)) return(hourly_display)
  site_ids <- unique(hourly_display$aqs_site_id)
  site_names <- unique(hourly_display$site_name)
  if (length(site_ids) != 1L || length(site_names) != 1L) {
    stop("Display-hour completion expects exactly one Dallas monitoring location.")
  }
  grid <- tidyr::crossing(
    date = seq(min(hourly_display$date), max(hourly_display$date), by = "day"),
    hour = 0:23
  ) |>
    dplyr::mutate(
      aqs_site_id = site_ids,
      site_name = site_names,
      hour_lstd = sprintf("%02d:00", .data$hour),
      datetime_lstd = tceq_lstd_datetime(.data$date, .data$hour)
    )
  grid |>
    dplyr::left_join(
      hourly_display |>
        dplyr::select(-"site_name", -"hour_lstd"),
      by = c("aqs_site_id", "date", "hour", "datetime_lstd")
    ) |>
    dplyr::mutate(
      total_records = dplyr::coalesce(.data$total_records, 0L),
      available_records = dplyr::coalesce(.data$available_records, 0L),
      contributing_records = dplyr::coalesce(.data$contributing_records, 0L),
      dst_fold_count = dplyr::coalesce(.data$dst_fold_count, 0L),
      revision_count = dplyr::coalesce(.data$revision_count, 0L),
      parameter_fallback = dplyr::coalesce(.data$parameter_fallback, FALSE),
      qa_fallback = dplyr::coalesce(.data$qa_fallback, FALSE),
      qa_status_unavailable = dplyr::coalesce(.data$qa_status_unavailable, FALSE),
      has_regulatory_value = FALSE,
      composite_selection = dplyr::coalesce(.data$composite_selection, "missing"),
      value_flag_summary = dplyr::coalesce(.data$value_flag_summary, ""),
      reading_status_summary = dplyr::coalesce(.data$reading_status_summary, ""),
      data_source = "Dallas AQMesh"
    ) |>
    dplyr::arrange(.data$date, .data$hour)
}

pm25_build_computed_daily <- function(hourly, minimum_valid_hours = 18L) {
  require_packages(c("dplyr", "tibble"))
  tceq_assert_columns(
    hourly,
    c(
      "aqs_site_id", "date", "hour", "composite_pm25_ug_m3", "qa_fallback",
      "parameter_fallback", "contributing_records"
    ),
    "Hourly site composite"
  )
  hourly |>
    dplyr::group_by(.data$aqs_site_id, .data$date) |>
    dplyr::summarise(
      valid_hours = sum(is.finite(.data$composite_pm25_ug_m3)),
      represented_hours = dplyr::n_distinct(.data$hour),
      computed_daily_max_ug_m3 = tceq_safe_max(.data$composite_pm25_ug_m3),
      computed_daily_avg_ug_m3 = tceq_safe_mean(.data$composite_pm25_ug_m3),
      computed_daily_std_ug_m3 = if (sum(is.finite(.data$composite_pm25_ug_m3)) > 1L) {
        stats::sd(.data$composite_pm25_ug_m3, na.rm = TRUE)
      } else NA_real_,
      qa_fallback_hours = sum(.data$qa_fallback & is.finite(.data$composite_pm25_ug_m3)),
      parameter_fallback_hours = sum(
        .data$parameter_fallback & is.finite(.data$composite_pm25_ug_m3)
      ),
      contributing_records = sum(.data$contributing_records, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(event_eligible = .data$valid_hours >= minimum_valid_hours)
}

aqmesh_build_daily_summary <- function(hourly_utc, minimum_valid_hours = 18L) {
  computed <- pm25_build_computed_daily(hourly_utc, minimum_valid_hours)
  computed |>
    dplyr::mutate(
      reported_daily_records = NA_integer_,
      reported_parameter_codes = NA_character_,
      reported_daily_max_median_ug_m3 = NA_real_,
      reported_daily_max_min_ug_m3 = NA_real_,
      reported_daily_max_max_ug_m3 = NA_real_,
      reported_daily_avg_median_ug_m3 = NA_real_,
      reported_daily_avg_min_ug_m3 = NA_real_,
      reported_daily_avg_max_ug_m3 = NA_real_,
      reported_daily_std_median_ug_m3 = NA_real_,
      reported_daily_flags = NA_character_,
      computed_minus_reported_avg_ug_m3 = NA_real_,
      computed_minus_reported_max_ug_m3 = NA_real_,
      data_source = "Dallas AQMesh"
    ) |>
    dplyr::arrange(.data$date)
}
