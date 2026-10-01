#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- sub("^--file=", "", script_arg[1])
  root <- dirname(dirname(normalizePath(script_path)))
} else {
  root <- normalizePath(getwd())
}

source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "tceq_pm25_dashboard.R"))
source(file.path(root, "R", "dallas_aqmesh.R"))
require_packages(c(
  "digest", "dplyr", "jsonlite", "purrr", "readr", "tibble", "tidyr", "yaml"
))

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "gzip")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path)
  invisible(path)
}

atomic_write_csv <- function(object, path) {
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  readr::write_csv(object, temporary, na = "")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path)
  invisible(path)
}

args <- commandArgs(trailingOnly = TRUE)
tceq_arg <- grep("^--tceq-cache-dir=", args, value = TRUE)
aqmesh_arg <- grep("^--aqmesh-import-dir=", args, value = TRUE)
cache_arg <- grep("^--cache-dir=", args, value = TRUE)
if (any(c(length(tceq_arg), length(aqmesh_arg), length(cache_arg)) > 1L)) {
  stop("Specify each directory argument no more than once.")
}
unknown <- setdiff(args, c(tceq_arg, aqmesh_arg, cache_arg))
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

cfg <- read_config()
tceq_cache <- if (length(tceq_arg)) {
  normalizePath(sub("^--tceq-cache-dir=", "", tceq_arg), mustWork = TRUE)
} else tceq_dashboard_cache_dir(cfg)
aqmesh_import <- if (length(aqmesh_arg)) {
  normalizePath(sub("^--aqmesh-import-dir=", "", aqmesh_arg), mustWork = TRUE)
} else aqmesh_import_dir(cfg)
cache_dir <- if (length(cache_arg)) {
  normalizePath(sub("^--cache-dir=", "", cache_arg), mustWork = FALSE)
} else pm25_dashboard_cache_dir(cfg)

required_tceq <- file.path(
  tceq_cache, c("station_index.rds", "event_index.rds", "cache_manifest.json")
)
required_aqmesh <- file.path(
  aqmesh_import,
  c("latest_records.rds", "station_metadata.rds", "import_metadata.json")
)
if (!all(file.exists(required_tceq))) {
  stop("TCEQ dashboard cache is incomplete. Run scripts/08c_prepare_tceq_dfw_pm25_dashboard.R.")
}
if (!all(file.exists(required_aqmesh))) {
  stop(
    "Dallas AQMesh import is incomplete. Run scripts/08f_acquire_dallas_aqmesh_hourly.R ",
    "and scripts/08g_import_dallas_aqmesh_hourly.R."
  )
}
dir.create(file.path(cache_dir, "sites"), recursive = TRUE, showWarnings = FALSE)

tceq_stations <- readRDS(file.path(tceq_cache, "station_index.rds"))
tceq_manifest <- jsonlite::read_json(
  file.path(tceq_cache, "cache_manifest.json"), simplifyVector = TRUE
)
aqmesh_records <- readRDS(file.path(aqmesh_import, "latest_records.rds"))
aqmesh_stations <- readRDS(file.path(aqmesh_import, "station_metadata.rds"))
aqmesh_manifest <- jsonlite::read_json(
  file.path(aqmesh_import, "import_metadata.json"), simplifyVector = TRUE
)

site_summaries <- list()
daily_indexes <- list()
event_indexes <- list()

message("Normalizing ", nrow(tceq_stations), " TCEQ site bundles...")
for (site_id in tceq_stations$aqs_site_id) {
  bundle <- tceq_read_site_bundle(tceq_cache, site_id)
  bundle$metadata <- bundle$metadata |>
    dplyr::mutate(
      data_source = "TCEQ",
      network_label = "Texas Commission on Environmental Quality",
      source_station_id = .data$aqs_site_id,
      time_basis = "Local standard time (UTC-06:00 year-round)",
      time_label = "LST"
    )
  bundle$hourly <- bundle$hourly |>
    dplyr::mutate(
      datetime_utc = .data$datetime_lstd + 6 * 3600,
      data_source = "TCEQ"
    )
  bundle$daily <- bundle$daily |>
    dplyr::mutate(data_source = "TCEQ")
  bundle$events <- bundle$events |>
    dplyr::mutate(data_source = "TCEQ", time_label = "LST")
  bundle$raw_hourly <- bundle$raw_hourly |>
    dplyr::mutate(
      datetime_utc = .data$datetime_lstd + 6 * 3600,
      data_source = "TCEQ",
      reading_status_normalized = dplyr::case_when(
        .data$regulatory_qa %in% TRUE ~ "regulatory_qa",
        .data$regulatory_qa %in% FALSE ~ "nonregulatory_qa",
        TRUE ~ "not_reported"
      )
    )
  bundle$schema_version <- 3L
  output_path <- file.path(cache_dir, "sites", tceq_site_cache_filename(site_id))
  atomic_save_rds(bundle, output_path)
  row <- tceq_stations[tceq_stations$aqs_site_id == site_id, , drop = FALSE]
  site_summaries[[site_id]] <- row |>
    dplyr::mutate(
      data_source = "TCEQ",
      network_label = "Texas Commission on Environmental Quality",
      source_station_id = .data$aqs_site_id,
      time_basis = "Local standard time (UTC-06:00 year-round)",
      time_label = "LST",
      station_group = NA_character_,
      location_number = NA_integer_,
      pod_serial_number = NA_integer_,
      site_cache_file = file.path("sites", basename(output_path)),
      site_cache_sha256 = sha256_file(output_path)
    )
  daily_indexes[[site_id]] <- bundle$daily
  event_indexes[[site_id]] <- bundle$events
  rm(bundle)
  invisible(gc())
}

field_records <- aqmesh_records |>
  dplyr::filter(.data$location_class == "field_site")
field_ids <- sort(unique(field_records$aqs_site_id))
missing_metadata <- setdiff(field_ids, aqmesh_stations$aqs_site_id)
if (length(missing_metadata)) {
  stop(
    "Dallas field locations lack official station metadata: ",
    paste(missing_metadata, collapse = ", "), "."
  )
}
message("Preparing ", length(field_ids), " Dallas AQMesh field-location bundles...")
for (site_id in field_ids) {
  raw <- field_records |>
    dplyr::filter(.data$aqs_site_id == site_id) |>
    dplyr::arrange(.data$datetime_utc, .data$pod_serial_number)
  hourly_utc <- aqmesh_build_utc_composite(raw, field_only = FALSE)
  hourly <- hourly_utc |>
    aqmesh_build_display_composite() |>
    aqmesh_complete_display_hours()
  daily <- aqmesh_build_daily_summary(hourly_utc, minimum_valid_hours = 18L)
  events <- tceq_build_event_index(hourly_utc, daily) |>
    dplyr::mutate(data_source = "Dallas AQMesh", time_label = "local time")
  metadata <- aqmesh_stations |>
    dplyr::filter(.data$aqs_site_id == site_id)
  diagnostic_raw <- raw |>
    dplyr::select(dplyr::all_of(c(
      "datetime_lstd", "datetime_utc", "pm25_raw", "pm25_scaled_raw",
      "pm25_prescale_raw", "pm25_ug_m3", "value_flag", "reading_status",
      "reading_status_normalized", "particleprotocol_version",
      "pod_serial_number", "source_file", "report_block", "poc",
      "parameter_code", "regulatory_qa", "raw_series_key", "revision_count",
      "OBJECTID", "append_datetime_utc"
    ))) |>
    dplyr::mutate(data_source = "Dallas AQMesh")
  bundle <- list(
    schema_version = 3L,
    metadata = metadata,
    hourly = tibble::as_tibble(hourly),
    daily = tibble::as_tibble(daily),
    events = tibble::as_tibble(events),
    raw_hourly = tibble::as_tibble(diagnostic_raw),
    provenance = list(
      source = "City of Dallas AQMesh ArcGIS hourly layer",
      analytical_value = "pm2_5_scaled",
      regulatory_status = paste(
        "Non-regulatory, unverified community-sensor data; not a NAAQS determination"
      ),
      raw_revision_archive = normalizePath(aqmesh_raw_dir(cfg), winslash = "/")
    )
  )
  output_path <- file.path(cache_dir, "sites", tceq_site_cache_filename(site_id))
  atomic_save_rds(bundle, output_path)
  site_summaries[[site_id]] <- metadata |>
    dplyr::mutate(
      first_date = min(hourly$date),
      last_date = max(hourly$date),
      hourly_rows = nrow(hourly),
      valid_hours = sum(is.finite(hourly$composite_pm25_ug_m3)),
      qa_fallback_hours = sum(
        hourly$qa_fallback & is.finite(hourly$composite_pm25_ug_m3)
      ),
      parameter_88502_fallback_hours = 0L,
      short_spike_events = sum(events$event_type == "short_spike"),
      multi_day_events = sum(events$event_type == "multi_day"),
      site_cache_file = file.path("sites", basename(output_path)),
      site_cache_sha256 = sha256_file(output_path),
      cams_id = NA_integer_,
      cams_aliases = NA_character_,
      address = NA_character_,
      metadata_site_name = .data$site_name
    )
  daily_indexes[[site_id]] <- daily
  event_indexes[[site_id]] <- events
  rm(raw, diagnostic_raw, hourly_utc, hourly, daily, events, bundle)
  invisible(gc())
}

station_index <- dplyr::bind_rows(site_summaries) |>
  dplyr::arrange(.data$data_source, dplyr::desc(.data$active), .data$site_name)
if (anyDuplicated(station_index$aqs_site_id) || anyNA(station_index$latitude) ||
    anyNA(station_index$longitude)) {
  stop("Combined station index requires unique IDs and validated coordinates.")
}
daily_index <- dplyr::bind_rows(daily_indexes) |>
  dplyr::arrange(.data$aqs_site_id, .data$date)
event_index <- dplyr::bind_rows(event_indexes) |>
  dplyr::arrange(.data$aqs_site_id, .data$start_lstd, .data$event_type)

atomic_save_rds(station_index, file.path(cache_dir, "station_index.rds"))
atomic_save_rds(daily_index, file.path(cache_dir, "daily_index.rds"))
atomic_save_rds(event_index, file.path(cache_dir, "event_index.rds"))
atomic_write_csv(station_index, file.path(cache_dir, "station_index.csv"))
atomic_write_csv(event_index, file.path(cache_dir, "event_index.csv"))

manifest <- list(
  schema_version = 3L,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  data_sources = c("TCEQ", "Dallas AQMesh"),
  source_counts = as.list(table(station_index$data_source)),
  station_count = nrow(station_index),
  hourly_composite_rows = sum(station_index$hourly_rows),
  valid_hourly_values = sum(station_index$valid_hours),
  event_count = nrow(event_index),
  first_date = format(min(station_index$first_date), "%Y-%m-%d"),
  last_date = format(max(station_index$last_date), "%Y-%m-%d"),
  preparation_command = "Rscript --vanilla scripts/08h_prepare_combined_pm25_dashboard.R",
  tceq = list(
    source_cache = normalizePath(tceq_cache, winslash = "/"),
    cache_manifest_sha256 = sha256_file(file.path(tceq_cache, "cache_manifest.json")),
    raw_manifest_sha256 = tceq_manifest$raw_manifest_sha256,
    parameter_priority = c("88101", "88502")
  ),
  dallas_aqmesh = list(
    source_import = normalizePath(aqmesh_import, winslash = "/"),
    import_metadata_sha256 = sha256_file(file.path(aqmesh_import, "import_metadata.json")),
    raw_download_manifest_sha256 = aqmesh_manifest$raw_download_manifest_sha256,
    analytical_value = "pm2_5_scaled",
    revision_rule = aqmesh_manifest$revision_rule,
    regulatory_status = "Non-regulatory, unverified community-sensor data"
  ),
  algorithms = list(
    daily_minimum_valid_hours = 18L,
    short_spike = "Strictly greater than 35 ug/m3 for a maximal run of 1-5 consecutive UTC hours",
    multi_day = "At least two core days with daily max >25 and mean >20 ug/m3; one eligible trough day may bridge",
    dallas_dst_display = paste(
      "Events use UTC continuity. Repeated fall-back display hours are represented",
      "by their median in the 24-column heatmap and retained separately in hourly_event."
    )
  ),
  cache_files = list(
    station_index = "station_index.rds",
    daily_index = "daily_index.rds",
    event_index = "event_index.rds",
    sites = station_index$site_cache_file
  )
)
jsonlite::write_json(
  manifest, file.path(cache_dir, "cache_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
message(
  "Combined dashboard cache ready: ", cache_dir, "\n",
  nrow(station_index), " sites (",
  paste(names(table(station_index$data_source)), table(station_index$data_source), collapse = "; "),
  "); ", format(nrow(event_index), big.mark = ","), " events."
)
