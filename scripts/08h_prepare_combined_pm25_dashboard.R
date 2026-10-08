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
source(file.path(root, "R", "pm25_regional.R"))
require_packages(c(
  "curl", "digest", "dplyr", "jsonlite", "purrr", "readr", "sf", "tibble",
  "tidyr", "yaml"
))

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "gzip")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path)
  invisible(path)
}

atomic_save_rds_xz <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "xz")
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
boundary_arg <- grep("^--boundary-dir=", args, value = TRUE)
if (any(c(
    length(tceq_arg), length(aqmesh_arg), length(cache_arg), length(boundary_arg)
  ) > 1L)) {
  stop("Specify each directory argument no more than once.")
}
unknown <- setdiff(args, c(tceq_arg, aqmesh_arg, cache_arg, boundary_arg))
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
boundary_dir <- if (length(boundary_arg)) {
  normalizePath(sub("^--boundary-dir=", "", boundary_arg), mustWork = FALSE)
} else file.path(cfg$paths$reference, "dfw_pm25_dashboard")

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
regional_hourly_rows <- list()

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
  regional_hourly_rows[[site_id]] <- bundle$hourly |>
    dplyr::select(
      "aqs_site_id", "datetime_utc", "composite_pm25_ug_m3"
    )
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
  regional_hourly_rows[[site_id]] <- hourly_utc |>
    dplyr::select(
      "aqs_site_id", "datetime_utc", "composite_pm25_ug_m3"
    )
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

message("Preparing compact regional cross-sensor caches...")
regional_daily <- pm25_build_regional_daily(
  daily_index, event_index, station_index
)
regional_reach <- pm25_daily_reach(regional_daily)
regional_event_links <- pm25_build_regional_event_links(
  event_index, station_index
)
regional_hourly <- pm25_build_regional_hourly(
  dplyr::bind_rows(regional_hourly_rows), station_index
)
counties <- pm25_download_county_boundary(boundary_dir, year = 2024L)
spatial_support <- pm25_build_spatial_support(
  station_index, counties, grid_resolution_m = 4000, analysis_crs = 3083
)
spatial_validation <- pm25_surface_validation(
  regional_daily, station_index, spatial_support, regional_hourly
)

atomic_save_rds(station_index, file.path(cache_dir, "station_index.rds"))
atomic_save_rds(daily_index, file.path(cache_dir, "daily_index.rds"))
atomic_save_rds(event_index, file.path(cache_dir, "event_index.rds"))
atomic_save_rds_xz(regional_daily, file.path(cache_dir, "regional_daily.rds"))
atomic_save_rds_xz(regional_reach, file.path(cache_dir, "regional_reach.rds"))
atomic_save_rds_xz(
  regional_event_links, file.path(cache_dir, "regional_event_links.rds")
)
hourly_directory <- file.path(cache_dir, "regional_hourly")
hourly_temporary <- tempfile("regional_hourly_", tmpdir = cache_dir)
dir.create(hourly_temporary, recursive = TRUE)
for (year in names(regional_hourly$shards)) {
  saveRDS(
    regional_hourly$shards[[year]],
    file.path(hourly_temporary, paste0("year_", year, ".rds")),
    compress = "xz"
  )
}
if (dir.exists(hourly_directory)) unlink(hourly_directory, recursive = TRUE)
if (!file.rename(hourly_temporary, hourly_directory)) {
  stop("Could not atomically replace the regional hourly shards.")
}
atomic_save_rds_xz(
  regional_hourly$index, file.path(cache_dir, "regional_hourly_index.rds")
)
atomic_save_rds_xz(spatial_support, file.path(cache_dir, "spatial_support.rds"))
atomic_save_rds_xz(
  spatial_validation, file.path(cache_dir, "spatial_validation.rds")
)
atomic_write_csv(station_index, file.path(cache_dir, "station_index.csv"))
atomic_write_csv(event_index, file.path(cache_dir, "event_index.csv"))

regional_files <- c(
  regional_daily = "regional_daily.rds",
  regional_reach = "regional_reach.rds",
  regional_event_links = "regional_event_links.rds",
  regional_hourly_index = "regional_hourly_index.rds",
  spatial_support = "spatial_support.rds",
  spatial_validation = "spatial_validation.rds"
)
regional_hourly_files <- regional_hourly$index$years$file
all_regional_files <- c(regional_files, regional_hourly_files)
manifest <- list(
  schema_version = 4L,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  data_sources = c("TCEQ", "Dallas AQMesh"),
  source_counts = as.list(table(station_index$data_source)),
  station_count = nrow(station_index),
  hourly_composite_rows = sum(station_index$hourly_rows),
  valid_hourly_values = sum(station_index$valid_hours),
  event_count = nrow(event_index),
  first_date = format(min(station_index$first_date), "%Y-%m-%d"),
  last_date = format(max(station_index$last_date), "%Y-%m-%d"),
  preparation_command = "Rscript --vanilla scripts/08i_update_dfw_pm25_dashboard.R",
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
  regional_analysis = list(
    boundary = "NCTCOG 16-county region",
    boundary_source_url = spatial_support$source_url,
    boundary_retrieved_at_utc = spatial_support$retrieved_at_utc,
    analysis_crs = spatial_support$analysis_crs,
    grid_resolution_m = spatial_support$grid_resolution_m,
    idw_power = 2,
    idw_neighbours = 8L,
    idw_radius_m = 50000,
    support_nearest_m = 25000,
    support_minimum_nearby_sites = 3L,
    balanced_participation_minimum_network_sites = 3L,
    hourly_rows = sum(regional_hourly$index$years$rows),
    hourly_files = as.list(regional_hourly_files),
    grid_cells = sum(spatial_support$grid$inside_region),
    validation = spatial_validation,
    files = as.list(regional_files),
    file_sha256 = stats::setNames(as.list(vapply(
      all_regional_files,
      function(path) sha256_file(file.path(cache_dir, path)),
      character(1), USE.NAMES = FALSE
    )), all_regional_files)
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
    regional = as.list(regional_files),
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
