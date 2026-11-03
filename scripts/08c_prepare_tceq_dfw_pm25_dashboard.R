#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- sub("^--file=", "", script_arg[1])
  script_dir <- dirname(normalizePath(script_path))
  root <- dirname(script_dir)
} else {
  root <- normalizePath(getwd())
}

source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "tceq_pm25_dashboard.R"))
source(file.path(root, "R", "tceq_multi_measurement.R"))
require_packages(c(
  "curl", "digest", "dplyr", "jsonlite", "purrr", "readr", "rvest",
  "stringr", "tibble", "tidyr", "yaml"
))
cfg <- read_config()

args <- commandArgs(trailingOnly = TRUE)
refresh_metadata <- "--refresh-site-metadata" %in% args
cache_arg <- grep("^--cache-dir=", args, value = TRUE)
raw_arg <- grep("^--raw-dir=", args, value = TRUE)
if (length(cache_arg) > 1L || length(raw_arg) > 1L) {
  stop("Specify --cache-dir and --raw-dir no more than once.")
}
known <- c("--refresh-site-metadata", cache_arg, raw_arg)
unknown <- setdiff(args, known)
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

cache_dir <- if (length(cache_arg)) {
  normalizePath(sub("^--cache-dir=", "", cache_arg), mustWork = FALSE)
} else {
  tceq_dashboard_cache_dir(cfg)
}
raw_dir <- if (length(raw_arg)) {
  normalizePath(sub("^--raw-dir=", "", raw_arg), mustWork = TRUE)
} else {
  tceq_dashboard_raw_dir(root)
}

inventory_path <- file.path(raw_dir, "dfw_site_inventory.csv")
raw_manifest_path <- file.path(raw_dir, "download_manifest.csv")
if (!file.exists(inventory_path) || !file.exists(raw_manifest_path)) {
  stop("The raw TCEQ inventory or download manifest is missing from ", raw_dir, ".")
}

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(cache_dir, "sites"), recursive = TRUE, showWarnings = FALSE)

atomic_save_rds <- function(object, path) {
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "gzip")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path, ".")
  invisible(path)
}

atomic_write_csv <- function(object, path) {
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  readr::write_csv(object, temporary, na = "")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path, ".")
  invisible(path)
}

inventory <- readr::read_csv(inventory_path, show_col_types = FALSE)
expected_site_ids <- inventory |>
  dplyr::filter(.data$download_selected %in% TRUE) |>
  dplyr::pull(.data$aqs_site_id) |>
  unique() |>
  sort()
metadata_path <- file.path(cache_dir, "station_metadata.rds")
cached_metadata_matches <- FALSE
if (file.exists(metadata_path) && !refresh_metadata) {
  cached_metadata <- readRDS(metadata_path)
  cached_metadata_matches <- setequal(cached_metadata$aqs_site_id, expected_site_ids)
}
if (file.exists(metadata_path) && !refresh_metadata && cached_metadata_matches) {
  message("Using cached TCEQ station metadata: ", metadata_path)
  station_metadata <- cached_metadata
} else {
  message("Retrieving metadata for ", length(expected_site_ids), " canonical TCEQ monitoring sites...")
  station_metadata <- tceq_build_station_metadata(inventory)
  if (nrow(station_metadata) != length(expected_site_ids) || anyNA(station_metadata$latitude) ||
      anyNA(station_metadata$longitude)) {
    stop("Station metadata validation failed: every supported site requires coordinates.")
  }
  atomic_save_rds(station_metadata, metadata_path)
  atomic_write_csv(station_metadata, file.path(cache_dir, "station_metadata.csv"))
}

message("Importing the validated monthly TCEQ reports...")
import_environment <- new.env(parent = globalenv())
sys.source(
  file.path(root, "scripts", "08b_import_tceq_dfw_pm25_monthly.R"),
  envir = import_environment
)
by_site <- if (identical(
  normalizePath(raw_dir, mustWork = TRUE),
  normalizePath(import_environment$default_data_dir, mustWork = TRUE)
)) {
  import_environment$tceq_dfw_pm25_by_site
} else {
  import_environment$load_tceq_dfw_pm25(raw_dir)
}
if (length(setdiff(names(by_site), station_metadata$aqs_site_id))) {
  stop(
    "Imported PM2.5 site IDs are missing from station metadata: ",
    paste(sort(setdiff(names(by_site), station_metadata$aqs_site_id)), collapse = ", "),
    "."
  )
}
raw_manifest <- readr::read_csv(raw_manifest_path, show_col_types = FALSE)
supplemental_codes <- tceq_supplemental_measurement_catalog()$parameter_code
supplemental_manifest <- raw_manifest |>
  dplyr::filter(
    .data$parameter_code %in% supplemental_codes,
    !is.na(.data$relative_path), nzchar(.data$relative_path)
  )

site_ids <- station_metadata$aqs_site_id
site_summaries <- vector("list", length(site_ids))
daily_indexes <- vector("list", length(site_ids))
event_indexes <- vector("list", length(site_ids))
names(site_summaries) <- names(daily_indexes) <- names(event_indexes) <- site_ids
pm25_raw_template <- by_site[[1]][0, , drop = FALSE]

for (site_id in site_ids) {
  message("Preparing ", site_id, "...")
  raw <- if (site_id %in% names(by_site)) {
    by_site[[site_id]]
  } else pm25_raw_template
  raw <- raw |>
    dplyr::mutate(
      date = as.Date(.data$date),
      datetime_lstd = tceq_lstd_datetime(.data$date, .data$hour),
      raw_series_key = tceq_raw_series_key(
        .data$source_file, .data$report_block, .data$poc
      )
    )
  site_supplemental_files <- supplemental_manifest |>
    dplyr::filter(.data$aqs_site_id == site_id) |>
    dplyr::pull(.data$relative_path) |>
    unique()
  site_supplemental_raw <- load_tceq_dfw_supplemental(
    raw_dir, site_supplemental_files
  )
  site_supplemental_hourly <- tceq_build_supplemental_hourly(
    site_supplemental_raw
  )
  hourly <- if (nrow(raw)) {
    tceq_merge_supplemental_hourly(
      tceq_build_hourly_composite(raw), site_supplemental_hourly
    )
  } else {
    tceq_build_supplemental_only_hourly(site_supplemental_hourly)
  }
  if (!nrow(hourly)) {
    stop("No supported hourly measurements were imported for ", site_id, ".")
  }
  daily <- tceq_build_daily_summary(hourly, raw, minimum_valid_hours = 18L)
  events <- tceq_build_event_index(hourly, daily)
  metadata <- station_metadata |>
    dplyr::filter(.data$aqs_site_id == site_id)
  hourly$site_name <- metadata$site_name[[1]]
  bundle <- list(
    schema_version = 6L,
    metadata = metadata,
    hourly = tibble::as_tibble(hourly),
    daily = tibble::as_tibble(daily),
    events = tibble::as_tibble(events),
    raw_hourly = tibble::as_tibble(raw),
    supplemental_provenance = tceq_supplemental_provenance(
      site_supplemental_raw
    )
  )
  site_path <- file.path(cache_dir, "sites", tceq_site_cache_filename(site_id))
  atomic_save_rds(bundle, site_path)
  daily_indexes[[site_id]] <- daily
  event_indexes[[site_id]] <- events
  site_summaries[[site_id]] <- tibble::tibble(
    aqs_site_id = site_id,
    first_date = min(hourly$date),
    last_date = max(hourly$date),
    hourly_rows = nrow(hourly),
    valid_hours = sum(is.finite(hourly$composite_pm25_ug_m3)),
    valid_pm10_hours = if ("composite_pm10_ug_m3" %in% names(hourly)) {
      sum(is.finite(hourly$composite_pm10_ug_m3))
    } else 0L,
    valid_no2_hours = if ("composite_no2_ppb" %in% names(hourly)) {
      sum(is.finite(hourly$composite_no2_ppb))
    } else 0L,
    valid_o3_hours = if ("composite_o3_ppb" %in% names(hourly)) {
      sum(is.finite(hourly$composite_o3_ppb))
    } else 0L,
    valid_so2_hours = if ("composite_so2_ppb" %in% names(hourly)) {
      sum(is.finite(hourly$composite_so2_ppb))
    } else 0L,
    valid_co_hours = if ("composite_co_ppm" %in% names(hourly)) {
      sum(is.finite(hourly$composite_co_ppm))
    } else 0L,
    valid_temperature_hours = if ("composite_temperature_c" %in% names(hourly)) {
      sum(is.finite(hourly$composite_temperature_c))
    } else 0L,
    valid_humidity_hours = if (
      "composite_relative_humidity_percent" %in% names(hourly)
    ) {
      sum(is.finite(hourly$composite_relative_humidity_percent))
    } else 0L,
    qa_fallback_hours = sum(
      hourly$qa_fallback & is.finite(hourly$composite_pm25_ug_m3)
    ),
    parameter_88502_fallback_hours = sum(
      hourly$parameter_fallback & is.finite(hourly$composite_pm25_ug_m3)
    ),
    short_spike_events = sum(events$event_type == "short_spike"),
    multi_day_events = sum(events$event_type == "multi_day"),
    site_cache_file = file.path("sites", basename(site_path)),
    site_cache_sha256 = sha256_file(site_path)
  )
  rm(
    raw, hourly, daily, events, bundle, site_supplemental_raw,
    site_supplemental_hourly
  )
  invisible(gc())
}

station_index <- station_metadata |>
  dplyr::left_join(dplyr::bind_rows(site_summaries), by = "aqs_site_id") |>
  dplyr::arrange(dplyr::desc(.data$active), .data$site_name)
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
  schema_version = 6L,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  raw_data_dir = normalizePath(raw_dir, winslash = "/"),
  raw_manifest_path = normalizePath(raw_manifest_path, winslash = "/"),
  raw_manifest_sha256 = sha256_file(raw_manifest_path),
  site_inventory_sha256 = sha256_file(inventory_path),
  station_count = nrow(station_index),
  hourly_composite_rows = sum(station_index$hourly_rows),
  valid_hourly_values = sum(station_index$valid_hours),
  valid_supplemental_values = list(
    pm10 = sum(station_index$valid_pm10_hours),
    no2 = sum(station_index$valid_no2_hours),
    o3 = sum(station_index$valid_o3_hours),
    so2 = sum(station_index$valid_so2_hours),
    co = sum(station_index$valid_co_hours),
    temperature = sum(station_index$valid_temperature_hours),
    humidity = sum(station_index$valid_humidity_hours)
  ),
  event_count = nrow(event_index),
  short_spike_events = sum(event_index$event_type == "short_spike"),
  multi_day_events = sum(event_index$event_type == "multi_day"),
  first_date = format(min(station_index$first_date), "%Y-%m-%d"),
  last_date = format(max(station_index$last_date), "%Y-%m-%d"),
  algorithms = list(
    composite = paste(
      "Use finite parameter 88101 values when available, otherwise parameter 88502;",
      "within the selected parameter prefer records explicitly meeting EPA QA criteria,",
      "then take the median of qualifying records"
    ),
    parameter_priority = c("88101", "88502"),
    supplemental_parameters = list(
      pm10 = "81102 (ug/m3)", no2 = "42602 (ppb)", o3 = "44201 (ppb)",
      so2 = "42401 (ppb)", co = "42101 (ppm)",
      temperature = "62101 (degrees F converted to degrees C)",
      humidity = "62201 (percent)"
    ),
    daily_minimum_valid_hours = 18L,
    short_spike = "Strictly greater than 35 ug/m3 for a maximal run of 1-5 consecutive hours",
    multi_day = paste(
      "At least two core days with daily max >=25, median >=15, and IQR >=10 ug/m3;",
      "one eligible trough day may bridge"
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
  "Dashboard cache ready: ", cache_dir, "\n",
  nrow(station_index), " sites; ", format(sum(station_index$hourly_rows), big.mark = ","),
  " site-hours; ", format(nrow(event_index), big.mark = ","), " events."
)
