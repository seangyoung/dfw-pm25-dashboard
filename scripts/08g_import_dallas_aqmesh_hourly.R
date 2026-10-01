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
require_packages(c("digest", "dplyr", "jsonlite", "purrr", "readr", "tibble", "yaml"))

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "gzip")
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path)
  invisible(path)
}

args <- commandArgs(trailingOnly = TRUE)
raw_arg <- grep("^--raw-dir=", args, value = TRUE)
output_arg <- grep("^--output-dir=", args, value = TRUE)
if (length(raw_arg) > 1L || length(output_arg) > 1L) {
  stop("Specify --raw-dir and --output-dir no more than once.")
}
unknown <- setdiff(args, c(raw_arg, output_arg))
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

cfg <- read_config()
raw_dir <- if (length(raw_arg)) {
  normalizePath(sub("^--raw-dir=", "", raw_arg), mustWork = TRUE)
} else aqmesh_raw_dir(cfg)
output_dir <- if (length(output_arg)) {
  normalizePath(sub("^--output-dir=", "", output_arg), mustWork = FALSE)
} else aqmesh_import_dir(cfg)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "monthly"), recursive = TRUE, showWarnings = FALSE)

manifest_path <- file.path(raw_dir, "download_manifest.csv")
station_path <- file.path(raw_dir, "station_snapshot.json")
if (!file.exists(manifest_path) || !file.exists(station_path)) {
  stop(
    "Dallas raw archive is incomplete. Run scripts/08f_acquire_dallas_aqmesh_hourly.R first."
  )
}
download_manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
tceq_assert_columns(
  download_manifest,
  c("month", "page", "records", "relative_path", "sha256"),
  "Dallas AQMesh download manifest"
)
page_paths <- file.path(raw_dir, download_manifest$relative_path)
if (!all(file.exists(page_paths))) {
  stop("Raw page files are missing: ", paste(page_paths[!file.exists(page_paths)], collapse = ", "))
}
actual_hashes <- vapply(page_paths, sha256_file, character(1))
if (!identical(unname(actual_hashes), unname(download_manifest$sha256))) {
  stop("One or more Dallas AQMesh raw page hashes do not match the manifest.")
}

station_features <- aqmesh_parse_features(station_path)
stations <- aqmesh_station_metadata(station_features)
atomic_save_rds(stations, file.path(output_dir, "station_metadata.rds"))
readr::write_csv(stations, file.path(output_dir, "station_metadata.csv"), na = "")

monthly_paths <- character()
monthly_rows <- list()
for (month in unique(download_manifest$month)) {
  selected <- download_manifest$month == month
  paths <- page_paths[selected]
  message("Importing Dallas AQMesh ", month, " (", length(paths), " raw pages)...")
  raw <- purrr::map_dfr(paths, function(path) aqmesh_parse_features(gzfile(path)))
  expected <- sum(download_manifest$records[selected])
  if (nrow(raw) != expected) {
    stop(month, " import produced ", nrow(raw), " records; expected ", expected, ".")
  }
  latest <- aqmesh_latest_revisions(raw)
  clean <- aqmesh_clean_latest_records(latest)
  output_path <- file.path(output_dir, "monthly", paste0(month, ".rds"))
  atomic_save_rds(clean, output_path)
  monthly_paths <- c(monthly_paths, output_path)
  monthly_rows[[month]] <- tibble::tibble(
    month = month,
    raw_revision_rows = nrow(raw),
    latest_revision_rows = nrow(clean),
    superseded_revision_rows = nrow(raw) - nrow(clean),
    valid_scaled_pm25_rows = sum(is.finite(clean$pm25_ug_m3)),
    flagged_or_missing_pm25_rows = sum(!is.finite(clean$pm25_ug_m3)),
    field_site_rows = sum(clean$location_class == "field_site"),
    characterisation_rows = sum(clean$location_class == "characterisation"),
    commissioning_rows = sum(clean$location_class == "commissioning"),
    output_file = file.path("monthly", basename(output_path)),
    output_sha256 = sha256_file(output_path)
  )
  rm(raw, latest, clean)
  invisible(gc())
}

message("Combining latest revisions for downstream preparation...")
latest_records <- purrr::map_dfr(monthly_paths, readRDS) |>
  dplyr::arrange(.data$datetime_utc, .data$canonical_location_name, .data$pod_serial_number)
latest_path <- file.path(output_dir, "latest_records.rds")
atomic_save_rds(latest_records, latest_path)

location_audit <- latest_records |>
  dplyr::group_by(
    .data$raw_location_name, .data$canonical_location_name, .data$location_class
  ) |>
  dplyr::summarise(
    first_timestamp_utc = min(.data$datetime_utc),
    last_timestamp_utc = max(.data$datetime_utc),
    pod_serial_numbers = tceq_collapse_values(.data$pod_serial_number),
    records = dplyr::n(),
    valid_scaled_pm25_records = sum(is.finite(.data$pm25_ug_m3)),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$location_class, .data$canonical_location_name)
readr::write_csv(location_audit, file.path(output_dir, "location_alias_audit.csv"), na = "")

monthly_manifest <- dplyr::bind_rows(monthly_rows)
readr::write_csv(monthly_manifest, file.path(output_dir, "import_manifest.csv"), na = "")
metadata <- list(
  schema_version = 1L,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  raw_dir = normalizePath(raw_dir, winslash = "/"),
  raw_download_manifest_sha256 = sha256_file(manifest_path),
  station_snapshot_sha256 = sha256_file(station_path),
  raw_revision_rows = sum(monthly_manifest$raw_revision_rows),
  latest_revision_rows = nrow(latest_records),
  superseded_revision_rows = sum(monthly_manifest$superseded_revision_rows),
  field_site_rows = sum(latest_records$location_class == "field_site"),
  excluded_nonfield_rows = sum(latest_records$location_class != "field_site"),
  first_timestamp_utc = format(min(latest_records$datetime_utc), tz = "UTC", usetz = TRUE),
  last_timestamp_utc = format(max(latest_records$datetime_utc), tz = "UTC", usetz = TRUE),
  revision_rule = paste(
    "Retain the maximum append_date, then maximum OBJECTID, within",
    "location_name x pod_serial_number x project_time_interval_start"
  ),
  analytical_value = "pm2_5_scaled",
  invalid_value_rules = list(
    sentinel = "Values <= -900 are flagged SENTINEL and set to NA",
    negative = "Other negative scaled PM2.5 values are flagged NEG and set to NA",
    excluded_reading_status = c("Deliq", "Deliquescence", "Other Fault Zero")
  ),
  nonfield_policy = paste(
    "Characterisation, Pod commissioning, and missing-location records remain",
    "in latest_records.rds but are excluded from field-site dashboard composites"
  ),
  latest_records_file = basename(latest_path),
  latest_records_sha256 = sha256_file(latest_path)
)
jsonlite::write_json(
  metadata, file.path(output_dir, "import_metadata.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
message(
  "Dallas AQMesh import ready: ", output_dir, "\n",
  format(nrow(latest_records), big.mark = ","), " latest records retained; ",
  format(sum(monthly_manifest$superseded_revision_rows), big.mark = ","),
  " superseded revisions preserved in the raw page archive."
)
