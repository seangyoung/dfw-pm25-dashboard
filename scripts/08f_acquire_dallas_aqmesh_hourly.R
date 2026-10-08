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
  "curl", "data.table", "digest", "dplyr", "jsonlite", "readr", "tibble", "yaml"
))

url_with_query <- function(base, parameters) {
  encoded <- paste(
    names(parameters),
    vapply(parameters, utils::URLencode, character(1), reserved = TRUE),
    sep = "=", collapse = "&"
  )
  paste0(base, "?", encoded)
}

fetch_arcgis <- function(url, attempts = 4L) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    response <- tryCatch(
      curl::curl_fetch_memory(
        url,
        handle = curl::new_handle(
          useragent = "TX-FireHealth Dallas AQMesh reproducible acquisition"
        )
      ),
      error = function(e) e
    )
    if (!inherits(response, "error") && response$status_code == 200L) {
      parsed <- tryCatch(
        jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE),
        error = function(e) e
      )
      if (!inherits(parsed, "error") && is.null(parsed$error)) return(response$content)
      last_error <- if (inherits(parsed, "error")) {
        conditionMessage(parsed)
      } else {
        paste0("ArcGIS error: ", parsed$error$message)
      }
    } else {
      last_error <- if (inherits(response, "error")) {
        conditionMessage(response)
      } else paste("HTTP", response$status_code)
    }
    Sys.sleep(min(2 ^ (attempt - 1L), 8))
  }
  stop("Could not retrieve ArcGIS response after ", attempts, " attempts: ", last_error)
}

fetch_arcgis_post <- function(url, parameters, attempts = 4L) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    handle <- curl::new_handle(
      useragent = "TX-FireHealth Dallas AQMesh reproducible acquisition"
    )
    curl::handle_setform(handle, .list = as.list(parameters))
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = function(e) e
    )
    if (!inherits(response, "error") && response$status_code == 200L) {
      parsed <- tryCatch(
        jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE),
        error = function(e) e
      )
      if (!inherits(parsed, "error") && is.null(parsed$error)) return(response$content)
      last_error <- if (inherits(parsed, "error")) {
        conditionMessage(parsed)
      } else {
        paste0("ArcGIS error: ", parsed$error$message)
      }
    } else {
      last_error <- if (inherits(response, "error")) {
        conditionMessage(response)
      } else paste("HTTP", response$status_code)
    }
    Sys.sleep(min(2 ^ (attempt - 1L), 8))
  }
  stop(
    "Could not retrieve ArcGIS POST response after ", attempts,
    " attempts: ", last_error
  )
}

atomic_write_raw <- function(content, path, gzip = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  connection <- if (gzip) gzfile(temporary, open = "wb") else file(temporary, open = "wb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  writeBin(content, connection)
  close(connection)
  if (!file.rename(temporary, path)) stop("Could not atomically replace ", path)
  invisible(path)
}

query_json <- function(parameters) {
  jsonlite::fromJSON(rawToChar(fetch_arcgis(url_with_query(
    paste0(aqmesh_hourly_layer_url(), "/query"),
    c(parameters, f = "json")
  ))), flatten = TRUE)
}

fetch_object_id_responses <- function(where, object_ids, batch_size = 250L) {
  fetch_batch <- function(requested_ids) {
    # OBJECTID lists can exceed ArcGIS and intermediary URL-length limits.
    # POST keeps the request deterministic without placing the list in the URL.
    content <- fetch_arcgis_post(
      paste0(aqmesh_hourly_layer_url(), "/query"),
      c(
        where = where,
        objectIds = paste(requested_ids, collapse = ","),
        outFields = "*", returnGeometry = "false",
        orderByFields = "OBJECTID ASC", f = "json"
      )
    )
    parsed <- jsonlite::fromJSON(rawToChar(content), flatten = TRUE)
    if (!is.null(parsed$error)) {
      stop("ArcGIS error while retrieving an OBJECTID batch: ", parsed$error$message)
    }
    returned_ids <- if (is.null(parsed$features) || !nrow(parsed$features)) {
      integer()
    } else as.integer(parsed$features[["attributes.OBJECTID"]])
    complete <- length(returned_ids) == length(requested_ids) &&
      identical(sort(returned_ids), sort(as.integer(requested_ids)))
    if (complete) {
      return(list(list(
        content = content, object_ids = returned_ids,
        requested_ids = as.integer(requested_ids)
      )))
    }
    if (length(requested_ids) <= 1L) {
      stop(
        "ArcGIS did not return requested OBJECTID ", requested_ids,
        ". The source layer may have changed during acquisition."
      )
    }
    midpoint <- floor(length(requested_ids) / 2L)
    c(
      fetch_batch(requested_ids[seq_len(midpoint)]),
      fetch_batch(requested_ids[(midpoint + 1L):length(requested_ids)])
    )
  }

  groups <- split(
    as.integer(object_ids),
    ceiling(seq_along(object_ids) / as.integer(batch_size))
  )
  unname(unlist(lapply(groups, fetch_batch), recursive = FALSE))
}

parse_month_argument <- function(args, name, default) {
  match <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(match) > 1L) stop("Specify --", name, " no more than once.")
  value <- if (length(match)) sub(paste0("^--", name, "="), "", match) else default
  if (!grepl("^[0-9]{4}-[0-9]{2}$", value)) {
    stop("--", name, " must use YYYY-MM format.")
  }
  as.Date(paste0(value, "-01"))
}

args <- commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% args
raw_arg <- grep("^--raw-dir=", args, value = TRUE)
if (length(raw_arg) > 1L) stop("Specify --raw-dir no more than once.")
known <- c(
  "--refresh", raw_arg,
  grep("^--start-month=", args, value = TRUE),
  grep("^--end-month=", args, value = TRUE)
)
unknown <- setdiff(args, known)
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

cfg <- read_config()
raw_dir <- if (length(raw_arg)) {
  normalizePath(sub("^--raw-dir=", "", raw_arg), mustWork = FALSE)
} else aqmesh_raw_dir(cfg)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(raw_dir, "pages"), recursive = TRUE, showWarnings = FALSE)

message("Retrieving Dallas AQMesh layer and station metadata...")
layer_content <- fetch_arcgis(url_with_query(aqmesh_hourly_layer_url(), c(f = "pjson")))
station_content <- fetch_arcgis(url_with_query(
  paste0(aqmesh_station_layer_url(), "/query"),
  c(where = "1=1", outFields = "*", returnGeometry = "true", f = "json")
))
atomic_write_raw(layer_content, file.path(raw_dir, "hourly_layer_metadata.json"))
atomic_write_raw(station_content, file.path(raw_dir, "station_snapshot.json"))

first_response <- query_json(c(
  where = "1=1", outFields = "project_time_interval_start",
  orderByFields = "project_time_interval_start ASC", resultRecordCount = "1",
  returnGeometry = "false"
))
last_response <- query_json(c(
  where = "1=1", outFields = "project_time_interval_start",
  orderByFields = "project_time_interval_start DESC", resultRecordCount = "1",
  returnGeometry = "false"
))
first_epoch <- first_response$features[["attributes.project_time_interval_start"]][1]
last_epoch <- last_response$features[["attributes.project_time_interval_start"]][1]
available_first <- as.Date(format(aqmesh_epoch_milliseconds(first_epoch), "%Y-%m-01", tz = "UTC"))
available_last <- as.Date(format(aqmesh_epoch_milliseconds(last_epoch), "%Y-%m-01", tz = "UTC"))
start_month <- parse_month_argument(args, "start-month", format(available_first, "%Y-%m"))
end_month <- parse_month_argument(args, "end-month", format(available_last, "%Y-%m"))
if (start_month < available_first || end_month > available_last || start_month > end_month) {
  stop(
    "Requested months must fall within available coverage ", available_first,
    " through ", available_last, "."
  )
}

months <- seq(start_month, end_month, by = "month")
page_size <- 2000L
object_id_batch_size <- 250L
snapshot_tag <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
manifest_rows <- list()
existing_manifest_path <- file.path(raw_dir, "download_manifest.csv")
existing_manifest <- if (file.exists(existing_manifest_path)) {
  readr::read_csv(existing_manifest_path, show_col_types = FALSE)
} else tibble::tibble()
for (month in months) {
  month <- as.Date(month, origin = "1970-01-01")
  next_month <- seq(month, by = "month", length.out = 2L)[2]
  month_label <- format(month, "%Y-%m")
  where <- sprintf(
    paste0(
      "project_time_interval_start >= TIMESTAMP '%s 00:00:00' AND ",
      "project_time_interval_start < TIMESTAMP '%s 00:00:00'"
    ),
    format(month, "%Y-%m-%d"), format(next_month, "%Y-%m-%d")
  )
  count_response <- query_json(c(where = where, returnCountOnly = "true"))
  count <- as.integer(count_response$count)
  message(month_label, ": ", format(count, big.mark = ","), " source records")
  month_dir <- file.path(raw_dir, "pages", month_label)
  dir.create(month_dir, recursive = TRUE, showWarnings = FALSE)
  existing_month <- if (nrow(existing_manifest)) {
    existing_manifest[existing_manifest$month == month_label, , drop = FALSE]
  } else existing_manifest
  existing_paths <- file.path(raw_dir, existing_month$relative_path)
  existing_hashes_valid <- nrow(existing_month) > 0L &&
    all(file.exists(existing_paths)) &&
    identical(
      unname(vapply(existing_paths, sha256_file, character(1))),
      unname(existing_month$sha256)
    )
  reuse_month <- !refresh && month != available_last &&
    nrow(existing_month) > 0L &&
    sum(existing_month$records) == count && existing_hashes_valid
  if (reuse_month) {
    manifest_rows[[length(manifest_rows) + 1L]] <- existing_month
    next
  }
  if (!count) next

  id_content <- fetch_arcgis(url_with_query(
    paste0(aqmesh_hourly_layer_url(), "/query"),
    c(where = where, returnIdsOnly = "true", returnGeometry = "false", f = "json")
  ))
  id_response <- jsonlite::fromJSON(rawToChar(id_content), simplifyVector = TRUE)
  if (!is.null(id_response$error)) {
    stop("ArcGIS error while retrieving the ", month_label, " OBJECTID snapshot: ",
         id_response$error$message)
  }
  object_ids <- sort(unique(as.integer(id_response$objectIds)))
  object_ids <- object_ids[!is.na(object_ids)]
  if (length(object_ids) != count) {
    message(
      month_label, ": count changed from ", format(count, big.mark = ","),
      " to ", format(length(object_ids), big.mark = ","),
      " while acquiring; using the stable OBJECTID snapshot."
    )
    count <- length(object_ids)
  }
  id_path <- file.path(
    month_dir, paste0("object_ids_snapshot_", snapshot_tag, ".json")
  )
  atomic_write_raw(id_content, id_path)
  responses <- fetch_object_id_responses(
    where, object_ids, batch_size = object_id_batch_size
  )
  returned_ids <- integer()
  offset <- 0L
  for (page_number in seq_along(responses)) {
    response <- responses[[page_number]]
    path <- file.path(
      month_dir,
      sprintf("snapshot_%s_page_%04d.json.gz", snapshot_tag, page_number)
    )
    atomic_write_raw(response$content, path, gzip = TRUE)
    records <- length(response$object_ids)
    returned_ids <- c(returned_ids, response$object_ids)
    manifest_rows[[length(manifest_rows) + 1L]] <- tibble::tibble(
      month = month_label,
      page = page_number,
      result_offset = offset,
      records = records,
      source_month_records = count,
      relative_path = file.path("pages", month_label, basename(path)),
      sha256 = sha256_file(path),
      query_where = where,
      query_strategy = "stable_object_id_snapshot",
      object_id_min = min(response$object_ids),
      object_id_max = max(response$object_ids),
      object_ids_snapshot_path = file.path(
        "pages", month_label, basename(id_path)
      ),
      object_ids_snapshot_sha256 = sha256_file(id_path),
      retrieved_or_validated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
    offset <- offset + records
  }
  if (length(returned_ids) != length(object_ids) ||
      !identical(sort(returned_ids), object_ids) || anyDuplicated(returned_ids)) {
    stop(month_label, " OBJECTID snapshot was not reproduced exactly by page retrieval.")
  }
}

requested_months <- format(months, "%Y-%m")
retained_manifest <- if (nrow(existing_manifest)) {
  existing_manifest |>
    dplyr::filter(!.data$month %in% requested_months)
} else existing_manifest
manifest <- dplyr::bind_rows(retained_manifest, manifest_rows) |>
  dplyr::arrange(.data$month, .data$page)
readr::write_csv(manifest, file.path(raw_dir, "download_manifest.csv"), na = "")
metadata <- list(
  schema_version = 1L,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  hourly_layer_url = aqmesh_hourly_layer_url(),
  station_layer_url = aqmesh_station_layer_url(),
  source_item_url = "https://www.arcgis.com/home/item.html?id=7f7293db60ed4beda4d64b3c848e2a5d",
  first_available_timestamp_utc = format(
    aqmesh_epoch_milliseconds(first_epoch), tz = "UTC", usetz = TRUE
  ),
  last_available_timestamp_utc = format(
    aqmesh_epoch_milliseconds(last_epoch), tz = "UTC", usetz = TRUE
  ),
  selected_first_month = format(start_month, "%Y-%m"),
  selected_last_month = format(end_month, "%Y-%m"),
  page_size = page_size,
  object_id_batch_size = object_id_batch_size,
  pagination_strategy = paste(
    "Historical complete months reuse hash-validated pages.",
    "New or mutable months use a stable OBJECTID snapshot and recursively split",
    "partial ArcGIS responses until every requested OBJECTID is archived exactly once."
  ),
  records = sum(manifest$records),
  files = nrow(manifest),
  download_manifest_sha256 = sha256_file(file.path(raw_dir, "download_manifest.csv")),
  hourly_layer_metadata_sha256 = sha256_file(file.path(raw_dir, "hourly_layer_metadata.json")),
  station_snapshot_sha256 = sha256_file(file.path(raw_dir, "station_snapshot.json"))
)
jsonlite::write_json(
  metadata, file.path(raw_dir, "acquisition_metadata.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
upsert_source_manifest(cfg, data.table::data.table(
  source_id = c("dallas_aqmesh_hourly_arcgis", "dallas_aqmesh_station_arcgis"),
  local_path = c(
    manifest_relative_path(file.path(raw_dir, "download_manifest.csv"), cfg),
    manifest_relative_path(file.path(raw_dir, "station_snapshot.json"), cfg)
  ),
  version_or_date = c(
    paste(metadata$first_available_timestamp_utc, "through",
          metadata$last_available_timestamp_utc),
    metadata$generated_at_utc
  ),
  downloaded_at = metadata$generated_at_utc,
  sha256 = c(
    metadata$download_manifest_sha256,
    metadata$station_snapshot_sha256
  ),
  status = "available",
  notes = c(
    paste("Public City of Dallas AQMesh hourly ArcGIS archive acquired by",
          "scripts/08f_acquire_dallas_aqmesh_hourly.R;", aqmesh_hourly_layer_url()),
    paste("Public City of Dallas AQMesh station layer acquired by",
          "scripts/08f_acquire_dallas_aqmesh_hourly.R;", aqmesh_station_layer_url())
  )
))
message(
  "Dallas AQMesh raw archive ready: ", raw_dir, "\n",
  format(sum(manifest$records), big.mark = ","), " records in ", nrow(manifest),
  " immutable page files."
)
