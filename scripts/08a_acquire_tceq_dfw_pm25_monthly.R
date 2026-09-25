#!/usr/bin/env Rscript

# Acquire monthly, hourly TCEQ PM2.5 reports for every CAMS monitor listed in
# the Dallas-Fort Worth region. The TCEQ "comma-delimited" response is CSV text
# embedded in an HTML <pre> element, so this script extracts that report text
# without changing its values and records a request-level manifest.

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
script_dir <- dirname(normalizePath(script_path))
repo_root <- dirname(script_dir)

required_packages <- c("curl", "data.table", "digest", "xml2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "))
}

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args
inventory_only <- "--inventory-only" %in% args
reuse_inventory <- "--reuse-inventory" %in% args
parameter_arg <- grep("^--parameters=", args, value = TRUE)
parameter_codes <- if (length(parameter_arg)) {
  strsplit(sub("^--parameters=", "", parameter_arg[1]), ",", fixed = TRUE)[[1]]
} else {
  c("88101", "88502")
}
parameter_codes <- unique(trimws(parameter_codes))
supported_parameter_codes <- c("88101", "88502")
if (!length(parameter_codes) || any(!parameter_codes %in% supported_parameter_codes)) {
  stop(
    "--parameters must contain one or both of: ",
    paste(supported_parameter_codes, collapse = ", "), "."
  )
}
delay_arg <- grep("^--delay=", args, value = TRUE)
request_delay <- if (length(delay_arg)) {
  as.numeric(sub("^--delay=", "", delay_arg[1]))
} else {
  0.10
}
if (!is.finite(request_delay) || request_delay < 0) stop("--delay must be non-negative.")

tceq_url <- "https://www.tceq.texas.gov/cgi-bin/compliance/monops/monthly_summary.pl"
output_root <- file.path(repo_root, "data", "raw", "tceq_dfw_pm25_monthly")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

first_of_month <- function(x) as.Date(format(as.Date(x), "%Y-%m-01"))

previous_complete_month <- function(today = Sys.Date()) {
  first_of_month(first_of_month(today) - 1L)
}

encode_form <- function(fields) {
  paste0(
    names(fields), "=",
    vapply(fields, utils::URLencode, character(1), reserved = TRUE),
    collapse = "&"
  )
}

fetch_tceq <- function(fields = NULL, attempts = 4L) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    result <- tryCatch({
      handle <- curl::new_handle(
        useragent = "TX_FireHealth TCEQ PM2.5 research download",
        followlocation = TRUE,
        connecttimeout = 30,
        timeout = 120
      )
      if (!is.null(fields)) {
        curl::handle_setopt(handle, postfields = encode_form(fields))
      }
      response <- curl::curl_fetch_memory(tceq_url, handle = handle)
      if (response$status_code != 200L) {
        stop("HTTP status ", response$status_code)
      }
      rawToChar(response$content)
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(result)) {
      if (request_delay > 0) Sys.sleep(request_delay)
      return(result)
    }
    if (attempt < attempts) Sys.sleep(2^(attempt - 1L))
  }
  stop("TCEQ request failed after ", attempts, " attempts: ", last_error)
}

parse_deactivation_date <- function(label) {
  match <- regexec(
    "Deactivated ([A-Z][a-z]{2} [0-9]{2}, [0-9]{4})", label,
    perl = TRUE
  )
  captured <- regmatches(label, match)[[1]]
  if (length(captured) < 2L) return(as.Date(NA))
  as.Date(captured[2], format = "%b %d, %Y")
}

parse_dfw_sites <- function(html) {
  document <- xml2::read_html(html)
  nodes <- xml2::xml_find_all(document, ".//select[@name='select_site']/option")
  values <- xml2::xml_attr(nodes, "value")
  labels <- trimws(gsub("\u00a0", " ", xml2::xml_text(nodes), fixed = TRUE))
  region_start <- which(values == "region|4|Dallas-Fort Worth")
  if (length(region_start) != 1L) stop("Could not locate the TCEQ Dallas-Fort Worth region.")
  later_regions <- which(seq_along(values) > region_start & startsWith(values, "region|"))
  region_end <- if (length(later_regions)) later_regions[1] - 1L else length(values)
  keep <- seq.int(region_start + 1L, region_end)
  parts <- strsplit(values[keep], "|", fixed = TRUE)
  if (any(lengths(parts) != 4L)) stop("Unexpected TCEQ site-option encoding.")
  data.table::data.table(
    cams_id = vapply(parts, `[[`, character(1), 4L),
    aqs_site_id = vapply(parts, `[[`, character(1), 3L),
    site_name = trimws(vapply(parts, `[[`, character(1), 2L)),
    site_option = values[keep],
    site_label = labels[keep],
    deactivated_date = as.Date(vapply(labels[keep], function(x) {
      value <- parse_deactivation_date(x)
      if (is.na(value)) NA_character_ else as.character(value)
    }, character(1)))
  )
}

month_fields <- function(site_option, month, submitted = "0") {
  month <- first_of_month(month)
  c(
    submitted = submitted,
    first_look = "no",
    select_site = site_option,
    user_month = as.character(as.integer(format(month, "%m")) - 1L),
    user_year = format(month, "%Y")
  )
}

parameter_column <- function(prefix, parameter_code) {
  paste0(prefix, "_", parameter_code)
}

parse_availability <- function(html, parameter_code) {
  document <- xml2::read_html(html)
  input <- xml2::xml_find_first(
    document,
    sprintf(".//input[@name='include%s']", parameter_code)
  )
  if (inherits(input, "xml_missing")) return(NULL)
  label <- trimws(xml2::xml_text(xml2::xml_parent(input)))
  match <- regexec(
    "available from ([A-Z][a-z]{2} [0-9]{4}) to ([A-Z][a-z]{2} [0-9]{4})",
    label, perl = TRUE
  )
  captured <- regmatches(label, match)[[1]]
  if (length(captured) < 3L) stop("Could not parse PM2.5 availability text: ", label)
  list(
    availability_text = label,
    first_month = as.Date(paste0("01 ", captured[2]), format = "%d %b %Y"),
    last_month = as.Date(paste0("01 ", captured[3]), format = "%d %b %Y")
  )
}

discover_site_availability <- function(site, complete_month, parameter_code) {
  anchor <- if (!is.na(site$deactivated_date)) {
    first_of_month(site$deactivated_date)
  } else {
    complete_month
  }
  fallback <- as.Date(c(
    as.character(anchor),
    as.character(first_of_month(anchor - 1L)),
    as.character(first_of_month(anchor - 32L)),
    as.character(first_of_month(anchor - 183L)),
    "2024-08-01", "2020-08-01", "2018-08-01", "2015-08-01",
    "2012-08-01", "2010-08-01", "2007-08-01", "2005-08-01",
    "2000-08-01", "1996-08-01"
  ))
  fallback <- unique(fallback[fallback >= as.Date("1996-01-01") & fallback <= complete_month])
  for (probe_month in fallback) {
    probe_month <- as.Date(probe_month, origin = "1970-01-01")
    html <- fetch_tceq(month_fields(site$site_option, probe_month, submitted = "0"))
    availability <- parse_availability(html, parameter_code)
    if (!is.null(availability)) {
      return(c(availability, list(probe_month = probe_month, discovery_status = "pm25_available")))
    }
  }
  list(
    availability_text = NA_character_, first_month = as.Date(NA),
    last_month = as.Date(NA), probe_month = as.Date(NA),
    discovery_status = "no_pm25_parameter_found"
  )
}

extract_report_text <- function(html) {
  document <- xml2::read_html(html)
  blocks <- xml2::xml_text(xml2::xml_find_all(document, ".//pre"))
  if (!length(blocks)) return(NA_character_)
  report <- paste(blocks, collapse = "\n\n")
  report <- gsub("\\r\\n?", "\n", report)
  if (!grepl("(?m)^Date,00:00,01:00", report, perl = TRUE)) return(NA_character_)
  paste0(sub("\\n*$", "", report), "\n")
}

validate_report <- function(report, expected_month) {
  headers <- grep("^Date,", strsplit(report, "\n", fixed = TRUE)[[1]], value = TRUE)
  expected_header <- paste(
    c("Date", sprintf("%02d:00", 0:23), "Max", "Avg", "STD"),
    collapse = ","
  )
  if (!length(headers) || any(headers != expected_header)) {
    stop("Unexpected CSV header for ", format(expected_month, "%Y-%m"), ".")
  }
  invisible(TRUE)
}

report_fields <- function(site_option, month, parameter_code) {
  parameter_field <- stats::setNames("1", paste0("include", parameter_code))
  c(
    month_fields(site_option, month, submitted = "1"),
    parameter_field,
    time_format = "24hr",
    report_format = "comma",
    print_daily_max = "1",
    print_daily_avg = "1",
    print_daily_std = "1"
  )
}

write_csv_atomic <- function(text, path) {
  temporary <- tempfile(tmpdir = dirname(path), fileext = ".csv.part")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) stop("Could not move completed file to ", path)
  invisible(path)
}

count_data_rows <- function(report) {
  sum(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4},", strsplit(report, "\n", fixed = TRUE)[[1]]))
}

complete_month <- previous_complete_month()
message("Most recent complete calendar month: ", format(complete_month, "%Y-%m"))

inventory_path <- file.path(output_root, "dfw_site_inventory.csv")
if (reuse_inventory && file.exists(inventory_path)) {
  message("Reusing existing TCEQ site inventory: ", inventory_path)
  sites <- data.table::fread(inventory_path, colClasses = list(character = "cams_id"))
  date_columns <- intersect(
    c("deactivated_date", "first_month", "tceq_last_month", "download_last_month", "probe_month"),
    names(sites)
  )
  for (column in date_columns) sites[, (column) := as.Date(get(column))]
} else {
  sites <- parse_dfw_sites(fetch_tceq())
}

# Preserve the original 88101 inventory columns while adding explicit columns
# for both PM2.5 parameter streams. This makes an existing inventory reusable
# without hiding which parameter was discovered at a CAMS alias.
legacy_columns <- c(
  availability_text = "availability_text", first_month = "first_month",
  tceq_last_month = "tceq_last_month", download_last_month = "download_last_month",
  probe_month = "probe_month", discovery_status = "discovery_status"
)
for (parameter_code in supported_parameter_codes) {
  for (prefix in names(legacy_columns)) {
    column <- parameter_column(prefix, parameter_code)
    if (!column %in% names(sites)) {
      if (parameter_code == "88101" && legacy_columns[[prefix]] %in% names(sites)) {
        sites[, (column) := get(legacy_columns[[prefix]])]
      } else if (prefix %in% c("first_month", "tceq_last_month", "download_last_month", "probe_month")) {
        sites[, (column) := as.Date(NA)]
      } else {
        sites[, (column) := NA_character_]
      }
    }
  }
}

for (parameter_code in parameter_codes) {
  status_column <- parameter_column("discovery_status", parameter_code)
  unresolved <- which(is.na(sites[[status_column]]) | !nzchar(sites[[status_column]]))
  if (length(unresolved)) {
    message(
      "Checking parameter ", parameter_code, " availability for ",
      length(unresolved), " unresolved DFW CAMS entries..."
    )
    for (position in seq_along(unresolved)) {
      i <- unresolved[position]
      availability <- discover_site_availability(
        sites[i], complete_month, parameter_code
      )
      sites[i, (parameter_column("availability_text", parameter_code)) := availability$availability_text]
      sites[i, (parameter_column("first_month", parameter_code)) := availability$first_month]
      sites[i, (parameter_column("tceq_last_month", parameter_code)) := availability$last_month]
      sites[i, (parameter_column("download_last_month", parameter_code)) := if (!is.na(availability$last_month)) {
        min(availability$last_month, complete_month)
      } else as.Date(NA)]
      sites[i, (parameter_column("probe_month", parameter_code)) := availability$probe_month]
      sites[i, (status_column) := availability$discovery_status]
      if (position %% 10L == 0L || position == length(unresolved)) {
        message("  parameter ", parameter_code, " availability ", position, "/", length(unresolved))
      }
    }
  }
}

eligible_by_parameter <- lapply(supported_parameter_codes, function(parameter_code) {
  first_column <- parameter_column("first_month", parameter_code)
  last_column <- parameter_column("download_last_month", parameter_code)
  status_column <- parameter_column("discovery_status", parameter_code)
  result <- sites[
    get(status_column) == "pm25_available" & get(first_column) <= get(last_column)
  ]
  result[, `:=`(
    parameter_code = parameter_code,
    parameter_first_month = get(first_column),
    parameter_last_month = get(last_column),
    cams_numeric = as.integer(cams_id)
  )]
  result
})
eligible_cams <- data.table::rbindlist(eligible_by_parameter, fill = TRUE)
data.table::setorder(eligible_cams, parameter_code, aqs_site_id, cams_numeric)

# A physical station can appear several times in the CAMS menu under historical
# C/A/X instrument aliases. Those aliases return duplicate PM2.5 tables. Select
# the lowest-numbered CAMS entry once per EPA/AQS site and retain all aliases in
# the inventory for auditability.
all_streams <- eligible_cams[, .SD[1L], by = c("parameter_code", "aqs_site_id")]
pm_streams <- all_streams[parameter_code %in% parameter_codes]
for (code in supported_parameter_codes) {
  chosen <- all_streams[get("parameter_code") == code]
  canonical <- setNames(chosen$cams_id, chosen$aqs_site_id)
  sites[, (parameter_column("canonical_cams_id", code)) := unname(canonical[aqs_site_id])]
  sites[, (parameter_column("download_selected", code)) :=
          !is.na(get(parameter_column("canonical_cams_id", code))) &
          cams_id == get(parameter_column("canonical_cams_id", code))]
}
physical_sites <- all_streams[, .SD[which.min(as.integer(cams_id))], by = aqs_site_id]
canonical_cams <- setNames(physical_sites$cams_id, physical_sites$aqs_site_id)
sites[, canonical_cams_id := unname(canonical_cams[aqs_site_id])]
sites[, download_selected := !is.na(canonical_cams_id) & cams_id == canonical_cams_id]

# Keep the legacy columns as the union coverage used by metadata/cache code.
sites[, first_month := as.Date(pmin(
  get(parameter_column("first_month", "88101")),
  get(parameter_column("first_month", "88502")), na.rm = TRUE
), origin = "1970-01-01")]
sites[!is.finite(as.numeric(first_month)), first_month := as.Date(NA)]
sites[, download_last_month := as.Date(pmax(
  get(parameter_column("download_last_month", "88101")),
  get(parameter_column("download_last_month", "88502")), na.rm = TRUE
), origin = "1970-01-01")]
sites[!is.finite(as.numeric(download_last_month)), download_last_month := as.Date(NA)]

data.table::fwrite(sites, inventory_path, na = "")
message(
  "Found ", data.table::uniqueN(all_streams$aqs_site_id),
  " physical DFW PM2.5 monitoring locations; processing ",
  nrow(pm_streams), " of ", nrow(all_streams), " site-parameter streams (",
  nrow(eligible_cams), " eligible CAMS aliases)."
)

if (inventory_only) {
  message("Inventory-only run complete: ", inventory_path)
  quit(save = "no", status = 0L)
}

manifest_rows <- list()
row_index <- 0L
download_count <- 0L
skip_count <- 0L
no_data_count <- 0L
error_count <- 0L
manifest_path <- file.path(output_root, "download_manifest.csv")
prior_manifest <- if (file.exists(manifest_path)) {
  data.table::fread(manifest_path, colClasses = list(character = c("cams_id", "parameter_code")))
} else {
  data.table::data.table()
}

for (site_index in seq_len(nrow(pm_streams))) {
  site <- pm_streams[site_index]
  parameter_code <- site$parameter_code
  months <- seq(site$parameter_first_month, site$parameter_last_month, by = "month")
  site_dir <- file.path(output_root, sprintf("cams_%04d", as.integer(site$cams_id)))
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
  message(
    "CAMS ", site$cams_id, " (", site$site_name, "): ",
    format(min(months), "%Y-%m"), " through ", format(max(months), "%Y-%m"),
    " [parameter ", parameter_code, "; ", length(months), " months]"
  )

  for (month in months) {
    month <- as.Date(month, origin = "1970-01-01")
    coded_relative_path <- file.path(
      sprintf("cams_%04d", as.integer(site$cams_id)),
      sprintf(
        "cams_%04d_%s_%s_pm25.csv", as.integer(site$cams_id),
        format(month, "%Y-%m"), parameter_code
      )
    )
    legacy_relative_path <- file.path(
      sprintf("cams_%04d", as.integer(site$cams_id)),
      sprintf("cams_%04d_%s_pm25.csv", as.integer(site$cams_id), format(month, "%Y-%m"))
    )
    relative_path <- if (
      parameter_code == "88101" &&
      file.exists(file.path(output_root, legacy_relative_path))
    ) legacy_relative_path else coded_relative_path
    output_path <- file.path(output_root, relative_path)
    status <- "downloaded"
    error_message <- NA_character_
    report <- NA_character_
    prior_no_data <- nrow(prior_manifest) && any(
      prior_manifest$cams_id == as.character(site$cams_id) &
        prior_manifest$aqs_site_id == site$aqs_site_id &
        prior_manifest$report_month == format(month, "%Y-%m") &
        prior_manifest$parameter_code == parameter_code &
        prior_manifest$status %in% c("no_data", "known_no_data")
    )

    if (file.exists(output_path) && !overwrite) {
      status <- "existing"
      skip_count <- skip_count + 1L
    } else if (prior_no_data && !overwrite) {
      status <- "known_no_data"
      no_data_count <- no_data_count + 1L
    } else {
      html <- tryCatch(
        fetch_tceq(report_fields(site$site_option, month, parameter_code)),
        error = function(e) {
          error_message <<- conditionMessage(e)
          NA_character_
        }
      )
      if (is.na(html)) {
        status <- "request_error"
        error_count <- error_count + 1L
      } else {
        report <- extract_report_text(html)
        if (is.na(report)) {
          status <- "no_data"
          no_data_count <- no_data_count + 1L
        } else {
          validation_error <- tryCatch({
            validate_report(report, month)
            NA_character_
          }, error = conditionMessage)
          if (!is.na(validation_error)) {
            status <- "validation_error"
            error_message <- validation_error
            error_count <- error_count + 1L
          } else {
            write_csv_atomic(report, output_path)
            download_count <- download_count + 1L
          }
        }
      }
    }

    current_report <- if (file.exists(output_path)) {
      paste(readLines(output_path, warn = FALSE), collapse = "\n")
    } else {
      report
    }
    row_index <- row_index + 1L
    manifest_rows[[row_index]] <- data.table::data.table(
      cams_id = site$cams_id,
      aqs_site_id = site$aqs_site_id,
      site_name = site$site_name,
      report_month = format(month, "%Y-%m"),
      parameter_code = parameter_code,
      time_format = "24hr",
      daily_statistics = "maximum|average|standard_deviation",
      relative_path = if (file.exists(output_path)) relative_path else NA_character_,
      source_url = tceq_url,
      retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      sha256 = if (file.exists(output_path)) {
        digest::digest(file = output_path, algo = "sha256", serialize = FALSE)
      } else {
        NA_character_
      },
      bytes = if (file.exists(output_path)) file.info(output_path)$size else NA_real_,
      data_rows = if (!is.na(current_report)) count_data_rows(current_report) else 0L,
      status = status,
      error_message = error_message
    )
    if (row_index %% 50L == 0L) message("  processed ", row_index, " monthly requests")
  }
}

manifest <- data.table::rbindlist(manifest_rows, fill = TRUE)
if (nrow(prior_manifest) && !setequal(parameter_codes, supported_parameter_codes)) {
  manifest <- data.table::rbindlist(list(
    prior_manifest[!parameter_code %in% parameter_codes], manifest
  ), fill = TRUE)
}
data.table::setorder(manifest, parameter_code, aqs_site_id, report_month)
data.table::fwrite(manifest, manifest_path, na = "")

summary <- data.table::data.table(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  latest_complete_month = format(complete_month, "%Y-%m"),
  dfw_cams_entries = nrow(sites),
  pm25_cams_menu_entries = data.table::uniqueN(eligible_cams[, .(cams_id, aqs_site_id)]),
  pm25_monitoring_locations = data.table::uniqueN(all_streams$aqs_site_id),
  pm25_site_parameter_streams = nrow(all_streams),
  monthly_requests = nrow(manifest),
  downloaded = download_count,
  existing = skip_count,
  no_data = no_data_count,
  errors = error_count
)
data.table::fwrite(summary, file.path(output_root, "download_summary.csv"), na = "")

message(
  "Finished. downloaded=", download_count,
  ", existing=", skip_count,
  ", no_data=", no_data_count,
  ", errors=", error_count,
  ". Manifest: ", manifest_path
)
if (error_count > 0L) quit(save = "no", status = 2L)
