#!/usr/bin/env Rscript

# Import the TCEQ monthly PM2.5 CSV reports into one tidy tibble per physical
# AQS monitoring location. Source cells remain available in `pm25_raw`; cleaned
# numeric values use NA for negatives and character flags, with the reason kept
# in `value_flag` ("NEG" for negative measurements or the original TCEQ code).

file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_argument)) {
  script_path <- sub("^--file=", "", file_argument[1])
} else {
  source_paths <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  source_paths <- source_paths[!is.na(source_paths)]
  if (!length(source_paths)) stop("Could not determine the importer script path.")
  script_path <- tail(source_paths, 1L)
}
script_dir <- dirname(normalizePath(script_path))
repo_root <- dirname(script_dir)

required_packages <- c("dplyr", "purrr", "readr", "stringr", "tibble", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "))
}

default_data_dir <- file.path(repo_root, "data", "raw", "tceq_dfw_pm25_monthly")

clean_measurement <- function(raw_value) {
  raw_value <- trimws(as.character(raw_value))
  raw_value[is.na(raw_value) | !nzchar(raw_value)] <- NA_character_
  numeric_value <- suppressWarnings(as.numeric(raw_value))
  negative <- !is.na(numeric_value) & numeric_value < 0
  character_flag <- !is.na(raw_value) & is.na(numeric_value)
  tibble::tibble(
    value = ifelse(negative, NA_real_, numeric_value),
    flag = dplyr::case_when(
      negative ~ "NEG",
      character_flag ~ raw_value,
      TRUE ~ NA_character_
    )
  )
}

parse_report_metadata <- function(lines, header_index) {
  preceding <- lines[seq_len(header_index - 1L)]
  cams_line <- tail(grep("^CAMS [0-9]+ Monthly PM-2\\.5", preceding, value = TRUE), 1L)
  site_line <- tail(grep(" - EPA Site: ", preceding, value = TRUE, fixed = TRUE), 1L)
  parameter_line <- tail(grep("^PM-2\\.5 .*\\(POC [0-9]+\\)", preceding, value = TRUE), 1L)
  qa_line <- tail(grep("^Data from this instrument", preceding, value = TRUE), 1L)
  if (!length(cams_line) || !length(site_line) || !length(parameter_line)) {
    stop("Could not parse report metadata before CSV header on line ", header_index, ".")
  }
  tibble::tibble(
    cams_id = stringr::str_match(cams_line, "^CAMS ([0-9]+)")[, 2],
    site_name = stringr::str_match(site_line, "^(.*?)\\s+- EPA Site:")[, 2],
    aqs_site_id = stringr::str_match(site_line, "EPA Site:\\s+([^ ]+)")[, 2],
    poc = as.integer(stringr::str_match(parameter_line, "\\(POC ([0-9]+)\\)")[, 2]),
    parameter_code = if (grepl("Acceptable", parameter_line, fixed = TRUE)) {
      "88502"
    } else {
      "88101"
    },
    parameter_name = trimws(sub("\\s*\\(POC [0-9]+\\).*", "", parameter_line)),
    units = stringr::str_match(parameter_line, " measured in (.+)$")[, 2],
    qa_statement = if (length(qa_line)) qa_line else NA_character_,
    regulatory_qa = if (!length(qa_line)) {
      NA
    } else if (grepl("does not meet", qa_line, fixed = TRUE)) {
      FALSE
    } else if (grepl("meets EPA quality assurance criteria", qa_line, fixed = TRUE)) {
      TRUE
    } else {
      NA
    }
  )
}

report_blocks <- function(lines) {
  headers <- which(startsWith(lines, "Date,"))
  if (!length(headers)) stop("No CSV table header found.")
  purrr::map(seq_along(headers), function(i) {
    start <- headers[i]
    next_header <- if (i < length(headers)) headers[i + 1L] else length(lines) + 1L
    candidate <- seq.int(start + 1L, next_header - 1L)
    data_indices <- candidate[grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4},", lines[candidate])]
    if (!length(data_indices)) return(NULL)
    list(
      metadata = parse_report_metadata(lines, start) |>
        dplyr::mutate(report_block = i),
      csv = paste(c(lines[start], lines[data_indices]), collapse = "\n")
    )
  }) |>
    purrr::compact()
}

read_tceq_pm25_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  blocks <- report_blocks(lines)
  if (!length(blocks)) return(tibble::tibble())

  purrr::map_dfr(blocks, function(block) {
    daily <- readr::read_csv(
      I(block$csv),
      col_types = readr::cols(.default = readr::col_character()),
      na = character(), show_col_types = FALSE, progress = FALSE,
      name_repair = "minimal"
    )
    required <- c("Date", sprintf("%02d:00", 0:23), "Max", "Avg", "STD")
    if (!identical(names(daily), required)) {
      stop("Unexpected columns in ", path, ": ", paste(names(daily), collapse = ", "))
    }

    daily_max <- clean_measurement(daily$Max)
    daily_avg <- clean_measurement(daily$Avg)
    daily_std <- clean_measurement(daily$STD)

    hourly <- daily |>
      dplyr::select(-dplyr::all_of(c("Max", "Avg", "STD"))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(sprintf("%02d:00", 0:23)),
        names_to = "hour_lstd", values_to = "pm25_raw"
      ) |>
      dplyr::mutate(
        date = as.Date(.data$Date, format = "%m/%d/%Y"),
        hour = as.integer(substr(.data$hour_lstd, 1L, 2L)),
        daily_row = match(.data$Date, daily$Date)
      )
    cleaned <- clean_measurement(hourly$pm25_raw)

    dplyr::bind_cols(
      block$metadata[rep(1L, nrow(hourly)), ],
      hourly |>
        dplyr::transmute(
          date = .data$date,
          hour_lstd = .data$hour_lstd,
          hour = .data$hour,
          pm25_raw = .data$pm25_raw
        ),
      tibble::tibble(
        pm25_ug_m3 = cleaned$value,
        value_flag = cleaned$flag,
        daily_max_raw = daily$Max[hourly$daily_row],
        daily_max_ug_m3 = daily_max$value[hourly$daily_row],
        daily_max_flag = daily_max$flag[hourly$daily_row],
        daily_avg_raw = daily$Avg[hourly$daily_row],
        daily_avg_ug_m3 = daily_avg$value[hourly$daily_row],
        daily_avg_flag = daily_avg$flag[hourly$daily_row],
        daily_std_raw = daily$STD[hourly$daily_row],
        daily_std_ug_m3 = daily_std$value[hourly$daily_row],
        daily_std_flag = daily_std$flag[hourly$daily_row],
        source_file = basename(path)
      )
    )
  }) |>
    dplyr::arrange(.data$date, .data$hour, .data$poc)
}

load_tceq_dfw_pm25 <- function(data_dir = default_data_dir) {
  if (!dir.exists(data_dir)) stop("TCEQ data directory does not exist: ", data_dir)
  files <- list.files(
    data_dir,
    pattern = "^cams_[0-9]{4}_[0-9]{4}-[0-9]{2}(_(88101|88502))?_pm25\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  if (!length(files)) stop("No TCEQ PM2.5 monthly CSV files found in ", data_dir)

  all_rows <- purrr::map_dfr(files, read_tceq_pm25_file) |>
    dplyr::arrange(
      .data$aqs_site_id, .data$date, .data$hour, .data$cams_id, .data$poc,
      .data$source_file, .data$report_block
    )

  split(all_rows, all_rows$aqs_site_id) |>
    purrr::map(tibble::as_tibble)
}

# Source this script to create a named list. Each element is exactly one tibble
# for one physical AQS monitoring location, keyed by the TCEQ EPA-site ID.
tceq_dfw_pm25_by_site <- load_tceq_dfw_pm25()

if (sys.nframe() == 0L) {
  row_counts <- vapply(tceq_dfw_pm25_by_site, nrow, integer(1))
  cat(
    "Loaded ", length(tceq_dfw_pm25_by_site), " monitoring-location tibbles (",
    sum(row_counts), " hourly rows).\n", sep = ""
  )
  print(tibble::tibble(aqs_site_id = names(row_counts), hourly_rows = unname(row_counts)))
}
