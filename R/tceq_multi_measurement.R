# Import and combine the non-PM2.5 measurements used by the dashboard from the
# official TCEQ monthly-summary reports. Raw report cells and QA statements are
# retained; only the named normalized columns are used in dashboard summaries.

tceq_supplemental_measurement_catalog <- function() {
  tibble::tribble(
    ~parameter_code, ~key, ~parameter_label, ~native_unit, ~display_unit,
    ~value_column, ~flag_column, ~count_column,
    "81102", "pm10", "PM10", "ug/cu meter (25 c)", "ug/m3",
    "composite_pm10_ug_m3", "pm10_flag_summary", "pm10_contributing_records",
    "42602", "no2", "Nitrogen dioxide (NO2)", "parts per billion", "ppb",
    "composite_no2_ppb", "no2_flag_summary", "no2_contributing_records",
    "44201", "o3", "Ozone (O3)", "parts per billion", "ppb",
    "composite_o3_ppb", "o3_flag_summary", "o3_contributing_records",
    "42401", "so2", "Sulfur dioxide (SO2)", "parts per billion", "ppb",
    "composite_so2_ppb", "so2_flag_summary", "so2_contributing_records",
    "42101", "co", "Carbon monoxide (CO)", "parts per million", "ppm",
    "composite_co_ppm", "co_flag_summary", "co_contributing_records",
    "62101", "temperature", "Outdoor temperature", "degrees Fahrenheit", "degrees C",
    "composite_temperature_c", "temperature_flag_summary",
    "temperature_contributing_records",
    "62201", "humidity", "Relative humidity", "percent", "percent",
    "composite_relative_humidity_percent", "humidity_flag_summary",
    "humidity_contributing_records"
  )
}

tceq_supplemental_parameter_from_path <- function(path) {
  code <- stringr::str_match(
    basename(path), "_([0-9]{5})(?:_pm25)?\\.csv$"
  )[, 2]
  supported <- tceq_supplemental_measurement_catalog()$parameter_code
  if (is.na(code) || !code %in% supported) {
    stop("Could not identify a supported TCEQ parameter code from ", basename(path), ".")
  }
  code
}

tceq_clean_supplemental_measurement <- function(
    raw_value, parameter_code, temperature_difference = FALSE) {
  raw_value <- trimws(as.character(raw_value))
  raw_value[is.na(raw_value) | !nzchar(raw_value)] <- NA_character_
  numeric_value <- suppressWarnings(as.numeric(raw_value))
  negative_invalid <- !is.na(numeric_value) & numeric_value < 0 &
    parameter_code != "62101"
  character_flag <- !is.na(raw_value) & is.na(numeric_value)
  native_value <- ifelse(negative_invalid, NA_real_, numeric_value)
  normalized_unbounded <- if (parameter_code == "62101") {
    if (temperature_difference) native_value * 5 / 9 else (native_value - 32) * 5 / 9
  } else native_value
  bounds <- switch(
    parameter_code,
    "81102" = c(0, 5000),
    "42602" = c(0, 1000),
    "44201" = c(0, 1000),
    "42401" = c(0, 10000),
    "42101" = c(0, 100),
    "62101" = if (temperature_difference) c(0, 90) else c(-30, 60),
    "62201" = c(0, 100),
    c(-Inf, Inf)
  )
  out_of_range <- is.finite(normalized_unbounded) &
    (normalized_unbounded < bounds[1] | normalized_unbounded > bounds[2])
  normalized_value <- ifelse(out_of_range, NA_real_, normalized_unbounded)
  tibble::tibble(
    native_value = native_value,
    normalized_value = normalized_value,
    flag = dplyr::case_when(
      negative_invalid ~ "NEG",
      character_flag ~ raw_value,
      out_of_range ~ "OUT_OF_RANGE",
      TRUE ~ NA_character_
    )
  )
}

tceq_parse_supplemental_metadata <- function(lines, header_index, parameter_code) {
  preceding <- lines[seq_len(header_index - 1L)]
  cams_line <- tail(grep("^CAMS [0-9]+ Monthly ", preceding, value = TRUE), 1L)
  site_line <- tail(grep(" - EPA Site: ", preceding, value = TRUE, fixed = TRUE), 1L)
  parameter_line <- tail(grep("\\(POC [0-9]+\\) measured in ", preceding, value = TRUE), 1L)
  qa_line <- tail(grep("^Data from this instrument", preceding, value = TRUE), 1L)
  if (!length(cams_line) || !length(site_line) || !length(parameter_line)) {
    stop("Could not parse report metadata before CSV header on line ", header_index, ".")
  }
  catalog <- tceq_supplemental_measurement_catalog()
  expected <- catalog[catalog$parameter_code == parameter_code, , drop = FALSE]
  units <- stringr::str_match(parameter_line, " measured in (.+)$")[, 2]
  if (!grepl(expected$native_unit[[1]], units, fixed = TRUE)) {
    stop(
      "Unexpected units for TCEQ parameter ", parameter_code, ": ", units,
      "; expected text containing ", expected$native_unit[[1]], "."
    )
  }
  tibble::tibble(
    cams_id = stringr::str_match(cams_line, "^CAMS ([0-9]+)")[, 2],
    site_name = stringr::str_match(site_line, "^(.*?)\\s+- EPA Site:")[, 2],
    aqs_site_id = stringr::str_match(site_line, "EPA Site:\\s+([^ ]+)")[, 2],
    poc = as.integer(stringr::str_match(parameter_line, "\\(POC ([0-9]+)\\)")[, 2]),
    parameter_code = parameter_code,
    measurement_key = expected$key[[1]],
    parameter_name = trimws(sub("\\s*\\(POC [0-9]+\\).*", "", parameter_line)),
    units = units,
    normalized_units = expected$display_unit[[1]],
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

tceq_supplemental_report_blocks <- function(lines, parameter_code) {
  headers <- which(startsWith(lines, "Date,"))
  if (!length(headers)) stop("No CSV table header found.")
  purrr::map(seq_along(headers), function(i) {
    start <- headers[i]
    next_header <- if (i < length(headers)) headers[i + 1L] else length(lines) + 1L
    candidate <- seq.int(start + 1L, next_header - 1L)
    data_indices <- candidate[grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4},", lines[candidate])]
    if (!length(data_indices)) return(NULL)
    list(
      metadata = tceq_parse_supplemental_metadata(
        lines, start, parameter_code
      ) |>
        dplyr::mutate(report_block = i),
      csv = paste(c(lines[start], lines[data_indices]), collapse = "\n")
    )
  }) |>
    purrr::compact()
}

read_tceq_supplemental_file <- function(path) {
  parameter_code <- tceq_supplemental_parameter_from_path(path)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  blocks <- tceq_supplemental_report_blocks(lines, parameter_code)
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
    hourly <- daily |>
      dplyr::select(-dplyr::all_of(c("Max", "Avg", "STD"))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(sprintf("%02d:00", 0:23)),
        names_to = "hour_lstd", values_to = "measurement_raw"
      ) |>
      dplyr::mutate(
        date = as.Date(.data$Date, format = "%m/%d/%Y"),
        hour = as.integer(substr(.data$hour_lstd, 1L, 2L)),
        daily_row = match(.data$Date, daily$Date)
      )
    cleaned <- tceq_clean_supplemental_measurement(
      hourly$measurement_raw, parameter_code
    )
    daily_max <- tceq_clean_supplemental_measurement(daily$Max, parameter_code)
    daily_avg <- tceq_clean_supplemental_measurement(daily$Avg, parameter_code)
    daily_std <- tceq_clean_supplemental_measurement(
      daily$STD, parameter_code, temperature_difference = TRUE
    )
    dplyr::bind_cols(
      block$metadata[rep(1L, nrow(hourly)), ],
      hourly |>
        dplyr::transmute(
          date = .data$date, hour_lstd = .data$hour_lstd, hour = .data$hour,
          measurement_raw = .data$measurement_raw
        ),
      tibble::tibble(
        measurement_native = cleaned$native_value,
        measurement_value = cleaned$normalized_value,
        value_flag = cleaned$flag,
        daily_max_raw = daily$Max[hourly$daily_row],
        daily_max_value = daily_max$normalized_value[hourly$daily_row],
        daily_max_flag = daily_max$flag[hourly$daily_row],
        daily_avg_raw = daily$Avg[hourly$daily_row],
        daily_avg_value = daily_avg$normalized_value[hourly$daily_row],
        daily_avg_flag = daily_avg$flag[hourly$daily_row],
        daily_std_raw = daily$STD[hourly$daily_row],
        daily_std_value = daily_std$normalized_value[hourly$daily_row],
        daily_std_flag = daily_std$flag[hourly$daily_row],
        source_file = basename(path)
      )
    )
  }) |>
    dplyr::arrange(.data$date, .data$hour, .data$parameter_code, .data$poc)
}

load_tceq_dfw_supplemental <- function(data_dir, files = NULL) {
  codes <- paste(tceq_supplemental_measurement_catalog()$parameter_code, collapse = "|")
  if (is.null(files)) {
    files <- list.files(
      data_dir,
      pattern = paste0("^cams_[0-9]{4}_[0-9]{4}-[0-9]{2}_(", codes, ")\\.csv$"),
      recursive = TRUE, full.names = TRUE
    )
  } else {
    files <- as.character(files)
    if (!length(files)) return(tibble::tibble())
    files <- ifelse(
      grepl("^(/|[A-Za-z]:[/\\\\])", files), files, file.path(data_dir, files)
    )
    missing <- files[!file.exists(files)]
    if (length(missing)) {
      stop(
        "The TCEQ supplemental manifest references missing files: ",
        paste(basename(missing), collapse = ", "), "."
      )
    }
  }
  if (!length(files)) return(tibble::tibble())
  purrr::map_dfr(files, read_tceq_supplemental_file) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      datetime_lstd = tceq_lstd_datetime(.data$date, .data$hour),
      raw_series_key = tceq_raw_series_key(
        .data$source_file, .data$report_block, .data$poc
      )
    ) |>
    dplyr::arrange(
      .data$aqs_site_id, .data$date, .data$hour, .data$parameter_code,
      .data$poc, .data$source_file, .data$report_block
    )
}

tceq_supplemental_provenance <- function(raw) {
  if (!nrow(raw)) return(tibble::tibble())
  raw |>
    dplyr::group_by(
      .data$parameter_code, .data$measurement_key, .data$parameter_name,
      .data$units, .data$normalized_units, .data$poc, .data$regulatory_qa,
      .data$qa_statement
    ) |>
    dplyr::summarise(
      first_date = min(.data$date), last_date = max(.data$date),
      source_files = dplyr::n_distinct(.data$source_file),
      raw_cells = dplyr::n(),
      valid_cells = sum(is.finite(.data$measurement_value)),
      flagged_cells = sum(!is.na(.data$value_flag) & nzchar(.data$value_flag)),
      flag_codes = tceq_collapse_values(.data$value_flag),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$parameter_code, .data$poc, .data$first_date)
}

tceq_build_supplemental_hourly <- function(raw) {
  if (!nrow(raw)) return(tibble::tibble())
  required <- c(
    "aqs_site_id", "site_name", "date", "hour", "hour_lstd", "datetime_lstd",
    "parameter_code", "measurement_value", "value_flag", "regulatory_qa"
  )
  tceq_assert_columns(raw, required, "Imported supplemental TCEQ data")
  catalog <- tceq_supplemental_measurement_catalog()
  keys <- c("aqs_site_id", "site_name", "date", "hour", "hour_lstd", "datetime_lstd")
  pieces <- lapply(seq_len(nrow(catalog)), function(i) {
    spec <- catalog[i, , drop = FALSE]
    x <- raw[raw$parameter_code == spec$parameter_code[[1]], , drop = FALSE]
    if (!nrow(x)) return(NULL)
    status <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(
        flag_summary = tceq_collapse_values(.data$value_flag),
        has_qa_value = any(is.finite(.data$measurement_value) & .data$regulatory_qa %in% TRUE),
        has_nonqa_value = any(is.finite(.data$measurement_value) & .data$regulatory_qa %in% FALSE),
        .groups = "drop"
      )
    selected <- x |>
      dplyr::left_join(
        status |> dplyr::select(dplyr::all_of(c(keys, "has_qa_value"))),
        by = keys
      ) |>
      dplyr::filter(
        is.finite(.data$measurement_value),
        !.data$has_qa_value | .data$regulatory_qa %in% TRUE
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(
        composite_value = stats::median(.data$measurement_value),
        contributing_records = dplyr::n(),
        .groups = "drop"
      )
    result <- status |>
      dplyr::left_join(selected, by = keys) |>
      dplyr::mutate(
        qa_fallback = is.finite(.data$composite_value) &
          !.data$has_qa_value & .data$has_nonqa_value,
        qa_status_unavailable = is.finite(.data$composite_value) &
          !.data$has_qa_value & !.data$has_nonqa_value
      ) |>
      dplyr::select(
        dplyr::all_of(keys), "composite_value", "contributing_records",
        "flag_summary", "qa_fallback", "qa_status_unavailable"
      )
    names(result)[names(result) == "composite_value"] <- spec$value_column[[1]]
    names(result)[names(result) == "contributing_records"] <- spec$count_column[[1]]
    names(result)[names(result) == "flag_summary"] <- spec$flag_column[[1]]
    names(result)[names(result) == "qa_fallback"] <- paste0("tceq_", spec$key[[1]], "_qa_fallback")
    names(result)[names(result) == "qa_status_unavailable"] <- paste0(
      "tceq_", spec$key[[1]], "_qa_status_unavailable"
    )
    result
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) return(tibble::tibble())
  Reduce(function(x, y) dplyr::full_join(x, y, by = keys), pieces) |>
    dplyr::arrange(.data$aqs_site_id, .data$date, .data$hour)
}

tceq_build_supplemental_only_hourly <- function(supplemental) {
  if (!nrow(supplemental)) return(tibble::tibble())
  required <- c(
    "aqs_site_id", "site_name", "date", "hour", "hour_lstd", "datetime_lstd"
  )
  tceq_assert_columns(
    supplemental, required, "Supplemental-only TCEQ hourly composite"
  )
  primary_order <- c(
    "aqs_site_id", "site_name", "date", "hour", "hour_lstd",
    "total_records", "available_records", "primary_records",
    "acceptable_records", "has_primary_value", "has_acceptable_value",
    "value_flag_summary", "selected_parameter_code", "has_regulatory_value",
    "composite_pm25_ug_m3", "contributing_records", "composite_min_ug_m3",
    "composite_max_ug_m3", "composite_spread_ug_m3", "contributor_pocs",
    "contributor_parameter_codes", "contributor_sources",
    "has_qa_preferred_value", "has_explicit_nonqa_value",
    "parameter_fallback", "qa_fallback", "qa_status_unavailable",
    "composite_selection", "datetime_lstd"
  )
  supplemental |>
    dplyr::mutate(
      total_records = 0L,
      available_records = 0L,
      primary_records = 0L,
      acceptable_records = 0L,
      has_primary_value = FALSE,
      has_acceptable_value = FALSE,
      value_flag_summary = NA_character_,
      selected_parameter_code = NA_character_,
      has_regulatory_value = FALSE,
      composite_pm25_ug_m3 = NA_real_,
      contributing_records = 0L,
      composite_min_ug_m3 = NA_real_,
      composite_max_ug_m3 = NA_real_,
      composite_spread_ug_m3 = NA_real_,
      contributor_pocs = NA_character_,
      contributor_parameter_codes = NA_character_,
      contributor_sources = NA_character_,
      has_qa_preferred_value = FALSE,
      has_explicit_nonqa_value = FALSE,
      parameter_fallback = FALSE,
      qa_fallback = FALSE,
      qa_status_unavailable = FALSE,
      composite_selection = "missing"
    ) |>
    dplyr::select(dplyr::all_of(primary_order), dplyr::everything()) |>
    dplyr::arrange(.data$date, .data$hour)
}

tceq_merge_supplemental_hourly <- function(primary, supplemental) {
  if (!nrow(supplemental)) return(primary)
  keys <- c("aqs_site_id", "date", "hour", "hour_lstd", "datetime_lstd")
  merged <- dplyr::full_join(
    primary, supplemental, by = keys, suffix = c("", ".supplemental")
  ) |>
    dplyr::mutate(
      site_name = dplyr::coalesce(.data$site_name, .data$site_name.supplemental)
    ) |>
    dplyr::select(-"site_name.supplemental") |>
    dplyr::arrange(.data$date, .data$hour)
  count_columns <- tceq_supplemental_measurement_catalog()$count_column
  for (column in intersect(count_columns, names(merged))) {
    merged[[column]] <- dplyr::coalesce(as.integer(merged[[column]]), 0L)
  }
  merged
}
