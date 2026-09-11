# Reusable preparation and event-detection helpers for the DFW TCEQ PM2.5
# dashboard. Timestamps use UTC internally as a neutral clock representation;
# their displayed values are the local-standard-time values reported by TCEQ.

tceq_dashboard_cache_dir <- function(cfg) {
  file.path(cfg$paths$derived, "tceq_dfw_pm25_dashboard")
}

tceq_dashboard_raw_dir <- function(repo_root) {
  file.path(repo_root, "data", "raw", "tceq_dfw_pm25_monthly")
}

tceq_assert_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " lacks required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

tceq_collapse_values <- function(x, separator = "|") {
  x <- trimws(as.character(x))
  x <- sort(unique(x[!is.na(x) & nzchar(x)]))
  if (length(x)) paste(x, collapse = separator) else ""
}

tceq_safe_max <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_
}

tceq_safe_min <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) min(x, na.rm = TRUE) else NA_real_
}

tceq_safe_mean <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
}

tceq_safe_median <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) stats::median(x, na.rm = TRUE) else NA_real_
}

tceq_lstd_datetime <- function(date, hour) {
  # UTC deliberately prevents a browser or operating system from shifting the
  # clock values for daylight saving time. The values are labeled LST in UI.
  as.POSIXct(
    sprintf("%s %02d:00:00", as.character(as.Date(date)), as.integer(hour)),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
}

tceq_raw_series_key <- function(source_file, report_block, poc) {
  paste(as.character(source_file), as.integer(report_block), as.integer(poc),
        sep = "::")
}

tceq_fetch_site_html <- function(cams_id, attempts = 3L) {
  require_packages(c("curl"))
  url <- sprintf(
    "https://www.tceq.texas.gov/cgi-bin/compliance/monops/site_photo.pl?cams=%s",
    as.integer(cams_id)
  )
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      curl::curl_fetch_memory(
        url,
        handle = curl::new_handle(
          useragent = "TX-FireHealth TCEQ PM2.5 dashboard metadata"
        )
      ),
      error = function(e) e
    )
    if (!inherits(result, "error") && result$status_code == 200L) {
      return(rawToChar(result$content))
    }
    last_error <- if (inherits(result, "error")) {
      conditionMessage(result)
    } else {
      paste("HTTP", result$status_code)
    }
  }
  stop("Could not retrieve TCEQ metadata for CAMS ", cams_id, ": ", last_error)
}

tceq_parse_site_metadata <- function(html, cams_id, expected_aqs_site_id = NULL) {
  require_packages(c("rvest", "stringr", "tibble"))
  document <- rvest::read_html(html)
  items <- rvest::html_text2(rvest::html_elements(document, "li"))
  headings <- rvest::html_text2(rvest::html_elements(document, "h1"))
  site_headings <- headings[grepl("Site Photographs$", headings)]
  heading <- if (length(site_headings)) tail(site_headings, 1L) else tail(headings, 1L)
  pick <- function(prefix, required = TRUE) {
    value <- items[startsWith(items, prefix)]
    if (!length(value)) {
      if (required) stop("Missing '", prefix, "' on the CAMS ", cams_id, " page.")
      return(NA_character_)
    }
    trimws(sub(paste0("^", prefix), "", value[1]))
  }
  coordinate <- function(value, label) {
    matched <- stringr::str_match(value, "\\(([+-][0-9.]+)[^)]*\\)")[, 2]
    number <- suppressWarnings(as.numeric(matched))
    if (!is.finite(number)) stop("Could not parse ", label, " for CAMS ", cams_id, ".")
    number
  }
  aqs_hyphen <- pick("EPA site number:")
  aqs_site_id <- gsub("-", "_", aqs_hyphen, fixed = TRUE)
  if (!is.null(expected_aqs_site_id) && !identical(aqs_site_id, expected_aqs_site_id)) {
    stop(
      "CAMS ", cams_id, " metadata returned AQS site ", aqs_site_id,
      "; expected ", expected_aqs_site_id, "."
    )
  }
  latitude_text <- pick("Latitude:")
  longitude_text <- pick("Longitude:")
  status <- pick("Current status:")
  tibble::tibble(
    cams_id = as.integer(cams_id),
    aqs_site_id = aqs_site_id,
    metadata_site_name = trimws(sub(" Site Photographs$", "", heading)),
    state = pick("State:", required = FALSE),
    county = pick("County:", required = FALSE),
    city = pick("City:", required = FALSE),
    address = pick("Address:", required = FALSE),
    latitude = coordinate(latitude_text, "latitude"),
    longitude = coordinate(longitude_text, "longitude"),
    elevation = pick("Elevation:", required = FALSE),
    monitoring_since = pick("Real-time monitoring since:", required = FALSE),
    current_status = status,
    active = startsWith(tolower(status), "active"),
    metadata_url = sprintf(
      "https://www.tceq.texas.gov/cgi-bin/compliance/monops/site_photo.pl?cams=%s",
      as.integer(cams_id)
    ),
    metadata_retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

tceq_build_station_metadata <- function(inventory, fetcher = tceq_fetch_site_html) {
  require_packages(c("dplyr", "purrr", "tibble"))
  tceq_assert_columns(
    inventory,
    c("cams_id", "aqs_site_id", "site_name", "canonical_cams_id", "download_selected"),
    "TCEQ site inventory"
  )
  canonical <- inventory |>
    dplyr::filter(.data$download_selected %in% TRUE) |>
    dplyr::arrange(.data$aqs_site_id)
  if (nrow(canonical) != 16L || anyDuplicated(canonical$aqs_site_id)) {
    stop("Expected 16 unique canonical DFW PM2.5 monitoring sites.")
  }
  aliases <- inventory |>
    dplyr::filter(.data$aqs_site_id %in% canonical$aqs_site_id) |>
    dplyr::group_by(.data$aqs_site_id) |>
    dplyr::summarise(
      cams_aliases = paste(sort(unique(as.integer(.data$cams_id))), collapse = "|"),
      .groups = "drop"
    )
  pages <- purrr::map2_dfr(
    canonical$cams_id, canonical$aqs_site_id,
    function(cams, aqs) tceq_parse_site_metadata(fetcher(cams), cams, aqs)
  )
  canonical |>
    dplyr::transmute(
      cams_id = as.integer(.data$cams_id),
      aqs_site_id = .data$aqs_site_id,
      site_name = trimws(.data$site_name),
      first_month = as.Date(.data$first_month),
      download_last_month = as.Date(.data$download_last_month),
      inventory_deactivated_date = as.Date(.data$deactivated_date)
    ) |>
    dplyr::left_join(aliases, by = "aqs_site_id") |>
    dplyr::left_join(pages, by = c("cams_id", "aqs_site_id")) |>
    dplyr::arrange(.data$site_name)
}

tceq_build_hourly_composite <- function(raw) {
  require_packages(c("dplyr", "tibble"))
  required <- c(
    "aqs_site_id", "site_name", "date", "hour", "hour_lstd", "pm25_ug_m3",
    "value_flag", "poc", "source_file", "report_block", "regulatory_qa"
  )
  tceq_assert_columns(raw, required, "Imported hourly TCEQ data")
  keyed <- raw |>
    dplyr::mutate(
      date = as.Date(.data$date),
      finite_value = is.finite(.data$pm25_ug_m3),
      regulatory_value = .data$finite_value & !is.na(.data$regulatory_qa) &
        .data$regulatory_qa,
      raw_series_key = tceq_raw_series_key(
        .data$source_file, .data$report_block, .data$poc
      )
    )
  keys <- c("aqs_site_id", "site_name", "date", "hour", "hour_lstd")
  status <- keyed |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      total_records = dplyr::n(),
      available_records = sum(.data$finite_value),
      has_regulatory_value = any(.data$regulatory_value),
      value_flag_summary = tceq_collapse_values(.data$value_flag),
      .groups = "drop"
    )
  selected <- keyed |>
    dplyr::left_join(
      status |> dplyr::select(dplyr::all_of(c(keys, "has_regulatory_value"))),
      by = keys
    ) |>
    dplyr::filter(
      .data$finite_value &
        (!.data$has_regulatory_value | .data$regulatory_value)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      composite_pm25_ug_m3 = stats::median(.data$pm25_ug_m3),
      contributing_records = dplyr::n(),
      composite_min_ug_m3 = min(.data$pm25_ug_m3),
      composite_max_ug_m3 = max(.data$pm25_ug_m3),
      composite_spread_ug_m3 = .data$composite_max_ug_m3 - .data$composite_min_ug_m3,
      contributor_pocs = tceq_collapse_values(.data$poc),
      contributor_sources = tceq_collapse_values(.data$raw_series_key),
      .groups = "drop"
    )
  status |>
    dplyr::left_join(selected, by = keys) |>
    dplyr::mutate(
      qa_fallback = .data$available_records > 0L & !.data$has_regulatory_value,
      composite_selection = dplyr::case_when(
        .data$has_regulatory_value ~ "regulatory_qa",
        .data$available_records > 0L ~ "qa_fallback",
        TRUE ~ "missing"
      ),
      datetime_lstd = tceq_lstd_datetime(.data$date, .data$hour)
    ) |>
    dplyr::arrange(.data$date, .data$hour)
}

tceq_build_daily_summary <- function(hourly, raw, minimum_valid_hours = 18L) {
  require_packages(c("dplyr", "tibble"))
  tceq_assert_columns(
    hourly,
    c("aqs_site_id", "date", "composite_pm25_ug_m3", "qa_fallback",
      "contributing_records"),
    "Hourly composite"
  )
  tceq_assert_columns(
    raw,
    c("date", "source_file", "report_block", "poc", "daily_max_ug_m3",
      "daily_avg_ug_m3", "daily_std_ug_m3", "daily_max_flag", "daily_avg_flag",
      "daily_std_flag"),
    "Imported hourly TCEQ data"
  )
  computed <- hourly |>
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
      contributing_records = sum(.data$contributing_records, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(event_eligible = .data$valid_hours >= minimum_valid_hours)
  reported <- raw |>
    dplyr::distinct(
      .data$aqs_site_id, .data$date, .data$source_file, .data$report_block, .data$poc,
      .data$daily_max_ug_m3, .data$daily_avg_ug_m3, .data$daily_std_ug_m3,
      .data$daily_max_flag, .data$daily_avg_flag, .data$daily_std_flag
    ) |>
    dplyr::group_by(.data$aqs_site_id, .data$date) |>
    dplyr::summarise(
      reported_daily_records = dplyr::n(),
      reported_daily_max_median_ug_m3 = tceq_safe_median(.data$daily_max_ug_m3),
      reported_daily_max_min_ug_m3 = tceq_safe_min(.data$daily_max_ug_m3),
      reported_daily_max_max_ug_m3 = tceq_safe_max(.data$daily_max_ug_m3),
      reported_daily_avg_median_ug_m3 = tceq_safe_median(.data$daily_avg_ug_m3),
      reported_daily_avg_min_ug_m3 = tceq_safe_min(.data$daily_avg_ug_m3),
      reported_daily_avg_max_ug_m3 = tceq_safe_max(.data$daily_avg_ug_m3),
      reported_daily_std_median_ug_m3 = tceq_safe_median(.data$daily_std_ug_m3),
      reported_daily_flags = tceq_collapse_values(c(
        .data$daily_max_flag, .data$daily_avg_flag, .data$daily_std_flag
      )),
      .groups = "drop"
    )
  computed |>
    dplyr::left_join(reported, by = c("aqs_site_id", "date")) |>
    dplyr::mutate(
      computed_minus_reported_avg_ug_m3 =
        .data$computed_daily_avg_ug_m3 - .data$reported_daily_avg_median_ug_m3,
      computed_minus_reported_max_ug_m3 =
        .data$computed_daily_max_ug_m3 - .data$reported_daily_max_median_ug_m3
    ) |>
    dplyr::arrange(.data$date)
}

tceq_empty_events <- function() {
  tibble::tibble(
    aqs_site_id = character(), event_id = character(), event_type = character(),
    start_lstd = as.POSIXct(character(), tz = "UTC"),
    end_lstd = as.POSIXct(character(), tz = "UTC"),
    start_date = as.Date(character()), end_date = as.Date(character()),
    event_year = integer(), duration_hours = integer(), span_days = integer(),
    peak_hourly_ug_m3 = double(), mean_hourly_ug_m3 = double(),
    peak_daily_max_ug_m3 = double(), max_daily_avg_ug_m3 = double(),
    valid_hours = integer(), contributing_records = integer(), qa_fallback = logical(),
    core_dates = character(), trough_dates = character(), coverage_warning = logical(),
    definition = character()
  )
}

tceq_detect_short_spikes <- function(hourly, threshold = 35, maximum_hours = 5L) {
  require_packages(c("dplyr", "tibble"))
  if (!nrow(hourly)) return(tceq_empty_events())
  x <- hourly |>
    dplyr::arrange(.data$datetime_lstd) |>
    dplyr::mutate(
      qualifies = is.finite(.data$composite_pm25_ug_m3) &
        .data$composite_pm25_ug_m3 > threshold,
      elapsed_hours = c(
        NA_real_, diff(as.numeric(.data$datetime_lstd)) / 3600
      ),
      begins_run = .data$qualifies &
        (!dplyr::lag(.data$qualifies, default = FALSE) |
           is.na(.data$elapsed_hours) | .data$elapsed_hours != 1),
      run_id = cumsum(.data$begins_run)
    ) |>
    dplyr::filter(.data$qualifies)
  if (!nrow(x)) return(tceq_empty_events())
  events <- x |>
    dplyr::group_by(.data$aqs_site_id, .data$run_id) |>
    dplyr::summarise(
      start_lstd = min(.data$datetime_lstd),
      end_lstd = max(.data$datetime_lstd),
      start_date = min(.data$date),
      end_date = max(.data$date),
      duration_hours = dplyr::n(),
      span_days = dplyr::n_distinct(.data$date),
      peak_hourly_ug_m3 = max(.data$composite_pm25_ug_m3),
      mean_hourly_ug_m3 = mean(.data$composite_pm25_ug_m3),
      valid_hours = dplyr::n(),
      contributing_records = sum(.data$contributing_records, na.rm = TRUE),
      qa_fallback = any(.data$qa_fallback),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$duration_hours <= maximum_hours) |>
    dplyr::mutate(
      event_type = "short_spike",
      event_year = as.integer(format(.data$start_date, "%Y")),
      event_id = paste0(
        .data$aqs_site_id, "_spike_",
        format(.data$start_lstd, "%Y%m%dT%H", tz = "UTC")
      ),
      peak_daily_max_ug_m3 = NA_real_, max_daily_avg_ug_m3 = NA_real_,
      core_dates = "", trough_dates = "", coverage_warning = FALSE,
      definition = sprintf("> %.0f ug/m3 for 1-%d consecutive hours", threshold, maximum_hours)
    ) |>
    dplyr::select(dplyr::all_of(names(tceq_empty_events())))
  events
}

tceq_detect_multiday_events <- function(
    daily, hourly, maximum_threshold = 25, average_threshold = 20,
    minimum_valid_hours = 18L) {
  require_packages(c("dplyr", "tibble", "tidyr"))
  if (!nrow(daily)) return(tceq_empty_events())
  site_id <- unique(daily$aqs_site_id)
  if (length(site_id) != 1L) stop("Multi-day detection expects one monitoring site.")
  full <- tibble::tibble(date = seq(min(daily$date), max(daily$date), by = "day")) |>
    dplyr::left_join(daily, by = "date") |>
    dplyr::mutate(
      aqs_site_id = site_id,
      event_eligible = !is.na(.data$valid_hours) &
        .data$valid_hours >= minimum_valid_hours,
      core_day = .data$event_eligible &
        is.finite(.data$computed_daily_max_ug_m3) &
        .data$computed_daily_max_ug_m3 > maximum_threshold &
        is.finite(.data$computed_daily_avg_ug_m3) &
        .data$computed_daily_avg_ug_m3 > average_threshold,
      trough_day = .data$event_eligible & !.data$core_day &
        dplyr::lag(.data$core_day, default = FALSE) &
        dplyr::lead(.data$core_day, default = FALSE),
      included_day = .data$core_day | .data$trough_day,
      begins_episode = .data$included_day &
        !dplyr::lag(.data$included_day, default = FALSE),
      episode_id = cumsum(.data$begins_episode)
    )
  candidate <- full |>
    dplyr::filter(.data$included_day) |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::filter(sum(.data$core_day) >= 2L) |>
    dplyr::ungroup()
  if (!nrow(candidate)) return(tceq_empty_events())
  events <- candidate |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::group_modify(function(days, key) {
      start_date <- min(days$date)
      end_date <- max(days$date)
      h <- hourly |>
        dplyr::filter(.data$date >= start_date, .data$date <= end_date)
      tibble::tibble(
        aqs_site_id = site_id,
        start_lstd = tceq_lstd_datetime(start_date, 0L),
        end_lstd = tceq_lstd_datetime(end_date, 23L),
        start_date = start_date,
        end_date = end_date,
        duration_hours = as.integer(end_date - start_date + 1) * 24L,
        span_days = as.integer(end_date - start_date + 1),
        peak_hourly_ug_m3 = tceq_safe_max(h$composite_pm25_ug_m3),
        mean_hourly_ug_m3 = tceq_safe_mean(h$composite_pm25_ug_m3),
        peak_daily_max_ug_m3 = tceq_safe_max(days$computed_daily_max_ug_m3),
        max_daily_avg_ug_m3 = tceq_safe_max(days$computed_daily_avg_ug_m3),
        valid_hours = as.integer(sum(days$valid_hours, na.rm = TRUE)),
        contributing_records = as.integer(sum(h$contributing_records, na.rm = TRUE)),
        qa_fallback = any(h$qa_fallback & is.finite(h$composite_pm25_ug_m3)),
        core_dates = paste(days$date[days$core_day], collapse = "|"),
        trough_dates = paste(days$date[days$trough_day], collapse = "|"),
        coverage_warning = any(days$valid_hours < 24L)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      event_type = "multi_day",
      event_year = as.integer(format(.data$start_date, "%Y")),
      event_id = paste0(
        .data$aqs_site_id, "_multiday_", format(.data$start_date, "%Y%m%d"),
        "_", format(.data$end_date, "%Y%m%d")
      ),
      definition = sprintf(
        "At least 2 core days: daily max > %.0f and mean > %.0f ug/m3; one eligible trough day may bridge",
        maximum_threshold, average_threshold
      )
    ) |>
    dplyr::select(dplyr::all_of(names(tceq_empty_events())))
  events
}

tceq_build_event_index <- function(hourly, daily) {
  dplyr::bind_rows(
    tceq_detect_short_spikes(hourly),
    tceq_detect_multiday_events(daily, hourly)
  ) |>
    dplyr::arrange(.data$start_lstd, .data$event_type)
}

tceq_validate_heatmap_range <- function(start_date, end_date, maximum_days = 90L) {
  dates <- as.Date(c(start_date, end_date))
  if (length(dates) != 2L || anyNA(dates)) stop("Choose a valid start and end date.")
  if (dates[1] > dates[2]) stop("The start date must be on or before the end date.")
  span <- as.integer(dates[2] - dates[1]) + 1L
  if (span > maximum_days) {
    stop("The hourly heatmap is limited to ", maximum_days, " days; selected ", span, ".")
  }
  dates
}

tceq_complete_heatmap <- function(hourly, start_date, end_date, maximum_days = 90L) {
  require_packages(c("dplyr", "tidyr", "tibble"))
  dates <- tceq_validate_heatmap_range(start_date, end_date, maximum_days)
  tibble::tibble(date = seq(dates[1], dates[2], by = "day")) |>
    tidyr::crossing(hour = 0:23) |>
    dplyr::left_join(
      hourly |>
        dplyr::select(dplyr::all_of(c(
          "date", "hour", "composite_pm25_ug_m3", "value_flag_summary",
          "contributing_records", "available_records", "qa_fallback",
          "composite_selection"
        ))),
      by = c("date", "hour")
    ) |>
    dplyr::mutate(
      hour_lstd = sprintf("%02d:00", .data$hour),
      composite_selection = dplyr::coalesce(.data$composite_selection, "missing"),
      qa_fallback = dplyr::coalesce(.data$qa_fallback, FALSE),
      value_flag_summary = dplyr::coalesce(.data$value_flag_summary, "")
    )
}

tceq_cache_is_stale <- function(cache_manifest, raw_manifest_path) {
  if (!file.exists(raw_manifest_path) || is.null(cache_manifest$raw_manifest_sha256)) {
    return(NA)
  }
  !identical(
    as.character(cache_manifest$raw_manifest_sha256),
    as.character(sha256_file(raw_manifest_path))
  )
}

tceq_site_cache_filename <- function(aqs_site_id) {
  paste0(gsub("[^A-Za-z0-9_-]", "_", aqs_site_id), ".rds")
}

tceq_read_site_bundle <- function(cache_dir, aqs_site_id) {
  path <- file.path(cache_dir, "sites", tceq_site_cache_filename(aqs_site_id))
  if (!file.exists(path)) stop("Missing site cache: ", path)
  readRDS(path)
}

tceq_dashboard_bundle_loader <- function(
    cache_dir, max_entries = 3L, reader = tceq_read_site_bundle) {
  max_entries <- suppressWarnings(as.integer(max_entries))
  if (length(max_entries) != 1L || is.na(max_entries) || max_entries < 1L) {
    stop("max_entries must be one positive integer.")
  }
  state <- new.env(parent = emptyenv())
  state$bundles <- new.env(parent = emptyenv())
  state$recent <- character()

  function(aqs_site_id) {
    key <- gsub("[^A-Za-z0-9_]", "_", aqs_site_id)
    if (!exists(key, envir = state$bundles, inherits = FALSE)) {
      assign(key, reader(cache_dir, aqs_site_id), envir = state$bundles)
    }
    state$recent <- c(key, setdiff(state$recent, key))
    if (length(state$recent) > max_entries) {
      discard <- state$recent[seq.int(max_entries + 1L, length(state$recent))]
      rm(list = discard, envir = state$bundles)
      state$recent <- state$recent[seq_len(max_entries)]
    }
    get(key, envir = state$bundles, inherits = FALSE)
  }
}
