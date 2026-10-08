pm25_nctcog_counties <- function() {
  c(
    "Collin", "Dallas", "Denton", "Ellis", "Erath", "Hood", "Hunt",
    "Johnson", "Kaufman", "Navarro", "Palo Pinto", "Parker", "Rockwall",
    "Somervell", "Tarrant", "Wise"
  )
}

pm25_split_dates <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(as.Date(character()))
  as.Date(strsplit(x, "|", fixed = TRUE)[[1]])
}

pm25_event_key <- function(site_id, event_id) {
  paste(site_id, event_id, sep = "::")
}

pm25_haversine_km <- function(lon1, lat1, lon2, lat2) {
  radians <- pi / 180
  lon1 <- lon1 * radians
  lat1 <- lat1 * radians
  lon2 <- lon2 * radians
  lat2 <- lat2 * radians
  delta_lon <- lon2 - lon1
  delta_lat <- lat2 - lat1
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

pm25_build_regional_daily <- function(daily_index, event_index, station_index) {
  require_packages(c("dplyr", "tibble"))
  station_fields <- station_index |>
    dplyr::select(dplyr::all_of(c(
      "aqs_site_id", "site_name", "data_source", "latitude", "longitude"
    ))) |>
    dplyr::distinct(.data$aqs_site_id, .keep_all = TRUE)

  multi <- event_index |>
    dplyr::filter(.data$event_type == "multi_day")
  active <- lapply(seq_len(nrow(multi)), function(i) {
    event <- multi[i, , drop = FALSE]
    dates <- seq(as.Date(event$start_date), as.Date(event$end_date), by = "day")
    core <- pm25_split_dates(event$core_dates)
    trough <- pm25_split_dates(event$trough_dates)
    tibble::tibble(
      aqs_site_id = event$aqs_site_id,
      date = dates,
      event_active = TRUE,
      event_core = dates %in% core,
      event_trough = dates %in% trough,
      active_event_ids = event$event_id
    )
  })
  active <- if (length(active)) dplyr::bind_rows(active) else tibble::tibble(
    aqs_site_id = character(), date = as.Date(character()), event_active = logical(),
    event_core = logical(), event_trough = logical(), active_event_ids = character()
  )
  active <- active |>
    dplyr::group_by(.data$aqs_site_id, .data$date) |>
    dplyr::summarise(
      event_active = any(.data$event_active),
      event_core = any(.data$event_core),
      event_trough = any(.data$event_trough),
      active_event_ids = paste(sort(unique(.data$active_event_ids)), collapse = "|"),
      .groups = "drop"
    )

  daily_index |>
    dplyr::mutate(
      date = as.Date(.data$date),
      event_eligible = .data$valid_hours >= 18L,
      threshold_core = .data$event_eligible &
        is.finite(.data$computed_daily_max_ug_m3) &
        is.finite(.data$computed_daily_avg_ug_m3) &
        .data$computed_daily_max_ug_m3 > 25 &
        .data$computed_daily_avg_ug_m3 > 20
    ) |>
    dplyr::select(dplyr::any_of(
      c(
        "aqs_site_id", "date", "data_source", "valid_hours",
        "represented_hours", "computed_daily_max_ug_m3",
        "computed_daily_avg_ug_m3", "computed_daily_std_ug_m3",
        "qa_fallback_hours", "parameter_fallback_hours",
        "contributing_records", "event_eligible", "threshold_core"
      )
    )) |>
    dplyr::left_join(active, by = c("aqs_site_id", "date")) |>
    dplyr::mutate(
      event_active = dplyr::coalesce(.data$event_active, FALSE),
      event_core = dplyr::coalesce(.data$event_core, FALSE),
      event_trough = dplyr::coalesce(.data$event_trough, FALSE),
      active_event_ids = dplyr::coalesce(.data$active_event_ids, "")
    ) |>
    dplyr::left_join(station_fields, by = "aqs_site_id", suffix = c("", ".station")) |>
    dplyr::select(dplyr::any_of(c(
      "aqs_site_id", "date", "data_source", "valid_hours",
      "computed_daily_max_ug_m3", "computed_daily_avg_ug_m3",
      "qa_fallback_hours", "parameter_fallback_hours", "event_eligible",
      "threshold_core", "event_active", "event_core", "event_trough"
    ))) |>
    dplyr::arrange(.data$date, .data$data_source, .data$aqs_site_id)
}

pm25_daily_reach <- function(regional_daily, minimum_network_sites = 3L) {
  require_packages(c("dplyr", "tidyr", "tibble"))
  by_network <- regional_daily |>
    dplyr::group_by(.data$date, .data$data_source) |>
    dplyr::summarise(
      eligible_sites = sum(.data$event_eligible, na.rm = TRUE),
      affected_sites = sum(.data$threshold_core & .data$event_eligible, na.rm = TRUE),
      event_active_sites = sum(.data$event_active & .data$event_eligible, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      adequate_network = .data$eligible_sites >= minimum_network_sites,
      affected_percent = dplyr::if_else(
        .data$adequate_network,
        100 * .data$affected_sites / .data$eligible_sites,
        NA_real_
      )
    )

  wide <- by_network |>
    dplyr::mutate(network_key = dplyr::case_when(
      .data$data_source == "TCEQ" ~ "tceq",
      .data$data_source == "Dallas AQMesh" ~ "aqmesh",
      TRUE ~ make.names(tolower(.data$data_source))
    )) |>
    dplyr::select(
      "date", "network_key", "eligible_sites", "affected_sites",
      "event_active_sites", "adequate_network", "affected_percent"
    ) |>
    tidyr::pivot_wider(
      names_from = "network_key",
      values_from = c(
        "eligible_sites", "affected_sites", "event_active_sites",
        "adequate_network", "affected_percent"
      ),
      values_fill = list(
        eligible_sites = 0L, affected_sites = 0L, event_active_sites = 0L,
        adequate_network = FALSE, affected_percent = NA_real_
      )
    )
  required <- c("tceq", "aqmesh")
  for (network in required) {
    for (prefix in c(
        "eligible_sites", "affected_sites", "event_active_sites",
        "adequate_network", "affected_percent")) {
      field <- paste(prefix, network, sep = "_")
      if (!field %in% names(wide)) {
        wide[[field]] <- if (prefix == "affected_percent") NA_real_ else
          if (prefix == "adequate_network") FALSE else 0L
      }
    }
  }

  wide |>
    dplyr::rowwise() |>
    dplyr::mutate(
      eligible_sites = .data$eligible_sites_tceq + .data$eligible_sites_aqmesh,
      affected_sites = .data$affected_sites_tceq + .data$affected_sites_aqmesh,
      event_active_sites = .data$event_active_sites_tceq + .data$event_active_sites_aqmesh,
      raw_affected_percent = dplyr::if_else(
        .data$eligible_sites > 0L,
        100 * .data$affected_sites / .data$eligible_sites,
        NA_real_
      ),
      represented_networks = sum(
        c(.data$adequate_network_tceq, .data$adequate_network_aqmesh)
      ),
      balanced_participation_percent = {
        values <- c(.data$affected_percent_tceq, .data$affected_percent_aqmesh)
        if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
      },
      coverage_label = dplyr::case_when(
        .data$represented_networks >= 2L ~ "Both networks",
        .data$adequate_network_tceq ~ "TCEQ only",
        .data$adequate_network_aqmesh ~ "AQMesh only",
        TRUE ~ "Insufficient coverage"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$date)
}

pm25_build_regional_event_links <- function(event_index, station_index) {
  require_packages(c("dplyr", "tibble"))
  events <- event_index |>
    dplyr::filter(.data$event_type == "multi_day") |>
    dplyr::left_join(
      station_index |>
        dplyr::select(dplyr::all_of(c(
          "aqs_site_id", "site_name", "data_source", "latitude", "longitude"
        ))) |>
        dplyr::distinct(.data$aqs_site_id, .keep_all = TRUE),
      by = c("aqs_site_id", "data_source"), suffix = c("", ".station")
    ) |>
    dplyr::mutate(event_key = pm25_event_key(.data$aqs_site_id, .data$event_id))
  if (nrow(events) < 2L) return(tibble::tibble())

  core <- lapply(events$core_dates, pm25_split_dates)
  rows <- list()
  counter <- 0L
  for (i in seq_len(nrow(events) - 1L)) {
    for (j in seq.int(i + 1L, nrow(events))) {
      if (events$aqs_site_id[i] == events$aqs_site_id[j]) next
      overlap_start <- max(as.Date(events$start_date[i]), as.Date(events$start_date[j]))
      overlap_end <- min(as.Date(events$end_date[i]), as.Date(events$end_date[j]))
      if (overlap_start > overlap_end) next
      shared <- intersect(core[[i]], core[[j]])
      counter <- counter + 1L
      rows[[counter]] <- tibble::tibble(
        left = i, right = j,
        relationship = if (length(shared)) "shared_core" else "span_only",
        shared_core_dates = paste(shared, collapse = "|"),
        shared_core_days = length(shared),
        span_overlap_days = as.integer(overlap_end - overlap_start) + 1L
      )
    }
  }
  if (!length(rows)) return(tibble::tibble())
  pairs <- dplyr::bind_rows(rows)
  directed <- dplyr::bind_rows(
    pairs |> dplyr::transmute(
      anchor = .data$left, other = .data$right, dplyr::across(-c("left", "right"))
    ),
    pairs |> dplyr::transmute(
      anchor = .data$right, other = .data$left, dplyr::across(-c("left", "right"))
    )
  )
  anchor <- events[directed$anchor, , drop = FALSE]
  other <- events[directed$other, , drop = FALSE]
  tibble::tibble(
    anchor_event_key = anchor$event_key,
    anchor_site_id = anchor$aqs_site_id,
    anchor_event_id = anchor$event_id,
    other_event_key = other$event_key,
    other_site_id = other$aqs_site_id,
    other_site_name = other$site_name,
    other_data_source = other$data_source,
    other_event_id = other$event_id,
    other_start_date = as.Date(other$start_date),
    other_end_date = as.Date(other$end_date),
    start_lag_days = as.integer(as.Date(other$start_date) - as.Date(anchor$start_date)),
    relationship = directed$relationship,
    shared_core_dates = directed$shared_core_dates,
    shared_core_days = directed$shared_core_days,
    span_overlap_days = directed$span_overlap_days,
    distance_km = pm25_haversine_km(
      anchor$longitude, anchor$latitude, other$longitude, other$latitude
    ),
    peak_hourly_ug_m3 = other$peak_hourly_ug_m3,
    peak_daily_max_ug_m3 = other$peak_daily_max_ug_m3,
    max_daily_avg_ug_m3 = other$max_daily_avg_ug_m3,
    coverage_warning = other$coverage_warning,
    qa_fallback = other$qa_fallback,
    parameter_fallback = other$parameter_fallback
  ) |>
    dplyr::arrange(
      .data$anchor_event_key, dplyr::desc(.data$shared_core_days),
      .data$distance_km
    )
}

pm25_build_regional_hourly <- function(hourly_rows, station_index) {
  require_packages(c("dplyr", "tidyr", "tibble"))
  compact <- hourly_rows |>
    dplyr::filter(!is.na(.data$datetime_utc)) |>
    dplyr::group_by(.data$datetime_utc, .data$aqs_site_id) |>
    dplyr::summarise(
      pm25_ug_m3 = if (all(!is.finite(.data$composite_pm25_ug_m3))) {
        NA_real_
      } else stats::median(.data$composite_pm25_ug_m3, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "aqs_site_id", values_from = "pm25_ug_m3") |>
    dplyr::arrange(.data$datetime_utc)
  site_ids <- setdiff(names(compact), "datetime_utc")
  metadata <- station_index[match(site_ids, station_index$aqs_site_id), , drop = FALSE]
  timestamps <- as.POSIXct(compact$datetime_utc, tz = "UTC")
  years <- as.integer(format(timestamps, "%Y", tz = "UTC"))
  shards <- lapply(sort(unique(years)), function(year) {
    keep <- years == year
    list(
      schema_version = 1L,
      year = year,
      timestamp_utc = timestamps[keep],
      site_ids = site_ids,
      data_source = metadata$data_source,
      values = as.matrix(compact[keep, site_ids, drop = FALSE])
    )
  })
  names(shards) <- as.character(sort(unique(years)))
  list(
    index = list(
      schema_version = 2L,
      site_ids = site_ids,
      data_source = metadata$data_source,
      years = tibble::tibble(
        year = as.integer(names(shards)),
        first_timestamp_utc = as.POSIXct(vapply(
          shards, function(x) as.numeric(min(x$timestamp_utc)), numeric(1)
        ), origin = "1970-01-01", tz = "UTC"),
        last_timestamp_utc = as.POSIXct(vapply(
          shards, function(x) as.numeric(max(x$timestamp_utc)), numeric(1)
        ), origin = "1970-01-01", tz = "UTC"),
        rows = vapply(shards, function(x) length(x$timestamp_utc), integer(1)),
        file = file.path("regional_hourly", paste0("year_", names(shards), ".rds"))
      )
    ),
    shards = shards
  )
}

pm25_download_county_boundary <- function(reference_dir, year = 2024L) {
  require_packages(c("curl", "sf"))
  dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- sprintf("cb_%d_us_county_500k.zip", as.integer(year))
  archive <- file.path(reference_dir, filename)
  url <- sprintf(
    "https://www2.census.gov/geo/tiger/GENZ%d/shp/%s",
    as.integer(year), filename
  )
  if (!file.exists(archive)) {
    temporary <- tempfile(pattern = filename, tmpdir = reference_dir)
    on.exit(unlink(temporary), add = TRUE)
    curl::curl_download(url, temporary, quiet = FALSE)
    if (!file.rename(temporary, archive)) stop("Could not cache county geometry.")
  }
  extraction <- tempfile("nctcog_counties_")
  dir.create(extraction)
  on.exit(unlink(extraction, recursive = TRUE), add = TRUE)
  utils::unzip(archive, exdir = extraction)
  shape <- list.files(extraction, pattern = "\\.shp$", full.names = TRUE)
  if (length(shape) != 1L) stop("County archive did not contain one shapefile.")
  counties <- suppressWarnings(sf::st_read(shape, quiet = TRUE))
  counties <- counties[
    counties$STATEFP == "48" & counties$NAME %in% pm25_nctcog_counties(),
  ]
  if (nrow(counties) != 16L) stop("Expected all 16 NCTCOG county boundaries.")
  attr(counties, "source_url") <- url
  attr(counties, "source_archive") <- normalizePath(archive, winslash = "/")
  counties
}

pm25_boundary_lines <- function(counties) {
  require_packages("sf")
  boundary <- suppressWarnings(
    counties |>
      sf::st_transform(4326) |>
      sf::st_cast("MULTILINESTRING") |>
      sf::st_cast("LINESTRING")
  )
  coordinates <- sf::st_coordinates(boundary)
  level_columns <- grep("^L[0-9]+$", colnames(coordinates), value = TRUE)
  groups <- if (length(level_columns)) {
    apply(coordinates[, level_columns, drop = FALSE], 1, paste, collapse = "-")
  } else seq_len(nrow(coordinates))
  tibble::tibble(
    longitude = coordinates[, "X"],
    latitude = coordinates[, "Y"],
    group = groups
  )
}

pm25_build_spatial_support <- function(
    station_index, counties, grid_resolution_m = 4000, analysis_crs = 3083) {
  require_packages(c("sf", "tibble"))
  region <- counties |>
    sf::st_transform(analysis_crs) |>
    sf::st_make_valid() |>
    sf::st_union()
  box <- sf::st_bbox(region)
  x <- seq(
    floor(box[["xmin"]] / grid_resolution_m) * grid_resolution_m + grid_resolution_m / 2,
    ceiling(box[["xmax"]] / grid_resolution_m) * grid_resolution_m - grid_resolution_m / 2,
    by = grid_resolution_m
  )
  y <- seq(
    floor(box[["ymin"]] / grid_resolution_m) * grid_resolution_m + grid_resolution_m / 2,
    ceiling(box[["ymax"]] / grid_resolution_m) * grid_resolution_m - grid_resolution_m / 2,
    by = grid_resolution_m
  )
  grid <- expand.grid(x = x, y = y)
  points <- sf::st_as_sf(grid, coords = c("x", "y"), crs = analysis_crs)
  inside <- lengths(sf::st_intersects(points, region)) > 0L
  geographic <- sf::st_coordinates(sf::st_transform(points, 4326))
  grid$longitude <- geographic[, "X"]
  grid$latitude <- geographic[, "Y"]
  grid$inside_region <- inside
  grid$column <- match(grid$x, x)
  grid$row <- length(y) - match(grid$y, y) + 1L

  stations <- sf::st_as_sf(
    station_index, coords = c("longitude", "latitude"), crs = 4326,
    remove = FALSE
  ) |>
    sf::st_transform(analysis_crs)
  station_xy <- sf::st_coordinates(stations)
  distances <- round(sqrt(
    outer(grid$x, station_xy[, "X"], "-")^2 +
      outer(grid$y, station_xy[, "Y"], "-")^2
  ))
  storage.mode(distances) <- "integer"
  list(
    schema_version = 1L,
    analysis_crs = analysis_crs,
    grid_resolution_m = grid_resolution_m,
    x_values = x,
    y_values = rev(y),
    grid = tibble::as_tibble(grid),
    station_ids = station_index$aqs_site_id,
    station_network = station_index$data_source,
    station_x = station_xy[, "X"],
    station_y = station_xy[, "Y"],
    distances_m = distances,
    boundary_lines = pm25_boundary_lines(counties),
    bounds = c(
      west = min(grid$longitude), south = min(grid$latitude),
      east = max(grid$longitude), north = max(grid$latitude)
    ),
    county_names = sort(as.character(counties$NAME)),
    source_url = attr(counties, "source_url"),
    source_archive = attr(counties, "source_archive"),
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

pm25_points_in_polygon <- function(x, y, polygon_x, polygon_y) {
  n <- length(polygon_x)
  if (n < 3L) return(rep(FALSE, length(x)))
  inside <- rep(FALSE, length(x))
  j <- n
  for (i in seq_len(n)) {
    crosses <- ((polygon_y[i] > y) != (polygon_y[j] > y)) &
      (x < (polygon_x[j] - polygon_x[i]) * (y - polygon_y[i]) /
        (polygon_y[j] - polygon_y[i] + .Machine$double.eps) + polygon_x[i])
    inside <- xor(inside, crosses)
    j <- i
  }
  inside
}

pm25_network_surface <- function(
    values, network, spatial_support, power = 2, neighbours = 8L,
    radius_m = 50000, nearest_m = 25000) {
  station_network <- spatial_support$station_network
  valid <- which(station_network == network & is.finite(values))
  n_grid <- nrow(spatial_support$grid)
  empty <- list(
    value = rep(NA_real_, n_grid), supported = rep(FALSE, n_grid),
    nearest_m = rep(NA_real_, n_grid), nearby_count = integer(n_grid),
    station_count = length(valid)
  )
  if (length(valid) < 3L) return(empty)
  xy <- cbind(spatial_support$station_x[valid], spatial_support$station_y[valid])
  if (qr(cbind(1, xy))$rank < 3L) return(empty)
  hull <- chull(xy[, 1], xy[, 2])
  hull <- c(hull, hull[1])
  inside_hull <- pm25_points_in_polygon(
    spatial_support$grid$x, spatial_support$grid$y,
    xy[hull, 1], xy[hull, 2]
  )
  distance <- spatial_support$distances_m[, valid, drop = FALSE]
  nearest <- apply(distance, 1, min)
  nearby <- rowSums(distance <= radius_m)
  supported <- spatial_support$grid$inside_region & inside_hull &
    nearest <= nearest_m & nearby >= 3L
  estimate <- rep(NA_real_, n_grid)
  rows <- which(supported)
  for (row in rows) {
    order <- order(distance[row, ])
    order <- order[distance[row, order] <= radius_m]
    order <- head(order, neighbours)
    d <- distance[row, order]
    v <- values[valid[order]]
    if (any(d < 1e-6)) {
      estimate[row] <- mean(v[d < 1e-6])
    } else {
      weights <- 1 / d^power
      estimate[row] <- sum(weights * v) / sum(weights)
    }
  }
  list(
    value = estimate, supported = supported, nearest_m = nearest,
    nearby_count = nearby, station_count = length(valid)
  )
}

pm25_interpolate_surface <- function(
    values, spatial_support, view = "Combined", power = 2,
    neighbours = 8L, radius_m = 50000, nearest_m = 25000) {
  if (is.null(names(values))) stop("Surface values must be named by site ID.")
  aligned <- values[match(spatial_support$station_ids, names(values))]
  tceq <- pm25_network_surface(
    aligned, "TCEQ", spatial_support, power, neighbours, radius_m, nearest_m
  )
  aqmesh <- pm25_network_surface(
    aligned, "Dallas AQMesh", spatial_support,
    power, neighbours, radius_m, nearest_m
  )
  both <- tceq$supported & aqmesh$supported
  combined <- rep(NA_real_, length(tceq$value))
  only_tceq <- tceq$supported & !aqmesh$supported
  only_aqmesh <- aqmesh$supported & !tceq$supported
  combined[only_tceq] <- tceq$value[only_tceq]
  combined[only_aqmesh] <- aqmesh$value[only_aqmesh]
  weight_tceq <- 1 / pmax(tceq$nearest_m, 1000)^2
  weight_aqmesh <- 1 / pmax(aqmesh$nearest_m, 1000)^2
  combined[both] <- (
    weight_tceq[both] * tceq$value[both] +
      weight_aqmesh[both] * aqmesh$value[both]
  ) / (weight_tceq[both] + weight_aqmesh[both])
  contribution <- rep("none", length(combined))
  contribution[only_tceq] <- "TCEQ only"
  contribution[only_aqmesh] <- "AQMesh only"
  contribution[both] <- "Both networks"
  disagreement <- rep(NA_real_, length(combined))
  disagreement[both] <- abs(tceq$value[both] - aqmesh$value[both])

  selected <- switch(
    view,
    TCEQ = tceq$value,
    `Dallas AQMesh` = aqmesh$value,
    Disagreement = disagreement,
    combined
  )
  selected_supported <- switch(
    view,
    TCEQ = tceq$supported,
    `Dallas AQMesh` = aqmesh$supported,
    Disagreement = both,
    !is.na(combined)
  )
  nearest_combined <- pmin(tceq$nearest_m, aqmesh$nearest_m, na.rm = TRUE)
  nearest_combined[!is.finite(nearest_combined)] <- NA_real_
  list(
    value = selected,
    supported = selected_supported,
    combined = combined,
    tceq = tceq,
    aqmesh = aqmesh,
    disagreement = disagreement,
    contribution = contribution,
    nearby_count = pmax(tceq$nearby_count, aqmesh$nearby_count),
    nearest_m = nearest_combined,
    valid_tceq = tceq$station_count,
    valid_aqmesh = aqmesh$station_count
  )
}

pm25_surface_modeled_reach <- function(mean_surface, max_surface) {
  supported <- mean_surface$supported & max_surface$supported
  if (!any(supported)) return(NA_real_)
  100 * mean(
    mean_surface$value[supported] > 20 & max_surface$value[supported] > 25,
    na.rm = TRUE
  )
}

pm25_validation_frame_indices <- function(timestamps, frame_values, limit = 24L) {
  timestamps <- as.POSIXct(timestamps, origin = "1970-01-01", tz = "UTC")
  eligible <- which(is.finite(frame_values) & !is.na(timestamps))
  if (!length(eligible)) return(integer())
  if (length(eligible) <= limit) return(eligible)
  month <- as.integer(format(timestamps[eligible], "%m", tz = "UTC"))
  season <- cut(month, breaks = c(0, 2, 5, 8, 11, 12), labels = FALSE)
  season[month == 12L] <- 1L
  breaks <- unique(stats::quantile(
    frame_values[eligible], probs = seq(0, 1, length.out = 4),
    na.rm = TRUE, names = FALSE
  ))
  value_band <- if (length(breaks) >= 2L) {
    cut(frame_values[eligible], breaks = breaks, include.lowest = TRUE, labels = FALSE)
  } else rep(1L, length(eligible))
  strata <- paste(season, value_band, sep = "-")
  chosen <- integer()
  for (stratum in sort(unique(strata))) {
    candidates <- eligible[strata == stratum]
    allocation <- max(1L, floor(limit / length(unique(strata))))
    positions <- unique(round(seq(1, length(candidates), length.out = allocation)))
    chosen <- c(chosen, candidates[positions])
  }
  if (length(chosen) < limit) {
    remaining <- setdiff(eligible, chosen)
    positions <- unique(round(seq(1, length(remaining), length.out = limit - length(chosen))))
    chosen <- c(chosen, remaining[positions])
  }
  sort(head(unique(chosen), limit))
}

pm25_surface_validation <- function(
    regional_daily, station_index, spatial_support, regional_hourly = NULL,
    metrics = c("computed_daily_avg_ug_m3", "computed_daily_max_ug_m3"),
    daily_frame_limit = 24L, hourly_frame_limit = 24L) {
  require_packages(c("dplyr", "tibble", "tidyr"))
  results <- list()
  counter <- 0L
  validate_frame <- function(values, metric, frame_time) {
    for (site_id in names(values)[is.finite(values)]) {
      held <- values[[site_id]]
      values[[site_id]] <- NA_real_
      station_row <- match(site_id, spatial_support$station_ids)
      if (is.na(station_row)) next
      surface <- pm25_interpolate_surface(values, spatial_support, "Combined")
      grid_row <- which.min(
        (spatial_support$grid$x - spatial_support$station_x[station_row])^2 +
          (spatial_support$grid$y - spatial_support$station_y[station_row])^2
      )
      estimate <- surface$combined[grid_row]
      if (is.finite(estimate)) {
        counter <<- counter + 1L
        results[[counter]] <<- tibble::tibble(
          metric = metric,
          frame_time_utc = as.POSIXct(frame_time, origin = "1970-01-01", tz = "UTC"),
          site_id = site_id,
          data_source = station_index$data_source[
            match(site_id, station_index$aqs_site_id)
          ],
          observed = held, estimated = estimate, error = estimate - held
        )
      }
      values[[site_id]] <- held
    }
  }
  for (metric in metrics) {
    daily_frame_value <- regional_daily |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        value = if (all(!is.finite(.data[[metric]]))) {
          NA_real_
        } else max(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      )
    daily_frame_value$value[!is.finite(daily_frame_value$value)] <- NA_real_
    daily_indices <- pm25_validation_frame_indices(
      as.POSIXct(daily_frame_value$date, tz = "UTC"),
      daily_frame_value$value, daily_frame_limit
    )
    for (date in daily_frame_value$date[daily_indices]) {
      frame <- regional_daily[regional_daily$date == date, , drop = FALSE]
      values <- stats::setNames(frame[[metric]], frame$aqs_site_id)
      validate_frame(values, metric, as.POSIXct(date, tz = "UTC"))
    }
  }
  if (!is.null(regional_hourly) && length(regional_hourly$shards)) {
    hourly_rows <- lapply(regional_hourly$shards, function(shard) {
      frame_value <- apply(shard$values, 1, function(x) {
        if (all(!is.finite(x))) NA_real_ else max(x, na.rm = TRUE)
      })
      tibble::tibble(
        timestamp_utc = shard$timestamp_utc,
        frame_value = frame_value,
        shard_year = as.character(shard$year),
        shard_row = seq_along(shard$timestamp_utc)
      )
    }) |>
      dplyr::bind_rows()
    indices <- pm25_validation_frame_indices(
      hourly_rows$timestamp_utc, hourly_rows$frame_value, hourly_frame_limit
    )
    for (i in indices) {
      row <- hourly_rows[i, , drop = FALSE]
      shard <- regional_hourly$shards[[row$shard_year]]
      values <- stats::setNames(
        as.numeric(shard$values[row$shard_row, ]), shard$site_ids
      )
      validate_frame(values, "hourly", row$timestamp_utc)
    }
  }
  expected_metrics <- c(metrics, if (is.null(regional_hourly)) character() else "hourly")
  expected <- tidyr::crossing(
    metric = expected_metrics,
    data_source = c("TCEQ", "Dallas AQMesh")
  )
  summary <- if (!length(results)) tibble::tibble(
    metric = character(), data_source = character(), observations = integer(),
    mae = numeric(), rmse = numeric(), bias = numeric()
  ) else dplyr::bind_rows(results) |>
    dplyr::group_by(.data$metric, .data$data_source) |>
    dplyr::summarise(
      observations = dplyr::n(),
      mae = mean(abs(.data$error)),
      rmse = sqrt(mean(.data$error^2)),
      bias = mean(.data$error),
      .groups = "drop"
    )
  expected |>
    dplyr::left_join(summary, by = c("metric", "data_source")) |>
    dplyr::mutate(observations = dplyr::coalesce(.data$observations, 0L))
}
