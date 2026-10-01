find_repo_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "config", "config.yml")) &&
        dir.exists(file.path(here, "R")) &&
        dir.exists(file.path(here, "scripts"))) return(here)

    parent <- dirname(here)
    if (identical(parent, here)) {
      stop("Could not locate the dashboard repository root.")
    }
    here <- parent
  }
}

read_config <- function(path = NULL) {
  root <- find_repo_root()
  if (is.null(path)) path <- file.path(root, "config", "config.yml")
  cfg <- yaml::read_yaml(path)
  cfg$repo_root <- root
  for (nm in names(cfg$paths)) {
    value <- cfg$paths[[nm]]
    if (nzchar(value) && !grepl("^(/|[A-Za-z]:[/\\\\])", value)) {
      cfg$paths[[nm]] <- file.path(root, value)
    }
  }

  # Large data can live outside the synced project. An environment variable
  # takes precedence over the machine-local YAML override; otherwise the
  # original project-relative paths remain the portable fallback.
  local_path <- file.path(root, "config", "local.yml")
  local_cfg <- if (file.exists(local_path)) yaml::read_yaml(local_path) else list()
  data_root <- Sys.getenv("TX_FIREHEALTH_DATA_ROOT", unset = "")
  if (!nzchar(data_root) && !is.null(local_cfg$data_root)) {
    data_root <- as.character(local_cfg$data_root)
  }
  if (nzchar(data_root)) {
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", data_root)) {
      data_root <- file.path(root, data_root)
    }
    data_root <- normalizePath(data_root, winslash = "/", mustWork = FALSE)
    for (nm in c("raw", "reference", "interim", "derived", "legacy")) {
      cfg$paths[[nm]] <- file.path(data_root, nm)
    }
  }
  if (!nzchar(data_root)) data_root <- dirname(cfg$paths$raw)
  cfg$paths$data_root <- normalizePath(
    data_root, winslash = "/", mustWork = FALSE
  )
  if (!is.null(local_cfg$secure_health_root) &&
      nzchar(as.character(local_cfg$secure_health_root))) {
    cfg$paths$secure_health_root <- normalizePath(
      as.character(local_cfg$secure_health_root), winslash = "/",
      mustWork = FALSE
    )
  }
  cfg
}

manifest_relative_path <- function(path, cfg) {
  root <- paste0(normalizePath(
    cfg$paths$data_root, winslash = "/", mustWork = FALSE
  ), "/")
  value <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (startsWith(value, root)) substring(value, nchar(root) + 1L) else value
}

resolve_manifest_path <- function(path, cfg) {
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) return(path)
  file.path(cfg$paths$data_root, path)
}

upsert_source_manifest <- function(cfg, rows) {
  path <- file.path(cfg$repo_root, "data", "source_manifest.csv")
  manifest <- data.table::fread(path)
  rows <- data.table::as.data.table(rows)
  required <- names(manifest)
  missing <- setdiff(required, names(rows))
  if (length(missing)) stop("Source-manifest rows lack fields: ", paste(missing, collapse = ", "))
  manifest <- manifest[!source_id %in% rows$source_id]
  data.table::fwrite(
    data.table::rbindlist(list(manifest, rows[, ..required]), fill = TRUE), path
  )
  invisible(path)
}

parse_selected_years <- function(args, configured_years) {
  configured_years <- as.integer(configured_years)
  inline <- grep("^--years=", args, value = TRUE)
  separate <- which(args == "--years")
  if (length(inline) > 1L || length(separate) > 1L ||
      (length(inline) && length(separate))) {
    stop("Specify --years only once.")
  }
  if (!length(inline) && !length(separate)) return(configured_years)

  if (length(inline)) {
    specification <- sub("^--years=", "", inline)
  } else {
    position <- separate[1]
    if (position == length(args) || startsWith(args[position + 1L], "--")) {
      stop("--years requires a comma-separated value, for example --years=2012,2024.")
    }
    specification <- args[position + 1L]
  }
  pieces <- trimws(strsplit(specification, ",", fixed = TRUE)[[1]])
  if (!length(pieces) || any(!nzchar(pieces)) ||
      any(!grepl("^[0-9]{4}$", pieces))) {
    stop("Invalid --years value. Use comma-separated four-digit years.")
  }
  years <- unique(as.integer(pieces))
  unsupported <- setdiff(years, configured_years)
  if (length(unsupported)) {
    stop("Requested years are not configured: ", paste(unsupported, collapse = ", "),
         ". Configured years are ", paste(configured_years, collapse = ", "), ".")
  }
  years
}

ensure_project_dirs <- function(cfg) {
  dirs <- unlist(cfg$paths[c(
    "raw", "reference", "interim", "derived", "legacy", "reports"
  )])
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required R packages: ", paste(missing, collapse = ", "),
         ". Install them before running this stage.")
  }
  invisible(TRUE)
}

read_table_auto <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    require_packages("readr")
    return(data.table::as.data.table(readr::read_csv(
      path, show_col_types = FALSE, progress = interactive(), guess_max = 100000L
    )))
  }
  data.table::fread(path)
}

write_metadata <- function(output_path, metadata) {
  sidecar <- paste0(output_path, ".metadata.json")
  metadata$created_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  jsonlite::write_json(metadata, sidecar, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")
  invisible(sidecar)
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  require_packages("digest")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

angular_difference <- function(a, b) {
  abs((a - b + 180) %% 360 - 180)
}

initial_bearing <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi / 180
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dl <- (lon2 - lon1) * to_rad
  y <- sin(dl) * cos(p2)
  x <- cos(p1) * sin(p2) - sin(p1) * cos(p2) * cos(dl)
  (atan2(y, x) / to_rad + 360) %% 360
}

great_circle_distance_m <- function(lon1, lat1, lon2, lat2,
                                    earth_radius_m = 6371008.8) {
  to_rad <- pi / 180
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dp <- (lat2 - lat1) * to_rad
  dl <- (lon2 - lon1) * to_rad
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * earth_radius_m * asin(pmin(1, sqrt(a)))
}
