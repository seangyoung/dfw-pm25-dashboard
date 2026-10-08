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

regional_files <- c(
  "regional_daily.rds", "regional_reach.rds", "regional_event_links.rds",
  "regional_hourly_index.rds", "spatial_support.rds", "spatial_validation.rds"
)

regional_manifest_files <- function(manifest) {
  unique(c(
    regional_files,
    if (!is.null(manifest$regional_analysis$hourly_files)) {
      unlist(manifest$regional_analysis$hourly_files, use.names = FALSE)
    } else character()
  ))
}

main <- function() {
args <- commandArgs(trailingOnly = TRUE)
sync_cache <- "--sync-cache" %in% args
validate_only <- "--validate-only" %in% args
cache_only <- "--cache-only" %in% args || validate_only
cache_arg <- grep("^--cache-dir=", args, value = TRUE)
output_arg <- grep("^--output-dir=", args, value = TRUE)
if (length(cache_arg) > 1L || length(output_arg) > 1L) {
  stop("Specify --cache-dir and --output-dir no more than once.")
}
known <- c(
  "--sync-cache", "--cache-only", "--validate-only", cache_arg, output_arg
)
unknown <- setdiff(args, known)
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

require_packages(c("digest", "jsonlite"))

deployment_cache <- file.path(
  root, "tceq_pm25_dashboard", "deploy_cache"
)
source_cache <- if (length(cache_arg)) {
  normalizePath(sub("^--cache-dir=", "", cache_arg), mustWork = TRUE)
} else if (sync_cache || !dir.exists(deployment_cache)) {
  require_packages("yaml")
  pm25_dashboard_cache_dir(read_config())
} else {
  deployment_cache
}
output_dir <- if (length(output_arg)) {
  normalizePath(sub("^--output-dir=", "", output_arg), mustWork = FALSE)
} else {
  file.path(root, "_site")
}

validate_cache <- function(cache_dir) {
  required <- file.path(
    cache_dir, c("station_index.rds", "event_index.rds", "cache_manifest.json")
  )
  if (!all(file.exists(required))) {
    stop("Dashboard cache is incomplete: ", cache_dir)
  }
  manifest <- jsonlite::read_json(
    file.path(cache_dir, "cache_manifest.json"), simplifyVector = TRUE
  )
  if (!is.null(manifest$regional_analysis)) {
    expected_regional <- regional_manifest_files(manifest)
    missing_regional <- expected_regional[
      !file.exists(file.path(cache_dir, expected_regional))
    ]
    if (length(missing_regional)) {
      stop("Dashboard cache is missing regional artifacts: ", paste(
        missing_regional, collapse = ", "
      ))
    }
    expected_hashes <- unlist(
      manifest$regional_analysis$file_sha256, use.names = TRUE
    )
    if (length(expected_hashes)) {
      hash_paths <- names(expected_hashes)
      if (is.null(hash_paths) || any(!nzchar(hash_paths)) ||
          any(grepl("^(/|[A-Za-z]:)", hash_paths))) {
        stop("Regional artifact hashes must use relative cache paths.")
      }
      actual_hashes <- vapply(
        file.path(cache_dir, hash_paths), sha256_file, character(1)
      )
      if (!identical(unname(actual_hashes), unname(expected_hashes))) {
        mismatch <- hash_paths[unname(actual_hashes) != unname(expected_hashes)]
        stop("Regional cache hash mismatch: ", paste(mismatch, collapse = ", "))
      }
    }
  }
  stations <- readRDS(file.path(cache_dir, "station_index.rds"))
  if (!nrow(stations) || anyNA(stations$latitude) ||
      anyNA(stations$longitude) || anyDuplicated(stations$aqs_site_id)) {
    stop("Dashboard cache must contain unique PM2.5 sites with coordinates.")
  }
  site_paths <- file.path(cache_dir, stations$site_cache_file)
  if (!all(file.exists(site_paths))) {
    stop("Dashboard cache is missing site bundles: ", paste(
      basename(site_paths[!file.exists(site_paths)]), collapse = ", "
    ))
  }
  actual_hashes <- vapply(site_paths, sha256_file, character(1))
  if (!identical(unname(actual_hashes), unname(stations$site_cache_sha256))) {
    stop("One or more site bundles do not match the station-index hashes.")
  }
  invisible(stations)
}

copy_cache_snapshot <- function(from, to) {
  stations <- validate_cache(from)
  parent <- dirname(to)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("deploy_cache_", tmpdir = parent)
  dir.create(file.path(temporary, "sites"), recursive = TRUE)
  source_manifest <- jsonlite::read_json(
    file.path(from, "cache_manifest.json"), simplifyVector = TRUE
  )
  top_files <- c("station_index.rds", "event_index.rds")
  if (!is.null(source_manifest$regional_analysis)) {
    top_files <- c(top_files, regional_manifest_files(source_manifest))
  }
  for (name in setdiff(top_files, "station_index.rds")) {
    destination <- file.path(temporary, name)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(from, name), destination)) {
      stop("Could not copy ", name, " into the deployment snapshot.")
    }
  }
  site_paths <- file.path(from, stations$site_cache_file)
  portable_site_paths <- file.path(temporary, "sites", basename(site_paths))
  for (i in seq_along(site_paths)) {
    bundle <- readRDS(site_paths[[i]])
    if (!is.null(bundle$provenance$raw_revision_archive)) {
      bundle$provenance$raw_revision_archive <-
        "Not included in the deployment bundle"
    }
    saveRDS(bundle, portable_site_paths[[i]], compress = "gzip")
  }
  portable_stations <- stations
  portable_stations$site_cache_sha256 <- vapply(
    portable_site_paths, sha256_file, character(1)
  )
  saveRDS(
    portable_stations, file.path(temporary, "station_index.rds"),
    compress = "gzip"
  )
  tceq_raw_manifest <- file.path(
    root, "data", "raw", "tceq_dfw_pm25_monthly", "download_manifest.csv"
  )
  require_packages("yaml")
  dallas_raw_manifest <- file.path(
    aqmesh_raw_dir(read_config()), "download_manifest.csv"
  )
  provenance <- c(
    tceq_download_manifest = tceq_raw_manifest,
    dallas_aqmesh_download_manifest = dallas_raw_manifest
  )
  provenance <- provenance[file.exists(provenance)]
  for (name in names(provenance)) {
    destination <- file.path(temporary, paste0(name, ".csv"))
    if (!file.copy(provenance[[name]], destination)) {
      stop("Could not add ", name, " to the deployment snapshot.")
    }
  }
  portable_cache_manifest <- source_manifest
  portable_cache_manifest$raw_data_dir <- "Not included in the deployment bundle"
  portable_cache_manifest$raw_manifest_path <- "tceq_download_manifest.csv"
  portable_cache_manifest$preparation_command <- paste(
    "Rscript --vanilla scripts/08i_update_dfw_pm25_dashboard.R"
  )
  if (!is.null(portable_cache_manifest$tceq$source_cache)) {
    portable_cache_manifest$tceq$source_cache <-
      "Not included in the deployment bundle"
  }
  if (!is.null(portable_cache_manifest$dallas_aqmesh$source_import)) {
    portable_cache_manifest$dallas_aqmesh$source_import <-
      "Not included in the deployment bundle"
  }
  jsonlite::write_json(
    portable_cache_manifest, file.path(temporary, "cache_manifest.json"),
    pretty = TRUE, auto_unbox = TRUE, null = "null"
  )
  deployment_manifest <- list(
    schema_version = 1L,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    station_count = nrow(stations),
    first_date = source_manifest$first_date,
    last_date = source_manifest$last_date,
    source_cache_manifest_sha256 = sha256_file(
      file.path(from, "cache_manifest.json")
    ),
    source_manifest_sha256 = as.list(vapply(provenance, sha256_file, character(1))),
    data_classification = paste(
      "Public TCEQ ambient-air monitoring data and public City of Dallas",
      "AQMesh community-sensor data"
    ),
    files = c(
      top_files, "cache_manifest.json", paste0(names(provenance), ".csv"),
      file.path("sites", basename(site_paths))
    )
  )
  jsonlite::write_json(
    deployment_manifest, file.path(temporary, "deployment_manifest.json"),
    pretty = TRUE, auto_unbox = TRUE
  )
  if (dir.exists(to)) unlink(to, recursive = TRUE)
  if (!file.rename(temporary, to)) {
    stop("Could not atomically replace the deployment cache at ", to, ".")
  }
  invisible(to)
}

if (sync_cache) {
  if (identical(
    normalizePath(source_cache, mustWork = TRUE),
    normalizePath(deployment_cache, mustWork = FALSE)
  )) {
    stop("--sync-cache requires a source cache outside deploy_cache.")
  }
  message("Updating the versioned deployment cache from ", source_cache, "...")
  copy_cache_snapshot(source_cache, deployment_cache)
  source_cache <- deployment_cache
}

stations <- validate_cache(source_cache)
if (cache_only) {
  message("Deployment cache validated: ", source_cache)
  return(invisible(source_cache))
}

require_packages("shinylive")

stage <- tempfile("tceq_pm25_shinylive_app_")
on.exit(unlink(stage, recursive = TRUE), add = TRUE)
for (path in c(
    "R", "config", "scripts", "www",
    file.path("data", "derived", "dfw_pm25_dashboard", "sites")
  )) {
  dir.create(file.path(stage, path), recursive = TRUE, showWarnings = FALSE)
}

copy_checked <- function(from, to) {
  if (!file.copy(from, to, overwrite = TRUE)) {
    stop("Could not copy ", from, " to ", to, ".")
  }
}

copy_checked(file.path(root, "tceq_pm25_dashboard", "app.R"), file.path(stage, "app.R"))
copy_checked(
  file.path(root, "tceq_pm25_dashboard", "www", "styles.css"),
  file.path(stage, "www", "styles.css")
)
for (name in c(
  "utils.R", "tceq_pm25_dashboard.R", "dallas_aqmesh.R",
    "pm25_regional.R", "tceq_pm25_dashboard_app.R"
  )) {
  copy_checked(file.path(root, "R", name), file.path(stage, "R", name))
}
copy_checked(
  file.path(root, "config", "config.yml"), file.path(stage, "config", "config.yml")
)
writeLines(
  "This directory marker allows the portable app to locate its staged root.",
  file.path(stage, "scripts", "deployment_marker.txt")
)

cache_stage <- file.path(stage, "data", "derived", "dfw_pm25_dashboard")
stage_cache_files <- c("station_index.rds", "event_index.rds", "cache_manifest.json")
if (!is.null(jsonlite::read_json(
    file.path(source_cache, "cache_manifest.json"), simplifyVector = TRUE
  )$regional_analysis)) {
  stage_manifest <- jsonlite::read_json(
    file.path(source_cache, "cache_manifest.json"), simplifyVector = TRUE
  )
  stage_cache_files <- c(stage_cache_files, regional_manifest_files(stage_manifest))
}
for (name in stage_cache_files) {
  destination <- file.path(cache_stage, name)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  copy_checked(file.path(source_cache, name), destination)
}
for (path in file.path(source_cache, stations$site_cache_file)) {
  copy_checked(path, file.path(cache_stage, "sites", basename(path)))
}
# The download manifest remains beside the versioned deployment snapshot for
# provenance. It is not embedded in app.json because Shinylive treats CSV as
# text and may normalize line endings, which would create a false stale-cache
# warning when compared with its byte-level SHA-256 hash.

dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
temporary_output <- tempfile("shinylive_site_", tmpdir = dirname(output_dir))
on.exit(unlink(temporary_output, recursive = TRUE), add = TRUE)
message("Exporting the browser-only dashboard with Shinylive...")
shinylive::export(
  stage, temporary_output,
  wasm_packages = TRUE,
  max_filesize = "200M",
  template_params = list(title = "DFW PM2.5 Monitor Explorer")
)

# Version the app payload and retain the explicit browser-compatibility startup
# screen used by the public GitHub Pages deployment.
app_json_path <- file.path(temporary_output, "app.json")
app_version <- substr(sha256_file(app_json_path), 1L, 12L)
runtime_path <- file.path(temporary_output, "shinylive", "shinylive.js")
runtime_js <- paste(readLines(runtime_path, warn = FALSE), collapse = "\n")
app_fetch <- 'fetch("./app.json")'
if (!grepl(app_fetch, runtime_js, fixed = TRUE)) {
  stop("Could not locate the app.json request in the exported Shinylive runtime.")
}
runtime_js <- sub(
  app_fetch, sprintf('fetch("./app.json?v=%s")', app_version),
  runtime_js, fixed = TRUE
)
writeLines(runtime_js, runtime_path, useBytes = TRUE)

startup_dir <- file.path(root, "deployment")
for (name in c("startup.css", "startup.js")) {
  copy_checked(file.path(startup_dir, name), file.path(temporary_output, name))
}
index_path <- file.path(temporary_output, "index.html")
index_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
if (!grepl("</head>", index_html, fixed = TRUE) ||
    !grepl("<body>", index_html, fixed = TRUE) ||
    !grepl("</body>", index_html, fixed = TRUE)) {
  stop("The exported Shinylive index does not contain the expected HTML tags.")
}
startup_markup <- paste0(
  '<div id="dfw-startup" role="status" aria-live="polite">',
  '<div class="dfw-startup-card">',
  '<div class="dfw-startup-spinner" aria-hidden="true"></div>',
  '<h1>DFW PM2.5 Monitor Explorer</h1>',
  '<p id="dfw-startup-status">Loading the browser-based R environment and monitoring data&hellip;</p>',
  '<p class="dfw-startup-note">First load may take 30&ndash;60 seconds. ',
  '<strong>Chrome or Edge is recommended.</strong> Safari may not load this WebAssembly application reliably.</p>',
  '</div></div>'
)
index_html <- sub(
  "</head>", '  <link rel="stylesheet" href="./startup.css" />\n</head>',
  index_html, fixed = TRUE
)
index_html <- sub(
  "<body>", paste0("<body>\n    ", startup_markup),
  index_html, fixed = TRUE
)
index_html <- sub(
  "</body>", '  <script src="./startup.js"></script>\n</body>',
  index_html, fixed = TRUE
)
versioned_assets <- c(
  "./shinylive/load-shinylive-sw.js", "./shinylive/shinylive.js",
  "./startup.css", "./startup.js"
)
for (asset in versioned_assets) {
  index_html <- gsub(
    asset, paste0(asset, "?v=", app_version), index_html, fixed = TRUE
  )
}
writeLines(index_html, index_path, useBytes = TRUE)

if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
if (!file.rename(temporary_output, output_dir)) {
  stop("Could not atomically replace Shinylive output at ", output_dir, ".")
}

message(
  "Shinylive site ready: ", output_dir, "\n",
  "Preview from the project root with:\n",
  "Rscript --vanilla -e 'httpuv::runStaticServer(\"_site\")'"
)
}

main()
