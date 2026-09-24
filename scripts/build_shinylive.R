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

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  cache_arg <- grep("^--cache-dir=", args, value = TRUE)
  output_arg <- grep("^--output-dir=", args, value = TRUE)
  if (length(cache_arg) > 1L || length(output_arg) > 1L) {
    stop("Specify --cache-dir and --output-dir no more than once.")
  }
  unknown <- setdiff(args, c(cache_arg, output_arg))
  if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))

  require_packages(c("digest", "jsonlite", "shinylive"))
  cache_dir <- if (length(cache_arg)) {
    normalizePath(sub("^--cache-dir=", "", cache_arg), mustWork = TRUE)
  } else {
    file.path(root, "tceq_pm25_dashboard", "deploy_cache")
  }
  output_dir <- if (length(output_arg)) {
    normalizePath(sub("^--output-dir=", "", output_arg), mustWork = FALSE)
  } else {
    file.path(root, "_site")
  }

  required <- file.path(
    cache_dir,
    c("station_index.rds", "event_index.rds", "cache_manifest.json")
  )
  if (!all(file.exists(required))) {
    stop("Dashboard cache is incomplete: ", cache_dir)
  }
  stations <- readRDS(file.path(cache_dir, "station_index.rds"))
  if (nrow(stations) != 16L || anyNA(stations$latitude) ||
      anyNA(stations$longitude) || anyDuplicated(stations$aqs_site_id)) {
    stop("Dashboard cache must contain 16 unique sites with coordinates.")
  }
  site_paths <- file.path(cache_dir, stations$site_cache_file)
  if (!all(file.exists(site_paths))) {
    stop("Dashboard cache is missing one or more site bundles.")
  }
  hashes <- vapply(site_paths, sha256_file, character(1))
  if (!identical(unname(hashes), unname(stations$site_cache_sha256))) {
    stop("One or more site bundles do not match the station-index hashes.")
  }

  stage <- tempfile("dfw_pm25_shinylive_app_")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (path in c(
      "R", "config", "scripts", "www",
      file.path("data", "derived", "tceq_dfw_pm25_dashboard", "sites")
    )) {
    dir.create(file.path(stage, path), recursive = TRUE, showWarnings = FALSE)
  }

  copy_checked <- function(from, to) {
    if (!file.copy(from, to, overwrite = TRUE)) {
      stop("Could not copy ", from, " to ", to, ".")
    }
  }
  copy_checked(
    file.path(root, "tceq_pm25_dashboard", "app.R"),
    file.path(stage, "app.R")
  )
  copy_checked(
    file.path(root, "tceq_pm25_dashboard", "www", "styles.css"),
    file.path(stage, "www", "styles.css")
  )
  for (name in c("utils.R", "tceq_pm25_dashboard.R", "tceq_pm25_dashboard_app.R")) {
    copy_checked(file.path(root, "R", name), file.path(stage, "R", name))
  }
  copy_checked(
    file.path(root, "config", "config.yml"),
    file.path(stage, "config", "config.yml")
  )
  writeLines(
    "Marker file for the portable Shinylive application root.",
    file.path(stage, "scripts", "deployment_marker.txt")
  )

  cache_stage <- file.path(stage, "data", "derived", "tceq_dfw_pm25_dashboard")
  for (name in c("station_index.rds", "event_index.rds", "cache_manifest.json")) {
    copy_checked(file.path(cache_dir, name), file.path(cache_stage, name))
  }
  for (path in site_paths) {
    copy_checked(path, file.path(cache_stage, "sites", basename(path)))
  }

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

  startup_dir <- file.path(root, "deployment")
  startup_files <- c("startup.css", "startup.js")
  for (name in startup_files) {
    copy_checked(
      file.path(startup_dir, name),
      file.path(temporary_output, name)
    )
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
    "</head>",
    '  <link rel="stylesheet" href="./startup.css" />\n</head>',
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
  writeLines(index_html, index_path, useBytes = TRUE)

  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  if (!file.rename(temporary_output, output_dir)) {
    stop("Could not atomically replace Shinylive output at ", output_dir, ".")
  }
  message("Shinylive site ready: ", output_dir)
}

main()
