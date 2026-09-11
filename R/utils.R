find_repo_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "config", "config.yml")) &&
        dir.exists(file.path(here, "R")) &&
        dir.exists(file.path(here, "scripts"))) {
      return(here)
    }
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
  cfg$paths$data_root <- root
  cfg
}

require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Restore renv.lock or install them before continuing."
    )
  }
  invisible(TRUE)
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  require_packages("digest")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
