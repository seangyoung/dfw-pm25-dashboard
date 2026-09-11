#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
} else {
  normalizePath(getwd())
}
source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "tceq_pm25_dashboard.R"))
require_packages(c(
  "dplyr", "purrr", "shiny", "stringr", "tibble", "tidyr"
))
sys.source(file.path(root, "tests", "test_tceq_pm25_dashboard.R"), envir = globalenv())
message("All dashboard tests passed.")
