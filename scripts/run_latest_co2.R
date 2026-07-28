#!/usr/bin/env Rscript

usage <- function() {
  cat(
    paste(
      "Run the latest working-tree version of the EU CO2 tracker.",
      "",
      "Usage:",
      "  run_latest_co2.R [--downscale] [--fresh]",
      "  run_latest_co2.R --help",
      "",
      "Options:",
      "  --downscale  Downscale the CO2 output to daily values.",
      "  --fresh      Bypass cached source data.",
      "  --help, -h   Show this help message.",
      "",
      paste0(
        "Outputs: diagnostics/full_run/<YYYY-MM-DD_HHMMSS>/",
        "co2_<YYYY-MM-DD_HHMMSS>.{parquet,csv}"
      ),
      sep = "\n"
    ),
    "\n"
  )
}

stop_usage <- function(...) {
  message("Error: ", paste(..., collapse = ""))
  usage()
  quit(status = 1)
}

parse_options <- function(args) {
  options <- list(downscale = FALSE, fresh = FALSE, help = FALSE)
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--help", "-h")) {
      options$help <- TRUE
      i <- i + 1
    } else if (arg == "--downscale") {
      options$downscale <- TRUE
      i <- i + 1
    } else if (arg == "--fresh") {
      options$fresh <- TRUE
      i <- i + 1
    } else {
      stop_usage("unknown argument: ", arg)
    }
  }

  options
}

default_output_paths <- function(run_started_at) {
  run_name <- format(run_started_at, "%Y-%m-%d_%H%M%S")
  output_dir <- file.path("diagnostics", "full_run", run_name)
  c(
    parquet = file.path(output_dir, paste0("co2_", run_name, ".parquet")),
    csv = file.path(output_dir, paste0("co2_", run_name, ".csv"))
  )
}

args <- commandArgs(trailingOnly = TRUE)
options <- parse_options(args)

if (options$help) {
  usage()
  quit(status = 0)
}

run_date <- Sys.Date()
run_started_at <- Sys.time()
outputs <- default_output_paths(run_started_at)

output_dir <- dirname(outputs[["parquet"]])
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_dir)) {
  stop("Failed to create output directory: ", output_dir)
}
if (any(dir.exists(outputs))) {
  stop("An output path is a directory: ", paste(outputs[dir.exists(outputs)], collapse = ", "))
}

message("[run_latest_co2.R] Loading package from the current working tree")
devtools::load_all(".", quiet = TRUE)

message(
  "[run_latest_co2.R] Running get_co2(date_to = ",
  run_date,
  ", downscale_daily = ",
  options$downscale,
  ", use_cache = ",
  !options$fresh,
  ")"
)
co2 <- get_co2(
  date_to = run_date,
  downscale_daily = options$downscale,
  use_cache = !options$fresh
)

arrow::write_parquet(co2, outputs[["parquet"]])
readr::write_csv(co2, outputs[["csv"]])

output_info <- file.info(outputs)
invalid_outputs <- !file.exists(outputs) | is.na(output_info$size) | output_info$size <= 0
if (any(invalid_outputs)) {
  stop(
    "Failed to write non-empty output file(s): ",
    paste(outputs[invalid_outputs], collapse = ", ")
  )
}

message("[run_latest_co2.R] Run date: ", run_date)
for (output in outputs) {
  message("[run_latest_co2.R] Wrote: ", normalizePath(output, mustWork = TRUE))
}
