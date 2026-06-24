#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
})

usage <- function() {
  cat(paste(
    "Usage:",
    "  collect_get_co2.R --output <raw.csv> --date-to <YYYY-MM-DD> --sha <sha>",
    sep = "\n"
  ), "\n")
}

stop_usage <- function(...) {
  message("Error: ", paste(..., collapse = ""))
  usage()
  quit(status = 1)
}

parse_options <- function(args) {
  opts <- list()
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      stop_usage("unexpected argument: ", arg)
    }
    if (i == length(args)) {
      stop_usage("missing value for option: ", arg)
    }

    name <- gsub("-", "_", sub("^--", "", arg))
    opts[[name]] <- args[[i + 1]]
    i <- i + 2
  }

  opts
}

require_option <- function(opts, name) {
  value <- opts[[name]]
  if (is.null(value) || !nzchar(value)) {
    stop_usage("missing required option --", gsub("_", "-", name))
  }
  value
}


run_collect <- function(opts) {
  output <- require_option(opts, "output")
  date_to <- as.Date(require_option(opts, "date_to"))
  sha <- require_option(opts, "sha")

  if (is.na(date_to)) {
    stop("Invalid --date-to value.")
  }

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  dir.create("cache", recursive = TRUE, showWarnings = FALSE)

  message("[collect_get_co2.R] Loading package at ", sha)
  devtools::load_all(".", quiet = TRUE)
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::TRACE)
  }
  message("[collect_get_co2.R] Running get_co2(date_to = ", date_to, ")")
  get_co2_args <- list()
  get_co2_formals <- names(formals(get_co2))
  add_arg_if_supported <- function(name, value) {
    if (name %in% get_co2_formals) {
      get_co2_args[name] <<- list(value)
    }
  }

  add_arg_if_supported("downscale_daily", FALSE)
  add_arg_if_supported("diagnostics_folder", NULL)
  add_arg_if_supported("date_to", date_to)
  add_arg_if_supported("use_cache", FALSE)

  co2 <- do.call(get_co2, get_co2_args)

  if (!"date_to" %in% names(get_co2_args) && "date" %in% names(co2)) {
    co2 <- co2 %>% filter(as.Date(date) <= date_to)
  }

  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "value")
  missing_cols <- setdiff(required_cols, names(co2))
  if (length(missing_cols) > 0) {
    stop(
      "get_co2 output is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  write_csv(co2, output, na = "")
  if (!file.exists(output) || file.info(output)$size <= 0) {
    stop("Failed to write usable raw output: ", output)
  }
  message("[collect_get_co2.R] Wrote ", output)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  usage()
  quit(status = 1)
}

run_collect(parse_options(args))
