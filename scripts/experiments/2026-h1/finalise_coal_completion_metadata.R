#!/usr/bin/env Rscript
# Preserve case definitions and the source context used by each frozen replay.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})
args <- commandArgs(trailingOnly = TRUE)
position <- match("--root", args)
root <- if (is.na(position)) ".tmp/coal_repair_completion" else args[[position + 1L]]
folder <- file.path(root, "holdouts_full_report")
output <- file.path(root, "masked_context")
dir.create(output, showWarnings = FALSE)
source("R/model_coal_replay.R")
rows <- lapply(list.files(folder, pattern = "_masked.rds$", full.names = TRUE), function(path) {
  x <- readRDS(path)
  id <- sub("_masked.rds$", "", basename(path))
  yr <- as.integer(substr(id, 1, 4))
  fuel <- strsplit(id, "_")[[1]][2]
  pattern <- sub("^[0-9]+_[^_]+_", "", id)
  context <- x$cutoff_context
  if (is.null(context)) {
    stop("Replay lacks its exact source context and must be rerun: ", id)
  }
  provenance <- "Exact context preserved by the completed replay"
  background_file <- x$background_context_file
  background <- coal_replay_background(readRDS(background_file),
    x$selected_fuel, x$eligible_countries)
  background_keys <- distinct(background, iso2, siec, unit)
  target_keys <- distinct(x$monthly, iso2, siec, unit)
  stopifnot(nrow(inner_join(target_keys, background_keys,
    by = c("iso2", "siec", "unit"))) == 0L)
  allocation <- attr(background, "coal_allocation")
  if (!is.null(allocation)) {
    stopifnot(!any(allocation$siec == x$selected_fuel &
      allocation$iso2 %in% x$eligible_countries))
  }
  context_path <- file.path(output, paste0(id, ".rds"))
  saveRDS(list(
    cutoff_context = context, cutoff = x$cutoff, source = provenance,
    masked_file = path, masked_sha256 = digest::digest(path, file = TRUE, algo = "sha256"),
    background_context_file = background_file,
    background_sha256 = digest::digest(background_file, file = TRUE, algo = "sha256"),
    background_keys = background_keys, target_keys = target_keys,
    isolated = TRUE
  ), context_path)
  masked_year <- if (pattern %in% c("forecast_total", "annual_only_forecast")) yr else yr - 1L
  target <- year(x$monthly$time) == masked_year
  hidden <- if (pattern == "missing_coking") x$monthly$nrg_bal == "TI_CO" else if (pattern == "missing_sector") x$monthly$nrg_bal == "TI_EHG_MAP" else rep(TRUE, nrow(x$monthly))
  if (pattern %in% c("annual_only", "annual_only_forecast")) {
    target <-
      if (pattern == "annual_only") year(x$monthly$time) < yr else rep(TRUE, nrow(x$monthly))
  }
  if (pattern == "partial_h1") hidden <- hidden & month(x$monthly$time) <= 3
  dates <- x$monthly$time[target & hidden]
  tibble(
    holdout = id, year = yr, siec = fuel, pattern = pattern, countries = n_distinct(x$monthly$iso2),
    completed = file.exists(file.path(folder, paste0(id, "_scores.csv"))),
    cutoff = x$cutoff, mask_start = if (length(dates)) min(dates) else as.Date(NA),
    mask_end = if (length(dates)) max(dates) else as.Date(NA), masked_rows = length(dates),
    annual_power_hidden = pattern == "missing_sector", annual_coking_hidden = pattern == "missing_coking",
    masked_file = path, masked_sha256 = digest::digest(path, file = TRUE, algo = "sha256"),
    context_file = context_path, context_sha256 = digest::digest(context_path, file = TRUE, algo = "sha256")
  )
})
write_csv(bind_rows(rows), file.path(root, "holdout_definitions.csv"))
