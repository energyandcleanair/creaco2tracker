#!/usr/bin/env Rscript
# Acquire once with source caches, then run both code versions offline.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(arrow)
})
args <- commandArgs(trailingOnly = TRUE)
option <- function(key, default) {
  i <- match(key, args)
  if (is.na(i)) default else args[[i + 1L]]
}
mode <- option("--mode", "replay")
root <- option("--output", ".tmp/coal_repair_completion")
code <- option("--code", ".")
label <- option("--label", "repaired")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(code, quiet = TRUE)
ns <- asNamespace("creaco2tracker")
replace_binding <- function(name, value) {
  unlockBinding(name, ns)
  assign(name, value, envir = ns)
  lockBinding(name, ns)
}
snapshot <- file.path(root, "inputs.rds")
frozen <- ".tmp/eu_coal_gap_validation_inputs_2026-09-16"
if (mode == "acquire") {
  if (!file.copy(file.path(frozen, "nrg_cb_sffm.parquet"),
    file.path(root, "reported_coal_monthly.parquet"), overwrite = TRUE)) {
    stop("Could not freeze the reported coal monthly extract")
  }
  original <- get("get_eurostat_from_code", ns)
  frozen_client <- function(code, ...) {
    if (code %in% c("nrg_cb_sff", "nrg_cb_sffm")) {
      return(read_parquet(file.path(frozen, paste0(code, ".parquet"))))
    }
    original(code = code, ...)
  }
  replace_binding("get_eurostat_from_code", frozen_client)
  settings <- list(date_to = as.Date("2026-06-30"), use_cache = TRUE)
  inputs <- list(
    consumption = eurostat_data_access_get_cons_sources(use_cache = TRUE),
    power = get_power_generation(
      date_to = settings$date_to, use_cache = TRUE, diagnostics_folder = NULL
    ),
    gas = get_gas_demand(
      iso2s = get_eu_iso2s(include_eu = TRUE), date_to = settings$date_to,
      use_cache = TRUE, correct_to_eurostat = TRUE, diagnostics_folder = NULL
    ),
    industry = get_eurostat_indprod(use_cache = TRUE, diagnostics_folder = NULL),
    conversion = read_parquet(file.path(frozen, "iea_conversion.parquet")),
    settings = settings,
    captured_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  saveRDS(inputs, snapshot)
  write_csv(tibble(
    source = names(inputs)[1:5],
    checksum = vapply(inputs[1:5], digest::digest, character(1), algo = "sha256"),
    captured_at_utc = inputs$captured_at_utc,
    acquisition = c("September 16 coal snapshot; other fuels from cache",
      "cached power pipeline", "cached gas pipeline", "cached industry",
      "September 16 conversion snapshot")
  ), file.path(root, "input_manifest.csv"))
  quit(status = 0)
}
inputs <- readRDS(snapshot)
replace_binding("eurostat_data_access_get_cons_sources", function(...) inputs$consumption)
replace_binding("get_power_generation", function(...) inputs$power)
replace_binding("get_gas_demand", function(...) inputs$gas)
replace_binding("get_eurostat_indprod", function(...) inputs$industry)
replace_binding("iea.get_conversion_factors", function(...) inputs$conversion)
for (name in c("diagnostic_eurostat_cons_yearly_monthly", "diagnostic_eurostat_cons",
  "diagnose_eu_vs_countries", "diagnose_ncv_data")) {
  replace_binding(name, function(...) invisible(NULL))
}
original_conversion <- get("get_co2_from_eurostat_cons", ns)
replace_binding("get_co2_from_eurostat_cons", function(eurostat_cons, ...) {
  if (mode %in% c("holdouts", "backgrounds")) return(original_conversion(eurostat_cons, ...))
  dir.create(file.path(root, label), recursive = TRUE, showWarnings = FALSE)
  saveRDS(eurostat_cons, file.path(root, label, "consumption.rds"))
  result <- original_conversion(eurostat_cons, ...)
  saveRDS(result, file.path(root, label, "unprojected.rds"))
  result
})
original_validation <- get("validate_co2", ns)
replace_binding("validate_co2", function(co2, ...) {
  original_validation(co2, diagnostics_folder = NULL)
})
# A missed source boundary must fail rather than make an unrecorded request.
for (name in c("get_eurostat_from_code", "ember.get_power_generation",
  "entsoe.get_power_generation")) {
  if (exists(name, ns, inherits = FALSE)) {
    replace_binding(name, function(...) stop("Unexpected source access during frozen replay"))
  }
}
if (mode == "holdouts") {
  source("scripts/experiments/2026-h1/coal_completion_replay_helpers.R", local = TRUE)
  run_coal_downstream_replays(inputs, root, option("--years", "2019:2024"))
  quit(status = 0)
}
if (mode == "backgrounds") {
  bounds <- as.integer(strsplit(option("--years", "2019:2024"), ":", fixed = TRUE)[[1]])
  source_inputs <- inputs
  folder <- file.path(root, "noncoal_backgrounds")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  logger::log_threshold(logger::WARN)
  for (yr in seq.int(min(bounds), max(bounds))) {
    path <- file.path(folder, paste0(yr, "_projected.rds"))
    if (file.exists(path)) next
    cutoff <- as.Date(sprintf("%d-06-30", yr))
    message("NONCOAL BACKGROUND ", yr)
    inputs <- source_inputs
    for (source in names(inputs$consumption)) {
      inputs$consumption[[source]]$monthly <- inputs$consumption[[source]]$monthly %>%
        filter(time <= cutoff)
      inputs$consumption[[source]]$yearly <- inputs$consumption[[source]]$yearly %>%
        filter(lubridate::year(time) < yr)
    }
    inputs$consumption$solid$monthly <- inputs$consumption$solid$monthly %>%
      filter(!siec %in% COAL_MONTHLY_GAP_FUELS)
    inputs$consumption$solid$yearly <- inputs$consumption$solid$yearly %>%
      filter(!siec %in% COAL_MONTHLY_GAP_FUELS)
    inputs$conversion <- inputs$conversion %>% filter(year < yr)
    inputs$power <- inputs$power %>% filter(date <= cutoff)
    inputs$gas <- inputs$gas %>% filter(date <= cutoff)
    inputs$industry <- inputs$industry %>% filter(time <= cutoff)
    consumption <- get_eurostat_cons(inputs$power, diagnostics_folder = NULL, use_cache = TRUE)
    converted <- get_co2_from_eurostat_cons(consumption, diagnostics_folder = NULL)
    projected <- project_until_now(converted, inputs$power, inputs$gas, inputs$industry,
      date_to = cutoff) %>% filter(date <= cutoff)
    stopifnot(!any(projected$fuel == FUEL_COAL))
    saveRDS(projected, path)
    saveRDS(list(cutoff = cutoff, conversion_year = max(inputs$conversion$year),
      input_manifest = read_csv(file.path(root, "input_manifest.csv"), show_col_types = FALSE)),
      file.path(folder, paste0(yr, "_manifest.rds")))
  }
  quit(status = 0)
}
forecast_method <- option("--forecast-method", "previous_year")
if (!forecast_method %in% c("previous_year", "three_year_average", "three_year_available")) {
  stop("Unsupported experimental total forecast method: ", forecast_method)
}
if (forecast_method != "previous_year") {
  original_total_forecast <- get("coal_prepare_total_forecasts", ns)
  replace_binding("coal_prepare_total_forecasts", function(x, date_to,
    diagnostics_folder = NULL, ...) {
    alternative <- original_total_forecast(x, date_to, diagnostics_folder = NULL,
      forecast_method = "three_year_average")
    if (forecast_method == "three_year_available") {
      previous <- original_total_forecast(x, date_to, diagnostics_folder = NULL)
      keys <- c("iso2", "siec", "unit", "fuel", "time")
      provenance <- attr(alternative, "coal_total_forecasts")
      missing <- provenance %>% filter(!is.finite(total)) %>% select(all_of(keys))
      attribute_names <- intersect(names(attributes(alternative)), c("coal_allocation",
        "coal_eu_repair_candidates", "coal_separate_projection"))
      attributes_to_keep <- attributes(alternative)[attribute_names]
      alternative <- bind_rows(anti_join(alternative, missing, by = keys),
        semi_join(previous, missing, by = keys))
      for (name in names(attributes_to_keep)) {
        attr(alternative, name) <- attributes_to_keep[[name]]
      }
      attr(alternative, "coal_total_forecasts") <- bind_rows(
        anti_join(provenance, missing, by = keys),
        semi_join(attr(previous, "coal_total_forecasts"), missing, by = keys)
      )
    }
    if (!is.null(diagnostics_folder)) {
      dir.create(diagnostics_folder, recursive = TRUE, showWarnings = FALSE)
      write_csv(attr(alternative, "coal_total_forecasts"),
        file.path(diagnostics_folder, "coal_total_forecasts.csv"))
    }
    alternative
  })
}
result <- get_co2(
  date_to = inputs$settings$date_to, use_cache = TRUE, downscale_daily = FALSE,
  diagnostics_folder = file.path(root, label, "diagnostics")
)
dir.create(file.path(root, label), recursive = TRUE, showWarnings = FALSE)
write_csv(result, file.path(root, label, "raw.csv"), na = "")
writeLines("complete", file.path(root, label, "complete.ok"))
