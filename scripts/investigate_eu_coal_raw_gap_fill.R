#!/usr/bin/env Rscript
# Offline raw-Eurostat coal gap-filling study. It never changes production code.

suppressPackageStartupMessages({
  library(devtools); library(dplyr); library(readr); library(tidyr); library(lubridate)
})
load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default) {
  at <- match(name, args)
  if (is.na(at) || at == length(args)) default else args[at + 1]
}
output <- arg("--output", ".tmp/eu_coal_raw_gap_fill")
max_holdouts <- as.integer(arg("--max-holdouts", "0"))
cutoff <- as.Date(arg("--cutoff", "2026-09-16"))
refresh <- "--refresh" %in% args
balance_filter <- strsplit(arg("--balances", ""), ",", fixed = TRUE)[[1]]
balance_filter <- balance_filter[nzchar(balance_filter)]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write_out <- function(x, name) readr::write_csv(x, file.path(output, name), na = "")
methods_for <- function(scenario, balance) c(
  if (balance == "GID_CAL") "accounting",
  if (scenario == "internal") c("bidirectional_ets", "interpolation"),
  "forward_ets", "linear", "historical_average", "previous_year"
)

monthly_path <- arg("--monthly-path", list.files("cache", pattern = "^eurostat_nrg_cb_sffm.*parquet$", full.names = TRUE)[1])
annual_path <- arg("--annual-path", list.files("cache", pattern = "^eurostat_nrg_cb_sff_.*parquet$", full.names = TRUE)[1])
if (is.na(monthly_path) || is.na(annual_path)) stop("Raw Eurostat caches are required")
monthly_raw <- arrow::read_parquet(monthly_path)
if ((refresh || max(monthly_raw$time) < cutoff) && !"--use-frozen" %in% args) {
  message("Refreshing nrg_cb_sffm to the requested common cutoff.")
  monthly_raw <- get_eurostat_from_code("nrg_cb_sffm", use_cache = FALSE)
  monthly_path <- list.files("cache", pattern = "^eurostat_nrg_cb_sffm.*parquet$", full.names = TRUE)[1]
}
annual_raw <- arrow::read_parquet(annual_path)
frozen_monthly_path <- file.path(output, "nrg_cb_sffm_frozen.parquet")
frozen_annual_path <- file.path(output, "nrg_cb_sff_frozen.parquet")
arrow::write_parquet(monthly_raw, frozen_monthly_path)
arrow::write_parquet(annual_raw, frozen_annual_path)
prepared <- .coal_gap_prepare(monthly_raw, annual_raw)
monthly <- .coal_gap_complete_series(prepared$monthly)
annual <- prepared$annual
write_out(tibble(
  source = c("nrg_cb_sffm", "nrg_cb_sff"), path = c(frozen_monthly_path, frozen_annual_path),
  checksum = unname(tools::md5sum(c(frozen_monthly_path, frozen_annual_path))),
  cached_latest = c(as.character(max(monthly_raw$time)), as.character(max(annual_raw$time))),
  requested_cutoff = as.character(cutoff), extracted_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  revision_note = "Current revised history is used; historical publication vintages are not reproduced."
), "frozen_inputs.csv")
write_out(monthly %>% count(iso2, siec, nrg_bal, unit, name = "reported_rows"), "source_coverage.csv")
expected <- tidyr::crossing(iso2 = get_eu_iso2s(FALSE), siec = .coal_gap_fuels,
  nrg_bal = sort(unique(monthly$nrg_bal)), unit = "THS_T")
write_out(expected %>% anti_join(monthly %>% distinct(iso2, siec, nrg_bal, unit),
  by = c("iso2", "siec", "nrg_bal", "unit")) %>% mutate(reason = "series unavailable in frozen source"),
  "excluded_country_fuel_balance_series.csv")

actual_runs <- .coal_gap_runs(monthly)
write_out(actual_runs, "actual_gap_runs.csv")
keys <- .coal_gap_keys()
synthetic <- monthly %>% filter(reported) %>% group_by(across(all_of(keys))) %>%
  group_modify(function(x, key) {
    starts <- x$time[x$time >= min(x$time) %m+% years(5) & x$time <= max(x$time) %m-% months(6)]
    tidyr::crossing(start = starts, length = c(1L, 3L, 6L)) %>%
      filter(start %m+% months(length - 1) <= max(x$time), year(start) < year(max(x$time)))
  }) %>% ungroup() %>% mutate(kind = "stress", scenario = "internal")
forecast_stress <- synthetic %>% group_by(across(all_of(keys))) %>%
  filter(start <= max(start) %m-% months(36)) %>% ungroup() %>% mutate(scenario = "forecast")
holdouts <- bind_rows(
  synthetic, forecast_stress,
  actual_runs %>% left_join(monthly %>% group_by(across(all_of(keys))) %>% summarise(last = max(time), .groups = "drop"), by = keys) %>%
    transmute(across(all_of(keys)), start, length, kind, scenario = if_else(end < last, "internal", "forecast"))
)
if (length(balance_filter) > 0) holdouts <- holdouts %>% filter(nrg_bal %in% balance_filter)
holdouts <- holdouts %>% mutate(gap_id = row_number())
if (max_holdouts > 0 && nrow(holdouts) > max_holdouts) {
  # Cycle through systems before taking their second, third, and later replay.
  # This keeps bounded runs broad while still supporting untouched-year checks.
  holdouts <- holdouts %>% mutate(replay_year = lubridate::year(start)) %>%
    group_by(iso2, siec, nrg_bal, scenario, length, kind, replay_year) %>%
    slice_head(n = 1) %>% ungroup() %>%
    group_by(iso2, siec, nrg_bal, scenario, length, kind) %>%
    arrange(replay_year, .by_group = TRUE) %>% mutate(replay_order = row_number()) %>%
    ungroup() %>% arrange(replay_order, siec, iso2, nrg_bal, scenario, length, kind) %>%
    slice_head(n = max_holdouts) %>% select(-replay_order, -replay_year)
}
write_out(holdouts, "masked_holdouts.csv")

evaluate <- function(h) {
  x <- monthly %>% filter(iso2 == h$iso2, siec == h$siec, nrg_bal == h$nrg_bal, unit == h$unit)
  masked <- .coal_gap_mask(x, h$start, h$length, h$scenario)
  if (is.null(masked)) return(tibble())
  attr(masked, "supply_inputs") <- monthly %>% filter(iso2 == h$iso2, siec == h$siec, unit == h$unit,
    nrg_bal %in% c("IPRD", "IMP", "EXP", "STK_CHG"))
  annual_series <- annual %>% filter(iso2 == h$iso2, siec == h$siec, unit == h$unit)
  target_dates <- seq(h$start, by = "month", length.out = h$length)
  actual <- masked %>% filter(time %in% target_dates) %>% arrange(time) %>% pull(observed_value)
  effective_length <- h$length
  bind_rows(lapply(methods_for(h$scenario, h$nrg_bal), function(method) {
    fit <- .coal_gap_predict(masked, annual_series, h$start, effective_length, h$scenario, method)
    if (is.null(fit)) return(tibble())
    tibble(gap_id = h$gap_id, iso2 = h$iso2, siec = h$siec, nrg_bal = h$nrg_bal, unit = h$unit,
      kind = h$kind, scenario = h$scenario, start = h$start, length = effective_length,
      time = seq(h$start, by = "month", length.out = effective_length), actual, predicted = fit$values,
      method, clipped = fit$clipped, ets_specification = fit$specification)
  }))
}
predictions <- bind_rows(lapply(split(holdouts, holdouts$gap_id), function(h) evaluate(h[1, ])))
write_out(predictions, "candidate_predictions.csv")
write_out(.coal_gap_score(predictions), "raw_gap_scores.csv")
ranked <- predictions %>% filter(!is.na(actual), !is.na(predicted)) %>%
  group_by(nrg_bal, scenario, method) %>%
  summarise(monthly_mae = mean(abs(predicted - actual)), h1_rmse = sqrt(mean((predicted - actual)^2)),
    signed_bias = mean(predicted - actual), observations = n(), .groups = "drop") %>%
  arrange(nrg_bal, scenario, monthly_mae) %>% group_by(nrg_bal, scenario) %>% mutate(rank = row_number()) %>% ungroup()
write_out(ranked, "model_score_table.csv")
write_out(ranked %>% arrange(nrg_bal, scenario, rank) %>% group_by(nrg_bal, scenario) %>%
  summarise(sequence = paste(method, collapse = " -> "), .groups = "drop"), "fallback_sequences.csv")
write_out(.coal_gap_reconcile(monthly, annual), "annual_reconciliation.csv")
writeLines(c(
  "# Raw Eurostat coal gap-filling experiment", "",
  sprintf("Requested cutoff: %s. Raw monthly data available through: %s.", cutoff, max(monthly_raw$time)),
  "Current revised history is used; publication-vintage revisions are not reproduced.", "",
  "Candidate scores use matched raw observations. The reported sequence is descriptive until it passes the protocol's chronological five-year, majority-country/year and 70% activity gates.",
  "Annual balances are validation-only and are never used to constrain fills.",
  "", "## Downstream comparison", "",
  "The raw-stage study leaves the live pipeline unchanged. A downstream repeat must pass masked raw inputs through process_solid_monthly(), preserving its coking, sector and EU rules, and record when its legacy gap handler overwrites a raw fill."
), file.path(output, "decision_note.md"))
message("Wrote ", output, ": ", nrow(holdouts), " holdouts; ", nrow(predictions), " candidate predictions.")
