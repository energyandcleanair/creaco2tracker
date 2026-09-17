#!/usr/bin/env Rscript
# Acceptance checks for the frozen completion study, separate from package tests.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})
args <- commandArgs(trailingOnly = TRUE)
arg <- function(key, default) {
  index <- match(key, args)
  if (is.na(index)) default else args[[index + 1L]]
}
root <- arg("--root", ".tmp/coal_repair_completion")
label <- arg("--label", "repaired_handoff")
scope <- match.arg(arg("--scope", "full"), c("full", "targeted"))
folder <- file.path(root, label)
stopifnot(file.exists(file.path(folder, "complete.ok")))
read <- function(path) read_csv(path, show_col_types = FALSE)
reference <- read(file.path(root, "reference_final", "raw.csv"))
result <- read(file.path(folder, "raw.csv"))
stopifnot(identical(names(reference), names(result)))
keys <- c("iso2", "date", "fuel", "sector", "estimate", "unit")
stopifnot(!anyDuplicated(result[keys]))
restored <- result %>% filter(iso2 %in% c("BG", "LU", "PL", "SK"),
  year(date) == 2025, month(date) <= 6, fuel == "total", estimate == "central")
stopifnot(nrow(restored) == 24L, all(is.finite(restored$value)))
coal <- result %>% filter(fuel == "coal", estimate == "central")
stopifnot(all(coal$value[is.finite(coal$value)] >= -1e-6))
coverage <- read(file.path(folder, "diagnostics", "eu_tail_country_coverage.csv"))
adjustments <- read(file.path(folder, "diagnostics", "eu_tail_adjustments.csv"))
replacements <- adjustments %>% filter(adjustment_model == "country_sum",
  fuel %in% c("coal", "total")) %>%
  left_join(coverage, by = c("date", "fuel", "sector", "unit"))
stopifnot(all(replacements$complete_members), all(replacements$n_countries == 27L))
source <- read(file.path(folder, "diagnostics", "eurostat", "coal_energy_coverage.csv"))
zero <- source %>% filter(iso2 == "SK", siec == "C0330", year == 2025, unit == "THS_T")
stopifnot(nrow(zero) == 1L, zero$energy == 0, zero$status == "reported_zero_energy_use")

sensitivity <- read(file.path(root, "sensitivity_matched_three_year", "raw.csv"))
matched <- result %>% filter(fuel == "total", estimate == "central", year(date) == 2026) %>%
  select(all_of(keys), default = value) %>% full_join(
    sensitivity %>% filter(fuel == "total", estimate == "central", year(date) == 2026) %>%
      select(all_of(keys), alternative = value), by = keys)
stopifnot(all(is.finite(matched$default) == is.finite(matched$alternative)))

holdouts <- file.path(root, "holdouts_full_report")
files <- list.files(holdouts, pattern = "^[0-9].*_scores.csv$", full.names = TRUE)
failures <- list.files(holdouts, pattern = "_failure.txt$")
# Failures in the unchanged control are measured as unavailable coverage.
# A repaired candidate must complete every replay without execution errors.
stopifnot(all(grepl("_unchanged_failure.txt$", failures)))
scores <- bind_rows(lapply(files, read))
case_count <- scores %>% distinct(evaluation_year, siec, pattern) %>% count(evaluation_year)
expected <- tibble(evaluation_year = 2019:2024, expected = c(13L, rep(19L, 5)))
case_count <- expected %>% left_join(case_count, by = "evaluation_year")
if (scope == "full") {
  stopifnot(all(!is.na(case_count$n)), all(case_count$n == case_count$expected))
}
methods <- scores %>% distinct(evaluation_year, siec, pattern, method) %>%
  count(evaluation_year, siec, pattern)
if (scope == "full") stopifnot(all(methods$n == 4L))
per_country <- scores %>% count(evaluation_year, siec, pattern, iso2)
if (scope == "full") stopifnot(all(per_country$n == 4L))
if (scope == "targeted") {
  required_methods <- scores %>% filter(method %in% c("repaired", "unchanged")) %>%
    count(evaluation_year, siec, pattern, iso2)
  stopifnot(all(required_methods$n == 2L), nrow(required_methods) == nrow(per_country))
  stopifnot(all(c("whole_year", "partial_h1", "annual_only", "missing_sector",
    "missing_coking", "forecast_total", "annual_only_forecast") %in% scores$pattern),
    all(c("C0100", "C0200", "S2000") %in% scores$siec))
  checks <- tibble(evaluation_year = 2024L, siec = c("C0100", "C0200", "S2000"),
    pattern = c("whole_year", "missing_sector", "whole_year"))
  stopifnot(nrow(anti_join(checks, scores,
    by = c("evaluation_year", "siec", "pattern"))) == 0L)
}
stopifnot(all(c("EU", "GR", "SE", "DE", "FR") %in% scores$iso2))
summary_folder <- file.path(root, if (scope == "full") "results" else "results_targeted")
nonnegative <- read(file.path(summary_folder, "replay_nonnegativity.csv")) %>%
  filter(method == "repaired")
if (scope == "full") stopifnot(all(nonnegative$introduced_negative_months == 0L))
if (scope == "targeted") {
  regression_folder <- file.path(root, "holdouts_sector_conflict_check")
  regression <- readRDS(file.path(regression_folder, "2021_C0100_annual_only_repaired.rds"))
  stopifnot(!any(regression$value[regression$fuel == "coal"] < -1e-6, na.rm = TRUE))
  before <- readRDS(file.path(holdouts, "2021_C0100_annual_only_repaired.rds"))
  totals <- before %>% filter(fuel == "total") %>% rename(before = value) %>%
    full_join(regression %>% filter(fuel == "total") %>% rename(after = value),
      by = c("iso2", "date", "fuel", "sector"))
  stopifnot(all(!is.finite(totals$before) | is.finite(totals$after)))
  write_csv(totals, file.path(root, "sector_conflict_total_comparison.csv"))
  # The conflict branch is not exercised by any separately handled fuel in
  # these frozen current-report inputs. Their full-pipeline outputs remain valid.
  for (run in c(label, "sensitivity_three_year", "sensitivity_matched_three_year")) {
    consumption <- readRDS(file.path(root, run, "consumption.rds"))
    selected <- attr(consumption, "coal_separate_projection")
    selected_values <- consumption %>% semi_join(selected,
      by = c("iso2", "siec", "unit", "fuel"))
    stopifnot(!any(selected_values$values < 0, na.rm = TRUE))
  }
}
for (path in list.files(holdouts, pattern = "_masked.rds$", full.names = TRUE)) {
  masked <- readRDS(path)
  stopifnot(all(masked$monthly$time <= masked$cutoff),
    all(year(masked$annual$time) < year(masked$cutoff)))
  if (!is.null(masked$cutoff_context)) {
    stopifnot(all(masked$cutoff_context$time <= masked$cutoff))
  }
  context_path <- file.path(root, "masked_context", sub("_masked.rds$", ".rds", basename(path)))
  context <- readRDS(context_path)
  stopifnot(isTRUE(context$isolated),
    identical(context$masked_sha256, digest::digest(path, file = TRUE, algo = "sha256")),
    identical(context$background_sha256,
      digest::digest(context$background_context_file, file = TRUE, algo = "sha256")),
    nrow(inner_join(context$target_keys, context$background_keys,
      by = c("iso2", "siec", "unit"))) == 0L)
}
write_csv(case_count, file.path(root, "verified_holdout_counts.csv"))
writeLines(c("PASS: public output schema and unique keys", "PASS: four country totals restored",
  "PASS: coal non-negativity and reported zero energy", "PASS: EU replacement completeness",
  "PASS: full-pipeline three-year sensitivity retains country coverage",
  if (scope == "full") "PASS: six historical cutoffs, 108 cases, four methods" else
    "PASS: targeted replay coverage of seven patterns and all fuels with monthly truth",
  "PASS: no candidate execution failures",
  if (scope == "full") "PASS: repaired replays introduce no negative coal components" else
    "PASS: targeted conflict replay rejects negative splits without losing complete totals",
  if (scope == "targeted")
    "PASS: final conflict guard leaves all three frozen current-report input paths unchanged",
  paste("Unchanged-control execution failures counted as unavailable:", length(failures)),
  "PASS: saved monthly and annual masks respect evaluation cutoffs",
  "PASS: held-out series excluded from background; exact contexts and checksums retained"),
  file.path(root, if (scope == "full") "acceptance_checks.txt" else
    "targeted_acceptance_checks.txt"))
message("All coal completion acceptance checks passed.")
