#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  position <- match(name, args)
  if (is.na(position) || position == length(args)) default else args[[position + 1]]
}

reference_path <- arg("--reference")
combined_path <- arg("--combined")
output <- arg("--output", ".tmp/coal_report_cases_2026-09-17")
prior_year <- as.integer(arg("--prior-year", "2025"))
current_year <- as.integer(arg("--current-year", "2026"))
target_fraction_2010 <- as.numeric(arg("--target-fraction-2010", "0.56"))

if (is.null(reference_path) || is.null(combined_path)) {
  stop("Supply --reference and --combined get_co2 CSV files.")
}
if (!all(file.exists(c(reference_path, combined_path)))) {
  stop("A requested get_co2 comparison input does not exist.")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)

read_run <- function(path, run) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date), run = run)
}
runs <- bind_rows(
  read_run(reference_path, "reference"),
  read_run(combined_path, "combined")
)
required_columns <- c(
  "iso2", "date", "fuel", "sector", "estimate", "value", "run"
)
if (!all(required_columns %in% names(runs))) {
  stop("The get_co2 comparison inputs do not have the expected schema.")
}

period_total <- function(run_name, target_year, months_required) {
  rows <- runs %>%
    filter(
      run == run_name,
      iso2 == "EU",
      fuel == "total",
      estimate == "central",
      year(date) == target_year,
      month(date) %in% months_required
    )
  if (nrow(rows) != length(months_required) || any(is.na(rows$value))) {
    stop(
      paste0(
        "EU total is incomplete for ", run_name, " ", target_year,
        " over requested months."
      )
    )
  }
  sum(rows$value) / 1e6
}

h1 <- crossing(
  run = c("reference", "combined"),
  year = c(prior_year, current_year)
) %>%
  rowwise() %>%
  mutate(value_mt = period_total(run, year, 1:6)) %>%
  ungroup()
annual <- crossing(run = c("reference", "combined"), year = c(2010L, prior_year)) %>%
  rowwise() %>%
  mutate(value_mt = period_total(run, year, 1:12)) %>%
  ungroup()

lookup_total <- function(data, run_name, target_year) {
  data$value_mt[data$run == run_name & data$year == target_year][[1]]
}
case_definitions <- tribble(
  ~case, ~prior_source, ~current_source,
  "unchanged_reference", "reference", "reference",
  "baseline_repair_only", "combined", "reference",
  "projection_change_only", "reference", "combined",
  "both_together", "combined", "combined"
)
cases <- case_definitions %>%
  rowwise() %>%
  mutate(
    h1_prior_mt = lookup_total(h1, prior_source, prior_year),
    h1_current_mt = lookup_total(h1, current_source, current_year),
    annual_2010_mt = lookup_total(annual, prior_source, 2010L),
    annual_prior_mt = lookup_total(annual, prior_source, prior_year),
    annual_2030_target_mt = target_fraction_2010 * annual_2010_mt,
    required_reduction_fraction = 1 -
      (annual_2030_target_mt / annual_prior_mt)^(1 / (2030 - prior_year)),
    h1_change_mt = h1_current_mt - h1_prior_mt,
    h1_change_pct = 100 * h1_change_mt / h1_prior_mt,
    required_reduction_pct = 100 * required_reduction_fraction,
    pace_ratio = -h1_change_pct / required_reduction_pct,
    pathway_h1_mt = h1_prior_mt * (1 - required_reduction_fraction),
    pathway_gap_mt = h1_current_mt - pathway_h1_mt,
    target_h1_mt = h1_prior_mt *
      (annual_2030_target_mt / annual_prior_mt),
    implied_target_year = if_else(
      h1_current_mt > 0 & h1_current_mt < h1_prior_mt,
      prior_year + log(target_h1_mt / h1_prior_mt) /
        log(h1_current_mt / h1_prior_mt),
      NA_real_
    )
  ) %>%
  ungroup()
write_csv(cases, file.path(output, "h1_report_cases.csv"), na = "")

component_changes <- runs %>%
  filter(
    iso2 == "EU",
    estimate == "central",
    fuel != "total",
    year(date) %in% c(prior_year, current_year),
    month(date) <= 6
  ) %>%
  group_by(run, year = year(date), fuel, sector) %>%
  summarise(
    months_available = sum(!is.na(value)),
    value_mt = if_else(months_available == 6, sum(value) / 1e6, NA_real_),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = c(months_available, value_mt)) %>%
  mutate(
    h1_change_mt = .data[[paste0("value_mt_", current_year)]] -
      .data[[paste0("value_mt_", prior_year)]]
  )
write_csv(component_changes, file.path(output, "h1_component_changes.csv"), na = "")

country_completeness <- runs %>%
  filter(
    fuel == "total",
    estimate == "central",
    year(date) %in% c(prior_year, current_year),
    month(date) <= 6
  ) %>%
  group_by(run, iso2, year = year(date)) %>%
  summarise(
    months_expected = 6L,
    months_available = sum(!is.na(value)),
    complete = months_available == months_expected,
    .groups = "drop"
  )
write_csv(
  country_completeness,
  file.path(output, "h1_country_completeness.csv"),
  na = ""
)

manifest <- tibble(
  role = c("reference", "combined"),
  path = normalizePath(c(reference_path, combined_path)),
  md5 = unname(tools::md5sum(c(reference_path, combined_path))),
  prior_year = prior_year,
  current_year = current_year,
  target_fraction_2010 = target_fraction_2010
)
write_csv(manifest, file.path(output, "input_manifest.csv"))

case_line <- function(case_name) {
  result <- cases %>% filter(case == case_name)
  paste0(
    "- **", gsub("_", " ", case_name), "**: ",
    sprintf("%+.2f Mt (%+.2f%%)", result$h1_change_mt, result$h1_change_pct),
    "; required pace ", sprintf("%.2f%%", result$required_reduction_pct),
    "; pace ratio ", sprintf("%.0f%%", 100 * result$pace_ratio),
    "; pathway gap ", sprintf("%+.2f Mt", result$pathway_gap_mt), "."
  )
}
reference <- cases %>% filter(case == "unchanged_reference")
combined <- cases %>% filter(case == "both_together")
writeLines(
  c(
    "# H1 coal repair report cases", "",
    paste0(
      "Comparison years: H1 ", prior_year, " and H1 ", current_year,
      ". All totals come from the two frozen get_co2 comparison outputs in the manifest."
    ),
    "", "## Cases", "",
    vapply(case_definitions$case, case_line, character(1)),
    "", "## Finding", "",
    paste0(
      "The combined coal policy changes the H1 year-on-year result from ",
      sprintf("%+.2f%%", reference$h1_change_pct), " to ",
      sprintf("%+.2f%%", combined$h1_change_pct), "."
    ),
    paste0(
      "The required annual reduction changes from ",
      sprintf("%.2f%%", reference$required_reduction_pct), " to ",
      sprintf("%.2f%%", combined$required_reduction_pct),
      " when the repaired full-year baseline is used."
    ),
    "", "## Interpretation limit", "",
    paste0(
      "Baseline-only and projection-only swap the prior-year denominator and current-year ",
      "numerator between the same two completed runs. They are sensitivity cases, not ",
      "independent refits, because the combined current-year projection can use repaired ",
      "history. The current source vintage does not reproduce the draft's 17.8 Mt result, ",
      "so these figures are not direct replacement text for the draft."
    )
  ),
  file.path(output, "decision_note.md")
)

message("Wrote report cases to ", output)
