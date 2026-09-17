#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(lubridate)
})
args <- commandArgs(trailingOnly = TRUE)
arg <- function(key, default) {
  i <- match(key, args)
  if (is.na(i)) default else args[[i + 1L]]
}
root <- arg("--root", ".tmp/coal_repair_completion")
label <- arg("--label", "repaired_handoff")
scope <- match.arg(arg("--scope", "full"), c("full", "targeted"))
out <- file.path(root, if (scope == "full") "results" else "results_targeted")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write <- function(x, name) write_csv(x, file.path(out, name), na = "")
runs <- bind_rows(lapply(c("reference_final", label), function(run) {
  read_csv(file.path(root, run, "raw.csv"),
    col_types = cols(unit = col_character()), show_col_types = FALSE) %>% mutate(run = run)
}))
coverage <- runs %>% filter(estimate == "central", fuel == "total", month(date) <= 6,
  year(date) %in% c(2025, 2026)) %>% group_by(run, iso2, year = year(date)) %>%
  summarise(months = n(), available = sum(!is.na(value)),
    h1_mt = if (n() == 6 && !anyNA(value)) sum(value) / 1e6 else NA_real_, .groups = "drop") %>%
  complete(run, iso2, year = c(2025L, 2026L),
    fill = list(months = 0L, available = 0L))
write(coverage, "country_completeness.csv")
sensitivity_labels <- c("sensitivity_three_year", "sensitivity_matched_three_year")
sensitivity_paths <- file.path(root, sensitivity_labels, "raw.csv")
if (all(file.exists(sensitivity_paths))) {
  sensitivity <- bind_rows(lapply(seq_along(sensitivity_paths), function(i) {
    read_csv(sensitivity_paths[i], col_types = cols(unit = col_character()),
      show_col_types = FALSE) %>% mutate(run = sensitivity_labels[i])
  }))
  forecast_cases <- bind_rows(runs %>% filter(run == label), sensitivity) %>%
    filter(estimate == "central", fuel == "total", month(date) <= 6,
      year(date) %in% c(2025L, 2026L)) %>%
    group_by(run, iso2, year = year(date)) %>% summarise(
      available_months = sum(is.finite(value)),
      h1_mt = if (n() == 6L && all(is.finite(value))) sum(value) / 1e6 else NA_real_,
      .groups = "drop") %>%
    group_by(run, iso2) %>% arrange(year) %>% mutate(
      yoy_change_mt = h1_mt - lag(h1_mt),
      yoy_change_pct = 100 * (h1_mt / lag(h1_mt) - 1)
    ) %>% ungroup()
  write(forecast_cases, "forecast_sensitivity_full_pipeline.csv")
  forecast_chart <- forecast_cases %>% filter(iso2 == "EU", year == 2026) %>%
    mutate(method = case_when(
      run == label ~ "Previous year",
      run == "sensitivity_three_year" ~ "Three-year only",
      TRUE ~ "Three-year with fallback"
    )) %>% ggplot(aes(method, yoy_change_pct)) + geom_point(size = 3) +
    labs(title = "Forecast choice and report impact",
      subtitle = "H1 2026 year-on-year change, % | EU emissions | Same frozen inputs",
      caption = paste("Three-year only leaves two additional country totals incomplete.",
        "The fallback comparison retains coverage."), x = NULL, y = NULL) + theme_minimal()
  ggsave(file.path(out, "forecast_sensitivity.png"), forecast_chart, width = 8, height = 5)
}
write(runs %>% filter(estimate == "central", is.na(value),
  year(date) %in% c(2025, 2026), month(date) <= 6), "remaining_missing_components.csv")
write(runs %>% filter(run == label, estimate == "central", fuel == "coal",
  sector == "unknown", year(date) %in% c(2025, 2026), month(date) <= 6) %>%
  group_by(iso2, year = year(date)) %>% summarise(
    months = n(), unresolved_months = sum(is.na(value)),
    known_unallocated_mt = sum(value, na.rm = TRUE) / 1e6, .groups = "drop"
  ), "unallocated_h1.csv")
projection <- read_csv(file.path(root, label, "diagnostics", "coal_projection_provenance.csv"),
  show_col_types = FALSE)
write(projection %>% filter(year(date) %in% c(2025, 2026), month(date) <= 6),
  "h1_projection_provenance.csv")
components <- read_csv(file.path(root, label, "diagnostics", "eurostat",
  "coal_component_completeness.csv"), show_col_types = FALSE)
write(components %>% filter(year(time) %in% c(2025, 2026), month(time) <= 6,
  !total_complete | !electricity_complete | !others_complete),
  "unresolved_input_components.csv")
energy <- read_csv(file.path(root, label, "diagnostics", "eurostat", "coal_energy_coverage.csv"),
  show_col_types = FALSE)
write(energy, "country_fuel_problem_table.csv")
coverage_chart <- energy %>% filter(year == 2025, unit == "THS_T", iso2 != "EU") %>%
  transmute(iso2, fuel = factor(siec, levels = c("C0100", "C0200", "C0330", "S2000"),
    labels = c("Hard coal", "Brown coal", "Briquettes", "Oil shale")),
    coverage = recode(coverage, monthly_reported = "Monthly reported",
      monthly_with_gaps = "Monthly gaps", annual_only = "Annual only",
      reported_zero_energy_use = "Annual energy reported zero", unresolved = "Unresolved")) %>%
  ggplot(aes(fuel, iso2, fill = coverage)) + geom_tile(colour = "white") +
  labs(title = "Where annual data carry the coal estimate",
    subtitle = "2025 energy use | EU member states | Annual and monthly source coverage",
    x = NULL, y = NULL, fill = NULL) + theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(out, "country_fuel_coverage.png"), coverage_chart, width = 9, height = 10)
detail <- read_csv(file.path(root, label, "diagnostics", "coal_separate_emissions.csv"),
  show_col_types = FALSE)
factors <- detail %>% mutate(year = year(time),
  factor = ncv_kjkg / 1000 * co2_factor_t_per_TJ) %>%
  distinct(iso2, siec, unit, year, factor)
allocation <- read_csv(file.path(root, label, "diagnostics", "eurostat", "coal_allocation.csv"),
  col_types = cols(profile_years = col_character()), show_col_types = FALSE)
applied_profiles <- allocation %>% mutate(year = year(time)) %>%
  group_by(iso2, siec, unit, year) %>% summarise(
    methods = paste(sort(unique(allocation_method)), collapse = ";"), .groups = "drop")
write(energy %>% filter(year == 2025, unit == "THS_T") %>%
  left_join(applied_profiles, by = c("iso2", "siec", "unit", "year")) %>%
  mutate(policy = case_when(
    coverage == "reported_zero_energy_use" ~ "preserve_zero_energy",
    coverage == "annual_only" ~ "annual_energy_remainder_allocation",
    coverage == "monthly_reported" ~ "preserve_usable_monthly_components",
    coverage == "monthly_with_gaps" ~ "shared_raw_and_annual_fallbacks",
    TRUE ~ "unresolved_required_energy_inputs"
  )), "country_fuel_applied_policy.csv")
timing <- allocation %>% filter(unit == "THS_T", year(time) == 2025,
  allocation_method == "equal_months") %>%
  group_by(iso2, siec, unit, year = year(time)) %>%
  summarise(allocated_annual_quantity = sum(values), .groups = "drop") %>%
  inner_join(factors, by = c("iso2", "siec", "unit", "year")) %>%
  mutate(annual_mt = allocated_annual_quantity * factor / 1e6,
    central_h1_mt = annual_mt / 2, feasible_h1_min_mt = 0, feasible_h1_max_mt = annual_mt,
    interpretation = "Timing bounds, not a confidence interval; monthly truth unavailable")
write(timing, "annual_only_timing_sensitivity.csv")
forecasts <- read_csv(file.path(root, label, "diagnostics", "eurostat", "coal_total_forecasts.csv"),
  show_col_types = FALSE) %>% mutate(year = year(time)) %>%
  left_join(factors, by = c("iso2", "siec", "unit", "year"))
spread <- forecasts %>% filter(year == 2026, month(time) <= 6,
  method == "previous_year_total") %>% group_by(iso2, siec, unit) %>%
  summarise(eligible_months = n(), matched_months = sum(!is.na(three_year_total)),
    previous_year_mt = sum(total[!is.na(three_year_total)] * factor[!is.na(three_year_total)]) / 1e6,
    three_year_mt = sum(three_year_total[!is.na(three_year_total)] *
      factor[!is.na(three_year_total)]) / 1e6, .groups = "drop") %>%
  mutate(previous_year_mt = if_else(matched_months > 0, previous_year_mt, NA_real_),
    three_year_mt = if_else(matched_months > 0, three_year_mt, NA_real_))
write(spread, "forecast_method_spread.csv")
score_files <- list.files(file.path(root, "holdouts_full_report"), pattern = "_scores.csv$",
  full.names = TRUE)
scores <- bind_rows(lapply(score_files, read_csv, show_col_types = FALSE))
finished <- scores %>% distinct(evaluation_year, siec, pattern) %>% count(evaluation_year)
expected <- tibble(evaluation_year = 2019:2024, expected = c(13L, rep(19L, 5))) %>%
  left_join(finished, by = "evaluation_year")
complete_study <- !any(is.na(expected$n)) && all(expected$n == expected$expected)
write(expected, "evaluation_completion.csv")
if (scope == "full" && !complete_study) {
  stop("Full-report historical evaluation is incomplete; do not publish partial rankings.")
}
failure_files <- list.files(file.path(root, "holdouts_full_report"),
  pattern = "_failure.txt$", full.names = TRUE)
write(tibble(file = basename(failure_files),
  error = vapply(failure_files, function(path) paste(readLines(path), collapse = "\n"),
    character(1))), "replay_execution_failures.csv")
if (nrow(scores) > 0) {
  nonnegative <- bind_rows(lapply(score_files, function(path) {
    case <- sub("_scores.csv$", "", basename(path))
    rows <- read_csv(path, show_col_types = FALSE)
    yr <- first(rows$evaluation_year)
    fuel <- first(rows$siec)
    select_coal <- function(data) data %>% filter(fuel == "coal",
      year(date) %in% c(yr - 1L, yr), month(date) <= 6, iso2 %in% rows$iso2) %>%
      select(iso2, date, sector, value)
    truth <- select_coal(readRDS(file.path(dirname(path), paste0(yr, "_", fuel, "_truth.rds"))))
    bind_rows(lapply(unique(rows$method), function(method) {
      prediction <- select_coal(readRDS(file.path(dirname(path),
        paste0(case, "_", method, ".rds"))))
      prediction %>% left_join(rename(truth, actual = value),
        by = c("iso2", "date", "sector")) %>% group_by(iso2) %>% summarise(
          negative_months = sum(value < -1e-6, na.rm = TRUE),
          introduced_negative_months = sum(value < -1e-6 &
            (is.na(actual) | actual >= -1e-6), na.rm = TRUE), .groups = "drop"
        ) %>% mutate(evaluation_year = yr, siec = fuel, pattern = first(rows$pattern),
          method = method)
    }))
  }))
  write(nonnegative, "replay_nonnegativity.csv")
  eu_report <- scores %>% filter(iso2 == "EU")
  write(eu_report, "eu_report_holdout_scores.csv")
  write(eu_report %>% filter(evaluation_year %in% 2020:2024) %>%
    group_by(siec, pattern, method) %>% summarise(
      evaluation_years = n(), available_years = sum(is.finite(yoy_error)),
      yoy_mae_mt = mean(abs(yoy_error), na.rm = TRUE) / 1e6,
      yoy_rmse_mt = sqrt(mean(yoy_error^2, na.rm = TRUE)) / 1e6,
      yoy_bias_mt = mean(yoy_error, na.rm = TRUE) / 1e6,
      level_mae_mt = mean(abs(level_error), na.rm = TRUE) / 1e6, .groups = "drop"
    ), "eu_report_method_scores.csv")
  write(eu_report %>% filter(evaluation_year %in% 2020:2024) %>%
    select(evaluation_year, siec, pattern, method, yoy_error) %>%
    pivot_wider(names_from = method, values_from = yoy_error) %>%
    filter(if_all(c(repaired, previous_year, three_year_average, unchanged), is.finite)) %>%
    pivot_longer(c(repaired, previous_year, three_year_average, unchanged),
      names_to = "method", values_to = "error") %>%
    group_by(siec, pattern, method) %>% summarise(
      matched_years = n(), mae_mt = mean(abs(error)) / 1e6,
      rmse_mt = sqrt(mean(error^2)) / 1e6, bias_mt = mean(error) / 1e6, .groups = "drop"
    ), "eu_report_matched_method_scores.csv")
  scores <- scores %>% filter(iso2 != "EU")
  # Coverage uses the target fuel's annual inland-consumption activity across
  # all member states, including countries excluded for missing monthly truth.
  # Other coal fuels in the joint projection must not inflate this denominator.
  bundle <- readRDS(file.path(root, "inputs.rds"))
  members <- setdiff(unique(energy$iso2), "EU")
  activity <- bundle$consumption$solid$yearly %>%
    filter(iso2 %in% members, nrg_bal == "IC_CAL", unit == "THS_T") %>%
    transmute(iso2, siec, year = year(time), quantity = values) %>%
    group_by(iso2, siec, year) %>% summarise(
      quantity = if (n() == 1L && all(is.finite(quantity))) abs(first(quantity)) else NA_real_,
      .groups = "drop")
  activity <- bind_rows(lapply(2019:2024, function(yr) {
    activity %>% filter(year %in% c(yr - 1L, yr)) %>% group_by(iso2, siec) %>%
      summarise(activity = if (n() == 2L && !anyNA(quantity)) sum(quantity) else NA_real_,
        .groups = "drop") %>% mutate(evaluation_year = yr)
  }))
  scores <- scores %>% select(-activity) %>%
    left_join(activity, by = c("iso2", "siec", "evaluation_year"))
  system_activity <- activity %>% filter(evaluation_year %in% 2020:2024) %>%
    group_by(siec) %>% summarise(system_activity = sum(activity, na.rm = TRUE),
      unknown_activity_country_years = sum(is.na(activity)), .groups = "drop")
  write(activity, "historical_activity_denominators.csv")
  write(scores, "holdout_scores.csv")
  write(scores %>% filter(evaluation_year %in% 2020:2024) %>%
    group_by(siec, pattern, method) %>% summarise(
    eligible_country_years = n(), available_country_years = sum(is.finite(yoy_error)),
    yoy_mae_t = mean(abs(yoy_error), na.rm = TRUE),
    yoy_rmse_t = sqrt(mean(yoy_error^2, na.rm = TRUE)),
    yoy_bias_t = mean(yoy_error, na.rm = TRUE),
    level_mae_t = mean(abs(level_error), na.rm = TRUE),
    level_rmse_t = sqrt(mean(level_error^2, na.rm = TRUE)),
    level_bias_t = mean(level_error, na.rm = TRUE), .groups = "drop"
  ), "downstream_scores_with_availability.csv")
  matched <- scores %>% select(iso2, evaluation_year, siec, pattern, method, yoy_error) %>%
    pivot_wider(names_from = method, values_from = yoy_error) %>%
    filter(is.finite(repaired), is.finite(previous_year), is.finite(three_year_average))
  write(matched, "matched_country_year_scores.csv")
  write(matched %>% filter(is.finite(unchanged), evaluation_year %in% 2020:2024) %>%
    pivot_longer(c(repaired, previous_year, three_year_average, unchanged),
      names_to = "method", values_to = "error") %>%
    group_by(siec, pattern, method) %>% summarise(
      matched_country_years = n(), mae_t = mean(abs(error)),
      rmse_t = sqrt(mean(error^2)), bias_t = mean(error), .groups = "drop"
    ), "matched_four_method_scores.csv")
  write(matched %>% filter(evaluation_year %in% 2020:2024) %>%
    pivot_longer(c(repaired, previous_year, three_year_average),
    names_to = "method", values_to = "error") %>%
    group_by(siec, pattern, method) %>% summarise(
      matched_country_years = n(), mae_t = mean(abs(error)),
      rmse_t = sqrt(mean(error^2)), bias_t = mean(error), .groups = "drop"
    ), "matched_downstream_scores.csv")
  unit_wins <- function(column, suffix) {
    matched %>% filter(evaluation_year %in% 2020:2024) %>%
      group_by(siec, pattern, across(all_of(column))) %>%
      summarise(repaired = mean(abs(repaired)), previous = mean(abs(previous_year)),
        average = mean(abs(three_year_average)), .groups = "drop") %>%
      group_by(siec, pattern) %>% summarise(
        !!paste0("win_", suffix, "_previous") := mean(repaired < previous - 1e-6),
        !!paste0("win_", suffix, "_average") := mean(repaired < average - 1e-6),
        .groups = "drop")
  }
  gates <- scores %>% filter(method == "repaired", evaluation_year %in% 2020:2024) %>%
    group_by(siec, pattern) %>% summarise(
      evaluation_years = n_distinct(evaluation_year[is.finite(yoy_error)]),
      recovered_activity = sum(activity[is.finite(yoy_error)], na.rm = TRUE),
      .groups = "drop"
    ) %>% left_join(system_activity, by = "siec") %>%
    mutate(activity_coverage = recovered_activity / system_activity) %>%
    left_join(matched %>% filter(evaluation_year %in% 2020:2024) %>%
      group_by(siec, pattern) %>% summarise(
        win_previous = mean(abs(repaired) < abs(previous_year) - 1e-6),
        win_average = mean(abs(repaired) < abs(three_year_average) - 1e-6),
        tie_previous = mean(abs(abs(repaired) - abs(previous_year)) <= 1e-6),
        tie_average = mean(abs(abs(repaired) - abs(three_year_average)) <= 1e-6),
        .groups = "drop"), by = c("siec", "pattern")) %>% mutate(
      passes = evaluation_years >= 5 & activity_coverage >= 0.70 &
        win_previous > 0.5 & win_average > 0.5,
      decision = case_when(
        pattern == "annual_only" ~ "authorised_annual_allocation_with_timing_sensitivity",
        passes ~ "eligible_for_shared_policy",
        TRUE ~ "no_new_statistical_policy"
      )
    )
  gates <- gates %>% left_join(unit_wins("iso2", "country"), by = c("siec", "pattern")) %>%
    left_join(unit_wins("evaluation_year", "year"), by = c("siec", "pattern")) %>%
    mutate(passes = passes & win_country_previous > 0.5 & win_country_average > 0.5 &
      win_year_previous > 0.5 & win_year_average > 0.5,
      decision = case_when(
        pattern == "annual_only" ~ "authorised_annual_allocation_with_timing_sensitivity",
        passes ~ "eligible_for_shared_policy",
        TRUE ~ "no_new_statistical_policy"
      ))
  gates <- gates %>% left_join(nonnegative %>% filter(method == "repaired", iso2 != "EU",
    evaluation_year %in% 2020:2024) %>% group_by(siec, pattern) %>%
    summarise(introduced_negative_months = sum(introduced_negative_months), .groups = "drop"),
    by = c("siec", "pattern")) %>% mutate(
      passes = passes & introduced_negative_months == 0L,
      decision = case_when(
        pattern == "annual_only" ~ "authorised_annual_allocation_with_timing_sensitivity",
        passes ~ "eligible_for_shared_policy",
        TRUE ~ "no_new_statistical_policy"
      ))
  if (scope == "targeted") {
    gates <- gates %>% mutate(passes = FALSE,
      decision = "no_statistical_promotion_targeted_acceptance_only")
  }
  write(gates, "selected_policy.csv")
  rolling <- bind_rows(lapply(2020:2024, function(yr) {
    matched %>% filter(evaluation_year < yr) %>%
      select(-unchanged) %>%
      pivot_longer(c(repaired, previous_year, three_year_average),
        names_to = "method", values_to = "yoy_error") %>%
      left_join(activity, by = c("iso2", "siec", "evaluation_year")) %>%
      group_by(siec, pattern, method) %>%
      summarise(mae = mean(abs(yoy_error)),
        available = sum(activity, na.rm = TRUE),
        .groups = "drop") %>%
      left_join(activity %>% filter(evaluation_year < yr) %>% group_by(siec) %>%
        summarise(possible = sum(activity, na.rm = TRUE), .groups = "drop"), by = "siec") %>%
      filter(is.finite(mae), available / possible >= 0.70) %>%
      group_by(siec, pattern) %>% arrange(mae, method) %>% slice(1) %>% ungroup() %>%
      mutate(evaluation_year = yr)
  }))
  write(rolling, "rolling_selection.csv")
  write(scores %>% inner_join(rolling %>% select(siec, pattern, method, evaluation_year),
    by = c("siec", "pattern", "method", "evaluation_year")), "untouched_selected_scores.csv")
  # Rebuild EU errors by year, preserving within-year country dependence.
  eu <- scores %>% group_by(evaluation_year, siec, pattern, method) %>% summarise(
    complete = all(is.finite(yoy_error)),
    error = if (all(is.finite(yoy_error))) sum(yoy_error) else NA_real_, .groups = "drop"
  )
  intervals <- eu %>% group_by(siec, pattern, method) %>%
    group_modify(function(group, keys) {
      bind_rows(lapply(sort(unique(group$evaluation_year)), function(yr) {
        past <- group$error[group$evaluation_year < yr & is.finite(group$error)]
        actual <- group$error[group$evaluation_year == yr]
        limits <- if (length(past) >= 3) quantile(past, c(0.05, 0.95)) else c(NA, NA)
        tibble(year = yr, lower_error = limits[1], upper_error = limits[2],
          error = actual, covered = actual >= limits[1] & actual <= limits[2],
          training_years = length(past))
      }))
    }) %>% ungroup()
  intervals <- intervals %>% mutate(scope = "covered_country_totals_with_other_fuels_fixed")
  write(intervals, "eu_year_grouped_intervals.csv")
  write(intervals %>% group_by(siec, pattern, method) %>% summarise(
    evaluated = sum(!is.na(covered)), achieved_coverage = mean(covered, na.rm = TRUE),
    .groups = "drop"), "interval_coverage.csv")
  chart <- matched %>% filter(evaluation_year %in% 2020:2024) %>%
    pivot_longer(c(repaired, previous_year, three_year_average),
    names_to = "method", values_to = "error") %>%
    ggplot(aes(method, abs(error) / 1e6, fill = method)) +
    geom_boxplot(outlier.alpha = 0.2) + facet_grid(pattern ~ siec, scales = "free_y",
      labeller = labeller(siec = c(C0100 = "Hard coal", C0200 = "Brown coal",
        S2000 = "Oil shale"), pattern = c(whole_year = "Whole baseline year",
        partial_h1 = "Three baseline months", annual_only = "Annual-only baseline",
        missing_sector = "Missing sector", missing_coking = "Missing coking",
        forecast_total = "H1 forecast", annual_only_forecast = "Annual-only forecast"))) +
    scale_x_discrete(labels = c(repaired = "Shared sequence", previous_year = "Previous year",
      three_year_average = "Three-year mean")) +
    labs(title = "Recovering H1 emissions changes",
      subtitle = "Absolute year-on-year error, Mt CO₂ | Matched country-years, 2020–2024",
      caption = if (scope == "targeted")
        "Incomplete diagnostic sample; this chart does not establish an overall ranking." else NULL,
      x = NULL, y = NULL) + theme_minimal() + theme(legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(file.path(out, "historical_errors.png"), chart, width = 12, height = 14)
}
chart <- coverage %>% filter(iso2 == "EU") %>%
  mutate(run = if_else(run == "reference_final", "Reference", "Coal repair")) %>%
  ggplot(aes(factor(year), h1_mt, fill = run)) + geom_col(position = "dodge") +
  labs(title = "H1 report sensitivity", subtitle = "EU emissions, Mt CO₂ | Frozen inputs",
    x = NULL, y = NULL, fill = NULL) + theme_minimal()
ggsave(file.path(out, "h1_report_impact.png"), chart, width = 8, height = 5)
chart <- timing %>% filter(iso2 != "EU", annual_mt > 0) %>%
  ggplot(aes(reorder(iso2, central_h1_mt), central_h1_mt)) + geom_point() +
  geom_linerange(aes(ymin = feasible_h1_min_mt, ymax = feasible_h1_max_mt)) + coord_flip() +
  labs(title = "Annual-only coal: uncertainty in timing",
    subtitle = "H1 2025, Mt CO₂ | Equal-month estimate and feasible bounds",
    caption = "Bounds are not a probability interval.", x = NULL, y = NULL) + theme_minimal()
ggsave(file.path(out, "annual_timing_sensitivity.png"), chart, width = 8, height = 6)
cases <- read_csv(file.path(root, "report_cases", "h1_report_cases.csv"),
  show_col_types = FALSE)
case_lines <- vapply(seq_len(nrow(cases)), function(i) {
  row <- cases[i, ]
  sprintf("- %s: %+.2f Mt (%+.2f%%); required annual pace %.2f%%; pace ratio %.0f%%.",
    gsub("_", " ", row$case), row$h1_change_mt, row$h1_change_pct,
    row$required_reduction_pct, 100 * row$pace_ratio)
}, character(1))
forecast_lines <- if (exists("forecast_cases")) {
  forecast_cases %>% filter(iso2 == "EU", year == 2026) %>%
    mutate(description = case_when(
      run == label ~ "Previous-year total forecast",
      run == "sensitivity_three_year" ~ "Pure three-year forecast (reduced country coverage)",
      TRUE ~ "Three-year where available, previous-year elsewhere (matched coverage)"
    )) %>% transmute(line = sprintf("- %s: H1 change %+.2f%%.",
      description, yoy_change_pct)) %>% pull(line)
} else "Forecast sensitivity has not completed."
policy_line <- if (scope == "targeted") {
  paste0("Targeted acceptance uses ", sum(finished$n),
    " completed replay cases alongside the full package suite and frozen all-EU production comparison. ",
    "These replays precede the final sector-conflict guard. The exhaustive study is paused; ",
    "historical errors are diagnostic only. ",
    "No statistical method or overall accuracy claim is promoted from this incomplete sample.")
} else if (nrow(scores) > 0) {
  paste0(sum(gates$passes %in% TRUE), " tested fuel/pattern systems meet every promotion gate. ",
    "See selected_policy.csv for coverage, country/year wins and ties. ",
    "A passing fixed-sequence comparison is not a validation of every fallback separately.")
} else "Historical evaluation is incomplete; no statistical method is promoted."
writeLines(c(
  "# Coal completion decision note",
  "",
  policy_line,
  "",
  "The annual-only energy path restores H1 2025 country totals for Bulgaria, Luxembourg, Poland and Slovakia.",
  "Positive inland consumption does not establish combustion: energy balances exclude statistical differences and non-energy use.",
  "Unallocated fuel totals remain distinct through conversion and forecasting, and rejoin once.",
  "If a derived sector is negative, the separately handled fuel retains its valid total as unallocated and records the conflicting split. A negative or incomplete total remains unresolved. A focused regression replay checks the final guard; prior replay scores are retained as pre-guard diagnostics.",
  "Slovakia's positive briquette inland balance is a statistical difference, with zero reported energy use; it produces zero combustion emissions.",
  "Bulgaria's known briquette energy remains unallocated where detailed electricity inputs are missing. Aggregate Ember generation does not identify that quantity.",
  "",
  "## H1 2026 sensitivity", "", case_lines,
  "",
  "These use H1 2025 as the baseline. The annual pathway benchmark is recomputed from each case's compatible 2010 and 2025 inputs.",
  "",
  "## Forecast sensitivity", "", forecast_lines, "",
  "Annual-only timing is an explicitly authorised allocation assumption. Its bounds are not confidence limits.",
  "Previous-year total forecasts and three-year averages are scored on matched historical coverage. The actual pure three-year forecast leaves Belgium and Ireland unresolved because their preceding three annual energy totals are incomplete; its EU headline also changes through the country-sum selector and is not a matched-coverage improvement. A separate three-year-where-available sensitivity retains the default estimate elsewhere and runs through the full pipeline.",
  "Raw fuel-to-CO2 equivalents are not used as downstream validation scores.",
  "Historical replays recover this tracker's accounting output using revised history; they do not recreate publication vintages.",
  "Reconstruction tests remove the previous year's whole-year, partial-year, sector or coking observations. Forecast tests remove the reporting H1. Complete annual balances are restricted to years before the reporting cutoff year.",
  "Other coal fuels are held fixed through the joint projection. Non-coal sources are processed and projected once for each cutoff, then held fixed through every candidate comparison. Alternative forecasts apply only to the masked system. Baseline forecasts cannot become reported accounting inputs or training observations.",
  "The full rolling design uses 2019 for initial selection and 2020–2024 for five subsequent evaluation years. evaluation_completion.csv records which cases actually completed. Coverage is measured against the target fuel's available annual inland-consumption activity, including excluded countries; unknown activity is listed separately.",
  "Raw monthly coal history expands from January 2008; the existing solid-fuel processor uses monthly observations from 2014 onward and preceding annual balances are retained. Fuel-quality inputs also stop before the cutoff year. The 2019 exclusions record unavailable oil-shale quality inputs.",
  "The replay runs solid-fuel processing, conversion, projection, fuel recombination, country totals and EU-tail selection. It is conditional on other fuels and supplied power/industry proxies, rather than a test of their accuracy. EU-series masking is an aggregate missing-data stress test; it does not reconstruct the original Greek omission's publication history.",
  "Every included case is rerun after correcting a study-only fuel-selection leak. Earlier scores in holdouts_background_leak_invalid and results_before_replay_isolation are invalid. Every stage now rejects overlap between held-out and unaffected fuel series; exact masked inputs and isolated background keys are retained.",
  "The rolling method selection compares shared-sequence, previous-year and three-year predictions on matched training observations; unchanged-pipeline controls are reported separately. Oil shale has no eligible 2019 training result, so its rolling selector has fewer than five untouched years even when the fixed comparison spans five years.",
  "Country-sum replay intervals group errors by year and apply only to covered systems. They are not intervals for the independently processed EU aggregate and do not validate briquette monthly timing.",
  "The EU tail now requires all member contributions before replacing a coal or all-fuel total with a country sum. Intermediate runs with partial country replacement are superseded; the final comparison preserves the EU estimate when those contributions are incomplete.",
  "",
  "Remaining incomplete country totals and components are listed separately. Missing observations are not zero emissions. Any execution failures count as unavailable coverage and are preserved in replay_execution_failures.csv.",
  "Sweden retains only one complete H1 2026 country-total month in both runs: unresolved coke and peat observations are outside these four coal-fuel rules.",
  "France's available final totals still depend partly on existing downstream estimates where annual coking and power detail are absent. The projection provenance lists these explicitly; complete output is not equivalent to reported inputs.",
  "The annual-only monthly allocation and previous-year total forecast are authorised assumptions, not claims of validated briquette monthly accuracy. The three-year forecast spread is a sensitivity; feasible annual timing bounds are not a probability interval.",
  "Inherited raw-balance policy switches remain separate from downstream promotion evidence. This study does not automatically enable additional statistical fallbacks.",
  "The H1 report cases use identical frozen inputs. Numerator/denominator swaps are sensitivities, not independent refits.",
  "The original draft's input/result discrepancy remains unresolved; these are not replacement draft figures."
), file.path(out, "decision_note.md"))
