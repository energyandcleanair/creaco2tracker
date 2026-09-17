#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(devtools)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(readr)
  library(tidyr)
})

load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default) {
  position <- match(name, args)
  if (is.na(position) || position == length(args)) default else args[[position + 1]]
}

input <- arg("--input", ".tmp/eu_coal_gap_validation_inputs_2026-09-16")
output <- arg("--output", ".tmp/eu_coal_gap_validation_2026-09-16")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write_out <- function(data, name) readr::write_csv(data, file.path(output, name), na = "")

required_files <- c("nrg_cb_sffm.parquet", "nrg_cb_sff.parquet", "iea_conversion.parquet")
if (!all(file.exists(file.path(input, required_files)))) {
  stop("The frozen validation snapshot is incomplete. Run the acquisition script first.")
}

monthly_raw <- arrow::read_parquet(file.path(input, "nrg_cb_sffm.parquet")) %>% add_iso2()
annual_raw <- arrow::read_parquet(file.path(input, "nrg_cb_sff.parquet")) %>% add_iso2()
conversion_raw <- arrow::read_parquet(file.path(input, "iea_conversion.parquet"))
invisible(file.copy(
  file.path(input, c("input_manifest.csv", "acquisition_settings.txt")),
  output,
  overwrite = TRUE
))

eu_iso2s <- get_eu_iso2s(include_eu = FALSE)
keys <- c("iso2", "siec", "nrg_bal", "unit")
target_balances <- COAL_MONTHLY_GAP_BALANCES
target_fuels <- COAL_MONTHLY_GAP_FUELS
solid_fuels <- c(target_fuels, SIEC_COKE_OVEN_COKE, SIEC_PEAT)
monthly_solid <- monthly_raw %>% filter(siec %in% solid_fuels, !is.na(iso2))
annual_solid <- annual_raw %>% filter(siec %in% solid_fuels, !is.na(iso2))

monthly <- monthly_raw %>%
  filter(iso2 %in% eu_iso2s, siec %in% target_fuels, unit == "THS_T") %>%
  transmute(iso2, siec, nrg_bal, unit, time = as.Date(time), value = values)
annual <- annual_raw %>%
  filter(iso2 %in% eu_iso2s, siec %in% target_fuels, unit == "THS_T") %>%
  transmute(iso2, siec, nrg_bal, unit, year = year(time), annual_value = values)
targets_reported <- monthly %>% filter(nrg_bal %in% target_balances, !is.na(value))
series <- targets_reported %>% distinct(across(all_of(keys)))

coverage <- monthly %>%
  filter(nrg_bal %in% c(target_balances, COAL_MONTHLY_SUPPLY_BALANCES)) %>%
  group_by(across(all_of(keys))) %>%
  summarise(
    first_reported = min(time[!is.na(value)], na.rm = TRUE),
    last_reported = max(time[!is.na(value)], na.rm = TRUE),
    reported_rows = sum(!is.na(value)),
    .groups = "drop"
  )
write_out(coverage, "source_coverage.csv")
write_out(
  tidyr::crossing(
    iso2 = eu_iso2s, siec = target_fuels, nrg_bal = target_balances, unit = "THS_T"
  ) %>%
    anti_join(series, by = keys) %>%
    mutate(reason = "series_unavailable_in_frozen_source"),
  "excluded_series.csv"
)

production_fill <- fill_raw_coal_monthly(monthly_solid, annual_solid)
production_provenance <- attr(production_fill, "coal_gap_provenance")
write_out(production_provenance, "actual_gap_provenance.csv")
write_out(attr(production_fill, "coal_gap_completeness"), "actual_gap_completeness.csv")
write_out(attr(production_fill, "coal_gap_reconciliation"), "annual_reconciliation.csv")

complete_target <- targets_reported %>%
  right_join(
    series %>%
      left_join(
        targets_reported %>%
          group_by(across(all_of(keys))) %>%
          summarise(first = min(time), last = max(time), .groups = "drop"),
        by = keys
      ),
    by = keys
  ) %>%
  group_by(across(all_of(keys))) %>%
  complete(time = seq(first(first), first(last), by = "month")) %>%
  ungroup() %>%
  mutate(reported = !is.na(value)) %>%
  select(all_of(keys), time, value, reported)

actual_patterns <- production_provenance %>%
  filter(
    !reported, gap_length <= 6,
    gap_start <= as.Date("2026-06-01"), gap_end >= as.Date("2026-01-01")
  ) %>%
  distinct(
    target_iso2 = iso2, siec, nrg_bal, gap_type,
    start_month = month(gap_start), gap_length
  ) %>%
  rename(scenario = gap_type) %>%
  mutate(kind = "actual_pattern")
stress_patterns <- tidyr::crossing(
  siec = target_fuels,
  nrg_bal = target_balances,
  scenario = c("internal", "trailing"),
  start_month = 1:12,
  gap_length = c(1L, 3L, 6L),
  kind = "stress",
  target_iso2 = NA_character_
)
patterns <- bind_rows(actual_patterns, stress_patterns) %>% distinct()

evaluation_years <- sort(unique(year(complete_target$time)))
evaluation_years <- tail(evaluation_years[evaluation_years < max(evaluation_years)], 8)
holdouts <- series %>%
  inner_join(patterns, by = c("siec", "nrg_bal"), relationship = "many-to-many") %>%
  tidyr::crossing(evaluation_year = evaluation_years) %>%
  mutate(
    gap_start = as.Date(sprintf("%d-%02d-01", evaluation_year, start_month)),
    gap_end = gap_start %m+% months(gap_length - 1L)
  ) %>%
  filter(kind == "stress" | iso2 == target_iso2) %>%
  filter(year(gap_end) == evaluation_year) %>%
  mutate(gap_id = row_number())

target_cells <- holdouts %>%
  select(gap_id, all_of(keys), kind, scenario, evaluation_year, gap_start, gap_end, gap_length) %>%
  rowwise() %>%
  mutate(time = list(seq(gap_start, gap_end, by = "month"))) %>%
  unnest(time) %>%
  ungroup() %>%
  left_join(
    complete_target %>% select(all_of(keys), time, actual = value, target_reported = reported),
    by = c(keys, "time")
  ) %>%
  group_by(gap_id) %>%
  filter(n() == first(gap_length), all(target_reported)) %>%
  ungroup()

valid_holdouts <- target_cells %>% distinct(gap_id)
holdouts <- holdouts %>% semi_join(valid_holdouts, by = "gap_id")
write_out(holdouts, "holdout_definitions.csv")

supply_month <- monthly %>%
  filter(nrg_bal %in% COAL_MONTHLY_SUPPLY_BALANCES) %>%
  group_by(iso2, siec, unit, time) %>%
  summarise(
    rows = n(),
    balances = n_distinct(nrg_bal),
    supply_value = if (rows == 4 && balances == 4 && all(!is.na(value))) {
      sum(case_when(
        nrg_bal == "IPRD" ~ value,
        nrg_bal == "IMP" ~ value,
        nrg_bal == "EXP" ~ -value,
        nrg_bal == "STK_CHG" ~ value
      ))
    } else {
      NA_real_
    },
    .groups = "drop"
  )
supply_year <- supply_month %>%
  mutate(year = year(time)) %>%
  group_by(iso2, siec, unit, year) %>%
  summarise(
    months = n_distinct(time),
    supply_value = if (months == 12 && all(!is.na(supply_value))) sum(supply_value) else NA_real_,
    .groups = "drop"
  ) %>%
  left_join(
    annual %>% filter(nrg_bal == "IC_CAL") %>% select(-nrg_bal),
    by = c("iso2", "siec", "unit", "year")
  ) %>%
  mutate(
    reconciled = case_when(
      is.na(supply_value) | is.na(annual_value) ~ FALSE,
      annual_value == 0 ~ abs(supply_value) <= 1e-6,
      TRUE ~ abs(supply_value - annual_value) / abs(annual_value) <= 0.05
    )
  )

accounting_is_eligible <- function(country, fuel, unit_value, target_year) {
  checks <- supply_year %>%
    filter(
      iso2 == country, siec == fuel, unit == unit_value,
      year < target_year
    ) %>%
    arrange(desc(year)) %>%
    slice_head(n = 3)
  nrow(checks) == 3 && all(checks$reconciled)
}

accounting_eligibility <- tidyr::crossing(
  series %>% filter(nrg_bal == "GID_CAL") %>% select(iso2, siec, unit),
  evaluation_year = evaluation_years
) %>%
  rowwise() %>%
  mutate(eligible = accounting_is_eligible(iso2, siec, unit, evaluation_year)) %>%
  ungroup()

reported_lookup <- complete_target %>%
  filter(reported) %>%
  select(all_of(keys), source_time = time, source_value = value)

previous <- target_cells %>%
  mutate(source_time = time %m-% years(1)) %>%
  left_join(reported_lookup, by = c(keys, "source_time")) %>%
  transmute(gap_id, time, predicted = source_value)

historical_average <- target_cells %>%
  tidyr::crossing(lag_year = 1:3) %>%
  mutate(source_time = time %m-% years(lag_year)) %>%
  left_join(reported_lookup, by = c(keys, "source_time")) %>%
  group_by(gap_id, time) %>%
  summarise(
    predicted = if (n() == 3 && all(!is.na(source_value))) mean(source_value) else NA_real_,
    .groups = "drop"
  )

interpolation_endpoints <- holdouts %>%
  filter(scenario == "internal") %>%
  mutate(
    before_time = gap_start %m-% months(1),
    after_time = gap_end %m+% months(1)
  ) %>%
  left_join(
    reported_lookup %>% rename(before_time = source_time, before_value = source_value),
    by = c(keys, "before_time")
  ) %>%
  left_join(
    reported_lookup %>% rename(after_time = source_time, after_value = source_value),
    by = c(keys, "after_time")
  ) %>%
  select(gap_id, before_value, after_value)
interpolation <- target_cells %>%
  left_join(interpolation_endpoints, by = "gap_id") %>%
  mutate(
    gap_position = 12 * (year(time) - year(gap_start)) + month(time) - month(gap_start) + 1,
    predicted = before_value + gap_position / (gap_length + 1) *
      (after_value - before_value)
  ) %>%
  select(gap_id, time, predicted)

accounting <- target_cells %>%
  left_join(
    accounting_eligibility,
    by = c("iso2", "siec", "unit", "evaluation_year")
  ) %>%
  left_join(
    supply_month %>% select(iso2, siec, unit, time, supply_value),
    by = c("iso2", "siec", "unit", "time")
  ) %>%
  mutate(
    predicted = if_else(
      nrg_bal == "GID_CAL" & coalesce(eligible, FALSE) & supply_value >= 0,
      supply_value,
      NA_real_
    )
  ) %>%
  select(gap_id, time, predicted)

candidate_values <- target_cells %>%
  select(
    gap_id, time, actual, iso2, siec, nrg_bal, unit, kind, scenario,
    evaluation_year, start_month = gap_start, gap_length
  ) %>%
  mutate(start_month = month(start_month)) %>%
  left_join(rename(previous, previous_year = predicted), by = c("gap_id", "time")) %>%
  left_join(
    rename(historical_average, three_year_average = predicted),
    by = c("gap_id", "time")
  ) %>%
  left_join(rename(interpolation, interpolation = predicted), by = c("gap_id", "time")) %>%
  left_join(rename(accounting, accounting = predicted), by = c("gap_id", "time")) %>%
  mutate(
    interpolation_enabled = .coal_monthly_interpolation_enabled(
      siec, nrg_bal, scenario
    ),
    production_interpolation = if_else(interpolation_enabled, interpolation, NA_real_),
    production_sequence = coalesce(accounting, production_interpolation, previous_year),
    selected_method = case_when(
      !is.na(accounting) ~ "accounting",
      !is.na(production_interpolation) ~ "interpolation",
      !is.na(previous_year) ~ "previous_year",
      TRUE ~ "unresolved"
    )
  )

predictions <- bind_rows(
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence
    )), method = "production_sequence", predicted = production_sequence),
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence, selected_method
    )), method = "accounting", predicted = accounting,
    selected_method = if_else(is.na(predicted), "unavailable", "accounting")),
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence, selected_method
    )), method = "interpolation", predicted = interpolation,
    selected_method = if_else(is.na(predicted), "unavailable", "interpolation")),
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence, selected_method
    )), method = "previous_year", predicted = previous_year,
    selected_method = if_else(is.na(predicted), "unavailable", "previous_year")),
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence, selected_method
    )), method = "three_year_average", predicted = three_year_average,
    selected_method = if_else(is.na(predicted), "unavailable", "three_year_average")),
  candidate_values %>%
    transmute(across(-c(
      previous_year, three_year_average, interpolation, accounting,
      interpolation_enabled, production_interpolation, production_sequence, selected_method
    )), method = "unchanged_pipeline", predicted = NA_real_,
    selected_method = "unresolved")
)
write_out(predictions, "candidate_predictions.csv")

scores <- predictions %>%
  group_by(iso2, siec, nrg_bal, kind, scenario, evaluation_year, method) %>%
  summarise(
    eligible_cells = sum(!is.na(predicted)),
    target_cells = n(),
    activity = sum(abs(actual)),
    covered_activity = sum(abs(actual[!is.na(predicted)])),
    monthly_mae = if (eligible_cells > 0) mean(abs(predicted - actual), na.rm = TRUE) else NA_real_,
    rmse = if (eligible_cells > 0) sqrt(mean((predicted - actual)^2, na.rm = TRUE)) else NA_real_,
    signed_bias = if (eligible_cells > 0) mean(predicted - actual, na.rm = TRUE) else NA_real_,
    gap_total_error = if (eligible_cells == target_cells) sum(predicted - actual) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(activity_coverage = covered_activity / activity)
write_out(scores, "model_scores.csv")

matched <- predictions %>%
  filter(method %in% c("production_sequence", "previous_year", "three_year_average")) %>%
  select(-selected_method) %>%
  pivot_wider(names_from = method, values_from = predicted) %>%
  filter(!is.na(production_sequence), !is.na(previous_year), !is.na(three_year_average)) %>%
  mutate(
    sequence_error = abs(production_sequence - actual),
    previous_error = abs(previous_year - actual),
    average_error = abs(three_year_average - actual)
  )
matched_scores <- matched %>%
  group_by(iso2, siec, nrg_bal, kind, scenario, evaluation_year) %>%
  summarise(
    activity = sum(abs(actual)),
    sequence_mae = mean(sequence_error),
    previous_mae = mean(previous_error),
    average_mae = mean(average_error),
    sequence_beats_previous = sequence_mae < previous_mae,
    sequence_beats_average = sequence_mae < average_mae,
    .groups = "drop"
  )
write_out(matched_scores, "matched_coverage_scores.csv")

policy_gates <- scores %>%
  filter(method == "production_sequence", kind == "stress") %>%
  group_by(siec, nrg_bal, scenario) %>%
  summarise(
    untouched_years = n_distinct(evaluation_year[eligible_cells > 0]),
    activity_coverage = sum(covered_activity) / sum(activity),
    .groups = "drop"
  ) %>%
  left_join(
    matched_scores %>%
      filter(kind == "stress") %>%
      group_by(siec, nrg_bal, scenario) %>%
      summarise(
        country_years = n(),
        share_beating_previous = mean(sequence_beats_previous),
        share_beating_average = mean(sequence_beats_average),
        .groups = "drop"
      ),
    by = c("siec", "nrg_bal", "scenario")
  ) %>%
  mutate(
    passes = untouched_years >= 5 & activity_coverage >= 0.70 &
      share_beating_previous > 0.5 & share_beating_average > 0.5
  )
write_out(policy_gates, "policy_gates.csv")

conversion <- process_conversion_factors(conversion_raw) %>%
  group_by(iso2, siec) %>%
  summarise(ncv_kjkg = mean(ncv_kjkg, na.rm = TRUE), .groups = "drop")
conversion_fallback <- conversion %>%
  group_by(siec) %>%
  summarise(fallback_ncv_kjkg = mean(ncv_kjkg, na.rm = TRUE), .groups = "drop")
co2_factors <- tidyr::crossing(iso2 = eu_iso2s, siec = target_fuels) %>%
  left_join(conversion, by = c("iso2", "siec")) %>%
  left_join(conversion_fallback, by = "siec") %>%
  mutate(ncv_kjkg = coalesce(ncv_kjkg, fallback_ncv_kjkg)) %>%
  left_join(get_ipcc_emission_factors(), by = "siec") %>%
  mutate(co2_t_per_thousand_tonnes = ncv_kjkg / 1000 * co2_factor_t_per_TJ) %>%
  select(iso2, siec, ncv_kjkg, co2_factor_t_per_TJ, co2_t_per_thousand_tonnes)
write_out(co2_factors, "conversion_factors.csv")

downstream_scores <- predictions %>%
  filter(month(time) <= 6) %>%
  left_join(co2_factors, by = c("iso2", "siec")) %>%
  mutate(
    total_multiplier = case_when(
      nrg_bal == "GID_CAL" ~ 1,
      nrg_bal == "TI_CO" ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
      nrg_bal == "TI_EHG_MAP" ~ 0,
      TRUE ~ NA_real_
    ),
    emissions_error_t = (predicted - actual) * total_multiplier * co2_t_per_thousand_tonnes
  ) %>%
  group_by(
    gap_id, iso2, siec, nrg_bal, kind, scenario, evaluation_year,
    start_month, gap_length, method
  ) %>%
  summarise(
    required_cells = n(),
    available_cells = sum(!is.na(emissions_error_t)),
    h1_emissions_error_t = if (available_cells == required_cells) {
      sum(emissions_error_t)
    } else {
      NA_real_
    },
    absolute_h1_yoy_change_error_t = abs(h1_emissions_error_t),
    .groups = "drop"
  )
write_out(downstream_scores, "downstream_h1_scores.csv")

actual_pattern_errors <- predictions %>%
  filter(
    kind == "actual_pattern", method == "production_sequence",
    month(time) <= 6, !is.na(predicted)
  ) %>%
  left_join(co2_factors, by = c("iso2", "siec")) %>%
  mutate(
    total_multiplier = case_when(
      nrg_bal == "GID_CAL" ~ 1,
      nrg_bal == "TI_CO" ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
      nrg_bal == "TI_EHG_MAP" ~ 0,
      TRUE ~ NA_real_
    ),
    emissions_error_t = (predicted - actual) * total_multiplier *
      co2_t_per_thousand_tonnes
  ) %>%
  group_by(evaluation_year) %>%
  summarise(emissions_error_t = sum_or_na(emissions_error_t), .groups = "drop")
empirical_interval <- actual_pattern_errors %>%
  summarise(
    historical_years = n_distinct(evaluation_year),
    lower_error_t = quantile(emissions_error_t, 0.05, na.rm = TRUE),
    upper_error_t = quantile(emissions_error_t, 0.95, na.rm = TRUE)
  )
write_out(actual_pattern_errors, "actual_pattern_historical_errors.csv")
write_out(empirical_interval, "actual_pattern_empirical_interval.csv")

build_solid <- function(raw) {
  monthly_processed <- process_solid_monthly(raw, pwr_generation = tibble()) %>%
    eurostat_split_solid_elec_others()
  annual_processed <- process_solid_yearly(annual_solid) %>%
    eurostat_split_solid_elec_others()
  yearly_monthly <- apply_seasonal_adjustment(annual_processed, monthly_processed)
  combine_monthly_yearly_with_cutoff(yearly_monthly, monthly_processed)
}

unchanged_solid <- build_solid(monthly_solid)
filled_solid <- build_solid(production_fill)
actual_fills_2026 <- production_provenance %>%
  filter(time >= as.Date("2026-01-01"), time <= as.Date("2026-06-01"), method != "reported")

h1_comparison <- full_join(
  unchanged_solid %>%
    filter(time >= as.Date("2026-01-01"), time <= as.Date("2026-06-01"), fuel == FUEL_COAL) %>%
    rename(unchanged_value = values, unchanged_source = source),
  filled_solid %>%
    filter(time >= as.Date("2026-01-01"), time <= as.Date("2026-06-01"), fuel == FUEL_COAL) %>%
    rename(filled_value = values, filled_source = source),
  by = c("iso2", "sector", "time", "unit", "siec", "fuel")
) %>%
  mutate(difference = filled_value - unchanged_value) %>%
  left_join(co2_factors, by = c("iso2", "siec")) %>%
  mutate(emissions_difference_t = difference * co2_t_per_thousand_tonnes)
write_out(h1_comparison, "h1_2026_downstream_comparison.csv")

fill_fates <- actual_fills_2026 %>%
  left_join(
    h1_comparison %>%
      group_by(iso2, siec, time) %>%
      summarise(
        unchanged_sources = paste(sort(unique(unchanged_source)), collapse = ","),
        filled_sources = paste(sort(unique(filled_source)), collapse = ","),
        downstream_rows = sum(!is.na(filled_value)),
        absolute_output_difference = sum(abs(difference), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("iso2", "siec", "time")
  ) %>%
  mutate(
    downstream_fate = case_when(
      method == "unresolved" ~ "remained_unresolved",
      is.na(downstream_rows) | downstream_rows == 0 ~ "discarded_missing_dependency",
      absolute_output_difference > 1e-9 ~ "changed_downstream_output",
      grepl("monthly", filled_sources) & grepl("yearly", unchanged_sources) ~
        "replaced_fallback_same_value",
      TRUE ~ "no_downstream_effect"
    )
  )
write_out(fill_fates, "h1_2026_fill_fates.csv")

plot_data <- scores %>%
  filter(kind == "stress", method != "unchanged_pipeline") %>%
  group_by(method, scenario) %>%
  summarise(
    monthly_mae = weighted.mean(monthly_mae, eligible_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(monthly_mae))
plot <- ggplot(plot_data, aes(method, monthly_mae, fill = scenario)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Coal gap-fill error by method",
    subtitle = "Monthly MAE | EU countries and four coal systems | Historical stress holdouts",
    x = NULL, y = "Thousand tonnes"
  ) +
  theme_minimal()
ggsave(file.path(output, "model_comparison.png"), plot, width = 9, height = 5, dpi = 150)

actual_summary <- actual_fills_2026 %>% count(method, name = "cells")
h1_delta <- sum(h1_comparison$emissions_difference_t, na.rm = TRUE)
passing_systems <- sum(policy_gates$passes, na.rm = TRUE)
failing_systems <- sum(!policy_gates$passes, na.rm = TRUE)
accounting_mae <- scores %>%
  filter(method == "accounting") %>%
  summarise(value = weighted.mean(monthly_mae, eligible_cells, na.rm = TRUE)) %>%
  pull(value)
sequence_mae <- scores %>%
  filter(method == "production_sequence") %>%
  summarise(value = weighted.mean(monthly_mae, eligible_cells, na.rm = TRUE)) %>%
  pull(value)
h1_sector_changes <- h1_comparison %>%
  group_by(sector) %>%
  summarise(value = sum(emissions_difference_t, na.rm = TRUE), .groups = "drop")
writeLines(
  c(
    "# EU coal gap-fill production validation", "",
    paste0("Frozen input: `", input, "`. All evaluation after acquisition ran offline."),
    paste0("Evaluation years: ", paste(evaluation_years, collapse = ", "), "."),
    paste0(
      "Historical holdouts use the current revised history and do not reconstruct ",
      "past publication vintages."
    ),
    "Accounting uses reported production, imports, exports and signed stock changes only.", "",
    "## Selected production policy", "",
    paste0(
      "Accounting remains the first choice for GID_CAL whenever its three-year ",
      "reconciliation gate passes."
    ),
    paste0(
      "Internal linear interpolation is enabled only for hard-coal GID_CAL. Other ",
      "internal gaps and all trailing gaps fall back to the reported previous-year month."
    ),
    paste0("Matched historical monthly MAE was ", round(accounting_mae, 3),
      " thousand tonnes for eligible accounting cells and ", round(sequence_mae, 1),
      " thousand tonnes for the complete production sequence."), "",
    "## Policy gates", "",
    paste0("Passing shared fuel/balance/scenario systems: ", passing_systems, "."),
    paste0("Failing shared systems: ", failing_systems, "."), "",
    "## H1 2026", "",
    paste0("Eligible raw H1 cells by method: ",
      paste(paste(actual_summary$method, actual_summary$cells, sep = "="), collapse = ", "), "."),
    paste0(
      "Raw-stage fill effect within the current downstream code: ", round(h1_delta),
      " tCO2 versus leaving the same raw cells missing."
    ),
    paste0("Sector changes: ", paste(
      paste(h1_sector_changes$sector, round(h1_sector_changes$value), sep = "="),
      collapse = ", "
    ), " tCO2."),
    paste0("Historical actual-pattern 90% error interval: ",
      round(empirical_interval$lower_error_t), " to ",
      round(empirical_interval$upper_error_t), " tCO2."),
    "Missing downstream output is recorded as missing coverage, never as zero error."
  ),
  file.path(output, "decision_note.md")
)

message(
  "Validation complete: ", nrow(holdouts), " holdouts and ",
  nrow(predictions), " predictions."
)
