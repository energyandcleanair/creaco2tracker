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
output <- arg("--output", ".tmp/coal_annual_reconstruction_validation_2026-09-17")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write_out <- function(data, name) write_csv(data, file.path(output, name), na = "")

required <- c("nrg_cb_sffm.parquet", "nrg_cb_sff.parquet", "iea_conversion.parquet")
if (!all(file.exists(file.path(input, required)))) {
  stop("The frozen validation snapshot is incomplete; no cached substitute is permitted.")
}

monthly_raw <- read_parquet(file.path(input, "nrg_cb_sffm.parquet")) %>% add_iso2()
annual_raw <- read_parquet(file.path(input, "nrg_cb_sff.parquet")) %>% add_iso2()
conversion_raw <- read_parquet(file.path(input, "iea_conversion.parquet"))
invisible(file.copy(
  file.path(input, c("input_manifest.csv", "acquisition_settings.txt")),
  output,
  overwrite = TRUE
))

eu_countries <- get_eu_iso2s(include_eu = FALSE)
annual <- annual_raw %>%
  filter(
    iso2 %in% eu_countries,
    siec %in% COAL_MONTHLY_GAP_FUELS,
    unit == "THS_T",
    nrg_bal %in% c("IC_CAL", "TI_CO_E", "TI_E", COAL_ANNUAL_POWER_BALANCES)
  )
monthly <- monthly_raw %>%
  filter(
    iso2 %in% eu_countries,
    siec %in% COAL_MONTHLY_GAP_FUELS,
    unit == "THS_T",
    nrg_bal %in% COAL_MONTHLY_GAP_BALANCES
  )
reported <- monthly %>%
  transmute(
    iso2, siec, nrg_bal, unit, time = as.Date(time), reported_value = values
  )

evaluation_years <- 2015:2024
patterns <- tribble(
  ~pattern, ~nrg_bal, ~months_hidden,
  "whole_year", "GID_CAL", 12L,
  "partial_h1", "GID_CAL", 6L,
  "missing_sector", "TI_EHG_MAP", 12L,
  "forecast_sector", "TI_EHG_MAP", 6L,
  "missing_coking", "TI_CO", 12L,
  "forecast_coking", "TI_CO", 6L
)

holdouts <- monthly %>%
  mutate(year = year(time), month = month(time)) %>%
  filter(year %in% evaluation_years) %>%
  inner_join(patterns, by = "nrg_bal", relationship = "many-to-many") %>%
  filter(nrg_bal != "TI_CO" | siec == SIEC_HARD_COAL) %>%
  group_by(iso2, siec, nrg_bal, unit, year, pattern, months_hidden) %>%
  summarise(
    months = n_distinct(month),
    complete = months == 12 && all(!is.na(values)),
    activity = sum(abs(values), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(complete, activity > 0) %>%
  mutate(holdout_id = row_number())
write_out(holdouts, "holdout_definitions.csv")

get_month_values <- function(country, fuel, balance, target_year) {
  values <- reported %>%
    filter(
      .data$iso2 == .env$country,
      .data$siec == .env$fuel,
      .data$nrg_bal == .env$balance,
      year(.data$time) == .env$target_year
    ) %>%
    arrange(time)
  if (nrow(values) != 12 || any(is.na(values$reported_value))) return(NULL)
  values$reported_value
}

predict_holdout <- function(holdout) {
  country <- holdout$iso2[[1]]
  fuel <- holdout$siec[[1]]
  balance <- holdout$nrg_bal[[1]]
  unit_value <- holdout$unit[[1]]
  target_year <- holdout$year[[1]]
  hidden <- seq_len(holdout$months_hidden[[1]])
  actual <- get_month_values(country, fuel, balance, target_year)
  if (is.null(actual)) return(tibble())
  annual_available <- annual
  if (holdout$pattern[[1]] == "missing_sector") {
    annual_available <- annual_available %>%
      filter(!(
        iso2 == country & siec == fuel & year(time) == target_year &
          nrg_bal %in% COAL_ANNUAL_POWER_BALANCES
      ))
  }
  if (holdout$pattern[[1]] == "missing_coking") {
    annual_available <- annual_available %>%
      filter(!(
        iso2 == country & siec == fuel & year(time) == target_year &
          nrg_bal == "TI_CO_E"
      ))
  }
  if (holdout$pattern[[1]] %in% c("forecast_sector", "forecast_coking")) {
    annual_available <- annual_available %>%
      filter(!(
        iso2 == country & siec == fuel & year(time) == target_year
      ))
  }

  previous <- get_month_values(country, fuel, balance, target_year - 1L)
  trailing <- lapply(target_year - 3:1, function(source_year) {
    get_month_values(country, fuel, balance, source_year)
  })
  previous_prediction <- if (is.null(previous)) {
    rep(NA_real_, length(hidden))
  } else {
    previous[hidden]
  }
  average_prediction <- if (any(vapply(trailing, is.null, logical(1)))) {
    rep(NA_real_, length(hidden))
  } else {
    rowMeans(do.call(cbind, trailing))[hidden]
  }

  anchor <- .coal_annual_anchor(
    annual_available, country, fuel, unit_value, target_year, balance
  )
  use_consumption <- balance == "TI_EHG_MAP" &&
    .coal_power_uses_consumption_profile(
      annual_available, country, fuel, unit_value, target_year
    )
  profile <- .coal_monthly_profile(
    reported, country, fuel, unit_value, target_year, balance, use_consumption
  )
  production <- rep(NA_real_, length(hidden))
  annual_method <- anchor$method
  profile_method <- if (is.null(profile)) NA_character_ else profile$method
  if (!is.na(anchor$value) && !is.null(profile)) {
    fixed_total <- sum(actual[-hidden])
    remainder <- anchor$value - fixed_total
    weights <- profile$shares[hidden]
    if (remainder >= -1e-6 && sum(weights) > 0) {
      production <- pmax(0, remainder) * weights / sum(weights)
    }
  }

  if (all(is.na(production)) && balance %in% c("TI_EHG_MAP", "TI_CO")) {
    history <- .coal_annual_history(
      annual_available, country, fuel, unit_value, target_year, balance
    )
    ratio <- .coal_stable_ratio(history$target_value, history$consumption_value)
    consumption <- get_month_values(country, fuel, "GID_CAL", target_year)
    if (!is.null(ratio) && !is.null(consumption)) {
      production <- pmax(0, consumption[hidden] * tail(ratio, 1))
      annual_method <- "previous_year_share_unconstrained"
      profile_method <- "same_month_reported_consumption"
    }
  }

  dates <- as.Date(sprintf("%d-%02d-01", target_year, hidden))
  bind_rows(
    tibble(method = "annual_backed", predicted = production),
    tibble(method = "previous_year", predicted = previous_prediction),
    tibble(method = "three_year_average", predicted = average_prediction),
    tibble(method = "unchanged", predicted = rep(NA_real_, length(hidden)))
  ) %>%
    group_by(method) %>%
    mutate(
      holdout_id = holdout$holdout_id[[1]],
      iso2 = country,
      siec = fuel,
      nrg_bal = balance,
      unit = unit_value,
      evaluation_year = target_year,
      pattern = holdout$pattern[[1]],
      time = dates,
      actual = actual[hidden],
      annual_method = if_else(method == "annual_backed", annual_method, NA_character_),
      profile_method = if_else(method == "annual_backed", profile_method, NA_character_)
    ) %>%
    ungroup()
}

predictions <- bind_rows(lapply(seq_len(nrow(holdouts)), function(index) {
  predict_holdout(holdouts[index, ])
}))
write_out(predictions, "candidate_predictions.csv")

conversion <- process_conversion_factors(conversion_raw) %>%
  group_by(iso2, siec) %>%
  summarise(ncv_kjkg = mean(ncv_kjkg, na.rm = TRUE), .groups = "drop")
conversion_fallback <- conversion %>%
  group_by(siec) %>%
  summarise(fallback_ncv_kjkg = mean(ncv_kjkg, na.rm = TRUE), .groups = "drop")
factors <- crossing(iso2 = eu_countries, siec = COAL_MONTHLY_GAP_FUELS) %>%
  left_join(conversion, by = c("iso2", "siec")) %>%
  left_join(conversion_fallback, by = "siec") %>%
  mutate(ncv_kjkg = coalesce(ncv_kjkg, fallback_ncv_kjkg)) %>%
  left_join(get_ipcc_emission_factors(), by = "siec") %>%
  transmute(
    iso2, siec,
    co2_t_per_thousand_tonnes = ncv_kjkg / 1000 * co2_factor_t_per_TJ
  )

scores <- predictions %>%
  left_join(factors, by = c("iso2", "siec")) %>%
  mutate(
    error = predicted - actual,
    total_multiplier = case_when(
      nrg_bal == "GID_CAL" ~ 1,
      nrg_bal == "TI_CO" ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
      nrg_bal == "TI_EHG_MAP" ~ 1,
      TRUE ~ NA_real_
    ),
    co2_error_t = error * total_multiplier * co2_t_per_thousand_tonnes
  ) %>%
  group_by(
    holdout_id, iso2, siec, nrg_bal, pattern, evaluation_year, method
  ) %>%
  summarise(
    target_cells = n(),
    available_cells = sum(!is.na(predicted)),
    activity = sum(abs(actual)),
    covered_activity = sum(abs(actual[!is.na(predicted)])),
    monthly_mae = if (available_cells > 0) {
      mean(abs(error), na.rm = TRUE)
    } else {
      NA_real_
    },
    rmse = if (available_cells > 0) {
      sqrt(mean(error^2, na.rm = TRUE))
    } else {
      NA_real_
    },
    signed_bias = if (available_cells > 0) mean(error, na.rm = TRUE) else NA_real_,
    gap_total_error = if (available_cells == target_cells) sum(error) else NA_real_,
    direct_co2_equivalent_error_t = if (available_cells == target_cells) {
      sum(co2_error_t)
    } else {
      NA_real_
    },
    absolute_direct_co2_equivalent_error_t = abs(direct_co2_equivalent_error_t),
    .groups = "drop"
  ) %>%
  mutate(activity_coverage = covered_activity / activity)
write_out(scores, "model_scores.csv")

matched <- scores %>%
  select(
    holdout_id, iso2, siec, nrg_bal, pattern, evaluation_year, method,
    absolute_direct_co2_equivalent_error_t
  ) %>%
  pivot_wider(names_from = method, values_from = absolute_direct_co2_equivalent_error_t) %>%
  filter(
    !is.na(annual_backed), !is.na(previous_year), !is.na(three_year_average)
  ) %>%
  mutate(
    beats_previous = annual_backed < previous_year,
    beats_average = annual_backed < three_year_average,
    ties_previous = annual_backed == previous_year,
    ties_average = annual_backed == three_year_average
  )
write_out(matched, "matched_country_year_scores.csv")

untouched_years <- 2020:2024
untouched_matched <- matched %>%
  filter(evaluation_year %in% untouched_years)
policy_gates <- scores %>%
  filter(method == "annual_backed", evaluation_year %in% untouched_years) %>%
  group_by(siec, nrg_bal, pattern) %>%
  summarise(
    evaluation_years = n_distinct(evaluation_year[available_cells == target_cells]),
    activity_coverage = sum(covered_activity) / sum(activity),
    .groups = "drop"
  ) %>%
  left_join(
    untouched_matched %>%
      group_by(siec, nrg_bal, pattern) %>%
      summarise(
        matched_country_years = n(),
        share_beating_previous = mean(beats_previous),
        share_beating_average = mean(beats_average),
        share_tying_previous = mean(ties_previous),
        share_tying_average = mean(ties_average),
        .groups = "drop"
      ),
    by = c("siec", "nrg_bal", "pattern")
  ) %>%
  mutate(
    passes = evaluation_years >= 5 & activity_coverage >= 0.70 &
      share_beating_previous > 0.5 & share_beating_average > 0.5,
    selected_policy = if_else(passes, "annual_backed", "existing_pipeline")
  )
write_out(policy_gates, "direct_candidate_policy.csv")

rolling_selection <- bind_rows(lapply(untouched_years, function(test_year) {
  training <- scores %>% filter(evaluation_year < test_year, method != "unchanged")
  training_matched <- matched %>% filter(evaluation_year < test_year)
  gates <- training %>%
    filter(method == "annual_backed") %>%
    group_by(siec, nrg_bal, pattern) %>%
    summarise(
      training_years = n_distinct(evaluation_year[!is.na(direct_co2_equivalent_error_t)]),
      activity_coverage = sum(covered_activity) / sum(activity),
      .groups = "drop"
    ) %>%
    left_join(
      training_matched %>%
        group_by(siec, nrg_bal, pattern) %>%
        summarise(
          share_beating_previous = mean(beats_previous),
          share_beating_average = mean(beats_average),
          .groups = "drop"
        ),
      by = c("siec", "nrg_bal", "pattern")
    ) %>%
    mutate(
      test_year = test_year,
      annual_backed_eligible = training_years >= 5 & activity_coverage >= 0.70 &
        share_beating_previous > 0.5 & share_beating_average > 0.5
    )
  baseline <- training %>%
    filter(method %in% c("previous_year", "three_year_average")) %>%
    group_by(siec, nrg_bal, pattern, method) %>%
    summarise(error = mean(absolute_direct_co2_equivalent_error_t, na.rm = TRUE), .groups = "drop") %>%
    group_by(siec, nrg_bal, pattern) %>%
    arrange(error, method) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(siec, nrg_bal, pattern, baseline_method = method)
  gates %>%
    left_join(baseline, by = c("siec", "nrg_bal", "pattern")) %>%
    mutate(
      selected_method = if_else(
        annual_backed_eligible, "annual_backed", baseline_method
      )
    )
}))
untouched_scores <- rolling_selection %>%
  select(siec, nrg_bal, pattern, evaluation_year = test_year, selected_method) %>%
  inner_join(
    scores,
    by = c("siec", "nrg_bal", "pattern", "evaluation_year")
  ) %>%
  filter(method == selected_method)
write_out(rolling_selection, "rolling_method_selection.csv")
write_out(untouched_scores, "untouched_evaluation_scores.csv")

summary_plot <- scores %>%
  filter(method != "unchanged", !is.na(monthly_mae)) %>%
  group_by(pattern, method) %>%
  summarise(monthly_mae = weighted.mean(monthly_mae, available_cells), .groups = "drop") %>%
  ggplot(aes(method, monthly_mae, fill = pattern)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Annual-backed coal reconstruction against simple baselines",
    subtitle = "Monthly MAE | EU countries and four coal systems | 2015–2024 holdouts",
    x = NULL,
    y = "Thousand tonnes"
  ) +
  theme_minimal()
ggsave(file.path(output, "model_comparison.png"), summary_plot, width = 9, height = 5, dpi = 150)

gate_summary <- policy_gates %>% count(selected_policy)
untouched_summary <- untouched_scores %>%
  summarise(
    evaluation_years = n_distinct(evaluation_year),
    available_country_years = sum(!is.na(direct_co2_equivalent_error_t)),
    mean_absolute_direct_error_t = mean(absolute_direct_co2_equivalent_error_t, na.rm = TRUE)
  )
writeLines(
  c(
    "# Coal annual reconstruction validation", "",
    paste0("Frozen input: `", input, "`. Evaluation ran offline."),
    paste0(
      "Holdouts use revised history from the frozen snapshot; they do not reproduce ",
      "past publication vintages."
    ),
    "", "## Design", "",
    paste0(
      "Whole-year consumption, H1 consumption, missing annual sector/coking, and ",
      "forecast sector/coking gaps were replayed for 2015–2024."
    ),
    paste0(
      "For 2020–2024, method selection used only earlier holdouts before scoring the ",
      "next untouched year."
    ),
    "", "## Result", "",
    paste0(
      "Selected-policy rows: ",
      paste(paste(gate_summary$selected_policy, gate_summary$n, sep = "="), collapse = ", "),
      "."
    ),
    paste0(
      "Untouched evaluation years: ", untouched_summary$evaluation_years,
      "; available country-years: ", untouched_summary$available_country_years,
      "; mean absolute direct CO2-equivalent error: ",
      round(untouched_summary$mean_absolute_direct_error_t), " tCO2."
    ),
    "These direct conversion scores do not validate downstream H1 change. Production policy requires the separate downstream replay."
  ),
  file.path(output, "decision_note.md")
)

message("Validation complete: ", nrow(holdouts), " holdouts.")
