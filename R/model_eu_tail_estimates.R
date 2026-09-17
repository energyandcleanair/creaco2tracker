#' Stabilise EU CO2 estimates in the latest months
#'
#' Latest EU rows can be less stable than later vintages because source coverage
#' changes while recent months are still being projected. This wrapper runs two
#' EU-tail submodels that propose row-level adjustments, then applies the
#' selected adjustments in one place:
#'
#' - country-sum adjustments use member-state sums when historical EU rows and
#'   country sums agree closely enough and enough countries are available in the
#'   tail month;
#' - seasonal year-over-year adjustments use a recent YoY ratio when a holdout
#'   backtest shows that ratio beats a one-step Holt-Winters forecast. Daily
#'   inputs preserve their current within-month shape and scale it to the
#'   coverage-matched seasonal target.
#'
#' The submodels return candidate replacement rows rather than mutating `co2`.
#' Non-total fuel adjustments are applied first and fuel totals are recomputed.
#' Explicit total-fuel adjustments are applied after that, so a validated
#' member-country total can replace the recomputed EU total in the final output.
#'
#' In the end this may replace EU (`iso2 == "EU"`) `value` rows for the latest
#' `tail_months`, keyed by `date`, `fuel`, `sector`, `estimate`, and `unit`.
#' Country rows are never replaced by this wrapper.
#'
#' @param co2 CO2 data with `iso2`, `date`, `fuel`, `sector`, `estimate`,
#'   `unit`, and `value` columns.
#' @param country_sum_min_countries Minimum number of member countries required
#'   before a country-sum adjustment can be selected.
#' @param country_sum_max_rel_diff Maximum historical relative difference
#'   allowed between the EU row and the member-country sum.
#' @param country_sum_min_points Minimum number of historical non-missing
#'   comparison points for the country-sum validation.
#' @param tail_months Number of latest months eligible for adjustment.
#' @param seasonal_recent_months Number of recent months used to estimate the
#'   YoY seasonal ratio.
#' @param seasonal_improvement_margin Multiplier used when comparing the
#'   seasonal holdout error with the Holt-Winters holdout error.
#' @param seasonal_min_history_points Minimum observations required for the
#'   Holt-Winters holdout forecast.
#' @param seasonal_min_ratio,seasonal_max_ratio Bounds for accepted YoY ratios.
#' @return A CO2 data frame with selected EU tail values replaced.
#' @noRd
stabilise_eu_tail_estimates <- function(
  co2,
  country_sum_min_countries = 20,
  country_sum_max_rel_diff = 0.05,
  country_sum_min_points = 6,
  tail_months = 4,
  seasonal_recent_months = 2,
  seasonal_improvement_margin = 1,
  seasonal_min_history_points = 24,
  seasonal_min_ratio = 0.5,
  seasonal_max_ratio = 1.5
) {
  country_sum_adjustments <- select_eu_tail_country_sum_adjustments(
    co2,
    min_countries = country_sum_min_countries,
    max_rel_diff = country_sum_max_rel_diff,
    min_points = country_sum_min_points,
    tail_months = tail_months
  )
  seasonal_yoy_adjustments <- select_eu_tail_seasonal_yoy_adjustments(
    co2,
    tail_months = tail_months,
    recent_months = seasonal_recent_months,
    improvement_margin = seasonal_improvement_margin,
    min_history_points = seasonal_min_history_points,
    min_ratio = seasonal_min_ratio,
    max_ratio = seasonal_max_ratio
  )

  adjustments <- bind_rows(seasonal_yoy_adjustments, country_sum_adjustments)
  result <- apply_selected_eu_tail_adjustments(co2, adjustments)
  attr(result, "eu_tail_country_coverage") <- attr(country_sum_adjustments, "country_coverage")
  attr(result, "eu_tail_adjustments") <- adjustments
  result
}

#' Return an empty EU-tail adjustment table
#'
#' This keeps selector return types stable when no submodel can propose an
#' adjustment. The schema matches the candidate rows consumed by
#' `apply_selected_eu_tail_adjustments()`.
#'
#' @return An empty tibble with EU-tail adjustment columns.
#' @noRd
empty_eu_tail_adjustments <- function() {
  tibble(
    iso2 = character(),
    date = as.Date(character()),
    fuel = character(),
    sector = character(),
    estimate = character(),
    unit = character(),
    value = numeric(),
    adjustment_model = character(),
    adjustment_priority = integer()
  )
}

#' Select EU-tail adjustments from member-country sums
#'
#' This submodel treats the sum of available EU member countries as a candidate
#' replacement for direct EU tail rows. It first checks non-tail history for each
#' `fuel`/`sector`/`unit` group. A group is eligible only when the direct EU
#' central estimate and the member-country central sum have at least
#' `min_points` overlapping observations and the maximum relative difference is
#' no larger than `max_rel_diff`.
#'
#' Coal and all-fuel totals require unique, finite contributions from every EU
#' member state. An incomplete country sum cannot replace an available EU value.
#'
#' For eligible groups, the selector proposes adjustments in the latest
#' `tail_months` where at least `min_countries` member countries have non-missing
#' values. The proposed value is the member-country sum for each estimate
#' (`central`, `lower`, and `upper` when present).
#'
#' The returned candidates replace EU rows with matching `date`, `fuel`,
#' `sector`, `estimate`, and `unit` if selected by
#' `apply_selected_eu_tail_adjustments()`. They can include total-fuel rows,
#' which are applied after any recomputed totals.
#'
#' @param co2 CO2 data with `iso2`, `date`, `fuel`, `sector`, `estimate`,
#'   `unit`, and `value` columns.
#' @param min_countries Minimum member-country coverage for a tail adjustment.
#' @param max_rel_diff Maximum historical relative difference allowed.
#' @param min_points Minimum historical comparison points required.
#' @param tail_months Number of latest months eligible for adjustment.
#' @return A tibble of candidate EU replacement rows with `adjustment_model` and
#'   `adjustment_priority`, or an empty adjustment table.
#' @noRd
select_eu_tail_country_sum_adjustments <- function(
  co2,
  min_countries = 20,
  max_rel_diff = 0.05,
  min_points = 6,
  tail_months = 4
) {
  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "unit", "value")
  if (!all(required_cols %in% names(co2)) || !"EU" %in% co2$iso2) {
    return(empty_eu_tail_adjustments())
  }

  co2_work <- co2 %>%
    mutate(
      .row_id = row_number(),
      .month = lubridate::floor_date(as.Date(date), "month")
    )

  group_cols <- c("fuel", "sector", "unit")
  members <- get_eu_iso2s(include_eu = FALSE)

  eu_central <- co2_work %>%
    filter(iso2 == "EU", estimate == "central") %>%
    select(.row_id, date, .month, all_of(group_cols), eu_value = value)

  country_sums_central <- co2_work %>%
    filter(iso2 %in% members, estimate == "central") %>%
    group_by(date, .month, across(all_of(group_cols))) %>%
    summarise(
      n_countries = n_distinct(iso2[is.finite(value)]),
      complete_members = n() == length(members) && n_countries == length(members),
      country_sum = if (first(fuel) %in% c(FUEL_COAL, FUEL_TOTAL) &&
        !complete_members) NA_real_ else sum_or_na(value),
      .groups = "drop"
    )

  central_check_data <- eu_central %>%
    left_join(country_sums_central, by = c("date", ".month", group_cols)) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      .tail_start = max(.month, na.rm = TRUE) %m-% months(tail_months - 1),
      .is_tail_month = .month >= .tail_start
    ) %>%
    ungroup()
  coverage <- central_check_data %>% transmute(
    date, fuel, sector, unit, eu_value, country_sum, n_countries, complete_members,
    requires_complete_members = fuel %in% c(FUEL_COAL, FUEL_TOTAL),
    is_tail_month = .is_tail_month
  )

  replace_keys <- central_check_data %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(function(df, key) {
      historical_data <- df %>%
        filter(!.is_tail_month)

      correlation_check <- check_proxy_correlation(
        historical_data$eu_value,
        historical_data$country_sum,
        max_rel_diff = max_rel_diff,
        min_points = min_points
      )

      if (!isTRUE(correlation_check$is_good_enough)) {
        return(tibble())
      }

      tail_rows <- df %>%
        filter(
          .is_tail_month,
          n_countries >= min_countries,
          !is.na(country_sum)
        ) %>%
        select(date)

      if (nrow(tail_rows) == 0) {
        return(tibble())
      }

      tail_rows
    }) %>%
    ungroup()

  if (nrow(replace_keys) == 0) {
    result <- empty_eu_tail_adjustments()
    attr(result, "country_coverage") <- coverage
    return(result)
  }

  result <- co2_work %>%
    filter(iso2 %in% members) %>%
    semi_join(replace_keys, by = c("date", group_cols)) %>%
    group_by(date, across(all_of(c(group_cols, "estimate")))) %>%
    summarise(
      .replacement_countries = n_distinct(iso2[is.finite(value)]),
      .complete_members = n() == length(members) &&
        .replacement_countries == length(members),
      .replacement_value = if (first(fuel) %in% c(FUEL_COAL, FUEL_TOTAL) &&
        !.complete_members) NA_real_ else sum_or_na(value),
      .groups = "drop"
    ) %>%
    filter(.replacement_countries >= min_countries, !is.na(.replacement_value)) %>%
    transmute(
      iso2 = "EU",
      date,
      fuel,
      sector,
      estimate,
      unit,
      value = .replacement_value,
      adjustment_model = "country_sum",
      adjustment_priority = 20L
    )
  attr(result, "country_coverage") <- coverage
  result
}

#' Select EU-tail adjustments from recent seasonal YoY ratios
#'
#' This submodel proposes non-total EU fuel adjustments when recent YoY
#' seasonality appears more reliable than the generic Holt-Winters forecast for
#' the group. It identifies the latest `tail_months`, uses the month immediately
#' before the tail as a holdout, and estimates a YoY ratio from the
#' `recent_months` before that holdout.
#'
#' A `fuel`/`sector`/`unit` group is selected only when:
#'
#' - the holdout month and same month in the prior year are present;
#' - the recent YoY ratio has enough complete points and stays within
#'   `min_ratio` and `max_ratio`;
#' - a one-step Holt-Winters forecast can be fitted with at least
#'   `min_history_points`;
#' - the seasonal holdout error is lower than the Holt-Winters holdout error
#'   times `improvement_margin`.
#'
#' For selected groups, the proposed tail values are prior-year EU values
#' multiplied by the accepted recent YoY ratio. For daily input, this defines a
#' target over the same available days in the prior year, and the current daily
#' profile is scaled to that target. These candidates only replace non-total EU
#' rows; totals are recomputed later by
#' `apply_selected_eu_tail_adjustments()`.
#'
#' @param co2 CO2 data with `iso2`, `date`, `fuel`, `sector`, `estimate`,
#'   `unit`, and `value` columns.
#' @param tail_months Number of latest months eligible for adjustment.
#' @param recent_months Number of recent months used to estimate the YoY ratio.
#' @param improvement_margin Multiplier used when comparing seasonal and
#'   Holt-Winters holdout errors.
#' @param min_history_points Minimum observations required for Holt-Winters.
#' @param min_ratio,max_ratio Bounds for accepted YoY ratios.
#' @return A tibble of candidate non-total EU replacement rows with
#'   `adjustment_model` and `adjustment_priority`, or an empty adjustment table.
#' @noRd
select_eu_tail_seasonal_yoy_adjustments <- function(
  co2,
  tail_months = 4,
  recent_months = 2,
  improvement_margin = 1,
  min_history_points = 24,
  min_ratio = 0.5,
  max_ratio = 1.5
) {
  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "unit", "value")
  if (!all(required_cols %in% names(co2)) || !"EU" %in% co2$iso2) {
    return(empty_eu_tail_adjustments())
  }

  input_dates <- sort(unique(as.Date(co2$date)))
  input_months <- lubridate::floor_date(input_dates, "month")
  is_daily <- any(duplicated(input_months))

  co2_work <- co2 %>%
    mutate(date = lubridate::floor_date(as.Date(date), "month"))

  tail_dates <- sort(unique(co2_work$date))
  tail_dates <- tail(tail_dates, tail_months)
  if (length(tail_dates) == 0) {
    return(empty_eu_tail_adjustments())
  }

  tail_start <- min(tail_dates)
  holdout_date <- tail_start %m-% months(1)
  ratio_months <- month(holdout_date %m-% months(rev(seq_len(recent_months))))

  predict_hw_one <- function(data, pred_date) {
    history <- data %>%
      filter(date < pred_date, !is.na(value)) %>%
      arrange(date)

    if (nrow(history) < min_history_points) {
      return(NA_real_)
    }

    ts_data <- ts(
      history$value,
      frequency = 12,
      start = c(year(min(history$date)), month(min(history$date)))
    )

    tryCatch(
      as.numeric(forecast::hw(ts_data, h = 1)$mean[[1]]),
      error = function(e) NA_real_
    )
  }

  seasonal_ratio <- function(data, target_year) {
    ratio_data <- data %>%
      filter(
        month(date) %in% ratio_months,
        year(date) %in% c(target_year - 1L, target_year)
      ) %>%
      mutate(.year = paste0("y", year(date)), .month = month(date)) %>%
      select(.year, .month, value) %>%
      pivot_wider(names_from = .year, values_from = value)

    previous_col <- paste0("y", target_year - 1L)
    current_col <- paste0("y", target_year)
    if (!all(c(previous_col, current_col) %in% names(ratio_data))) {
      return(NA_real_)
    }

    complete_points <- sum(
      !is.na(ratio_data[[previous_col]]) & !is.na(ratio_data[[current_col]])
    )
    if (complete_points < recent_months) {
      return(NA_real_)
    }

    previous_total <- sum(ratio_data[[previous_col]], na.rm = TRUE)
    current_total <- sum(ratio_data[[current_col]], na.rm = TRUE)
    ratio <- current_total / previous_total
    if (!is.finite(ratio) || previous_total <= 0 || ratio < min_ratio || ratio > max_ratio) {
      return(NA_real_)
    }

    ratio
  }

  selected_groups <- co2_work %>%
    filter(iso2 == "EU", estimate == "central", fuel != FUEL_TOTAL) %>%
    group_by(date, fuel, sector, unit) %>%
    summarise(value = sum_or_na(value), .groups = "drop") %>%
    group_by(fuel, sector, unit) %>%
    group_modify(function(df, keys) {
      actual_holdout <- df$value[df$date == holdout_date]
      previous_holdout <- df$value[df$date == (holdout_date %m-% years(1))]
      ratio <- seasonal_ratio(df, year(holdout_date))
      hw_holdout <- predict_hw_one(df, holdout_date)

      if (
        length(actual_holdout) != 1 ||
          length(previous_holdout) != 1 ||
          is.na(actual_holdout) ||
          is.na(previous_holdout) ||
          is.na(ratio) ||
          is.na(hw_holdout)
      ) {
        return(tibble())
      }

      seasonal_error <- abs(previous_holdout * ratio - actual_holdout)
      hw_error <- abs(hw_holdout - actual_holdout)
      if (
        is.finite(seasonal_error) &&
          is.finite(hw_error) &&
          seasonal_error < hw_error * improvement_margin
      ) {
        tibble(.seasonal_ratio = ratio)
      } else {
        tibble()
      }
    }) %>%
    ungroup()

  if (nrow(selected_groups) == 0) {
    return(empty_eu_tail_adjustments())
  }

  if (is_daily) {
    daily_eu <- co2 %>%
      filter(iso2 == "EU", fuel != FUEL_TOTAL) %>%
      mutate(
        date = as.Date(date),
        .month = lubridate::floor_date(date, "month"),
        .year = lubridate::year(date),
        .month_number = lubridate::month(date),
        .day = lubridate::day(date)
      )

    target_profiles <- daily_eu %>%
      filter(.month %in% tail_dates) %>%
      inner_join(selected_groups, by = c("fuel", "sector", "unit"))

    prior_profiles <- daily_eu %>%
      transmute(
        .target_year = .year + 1L,
        .month_number,
        .day,
        fuel,
        sector,
        estimate,
        unit,
        .prior_value = value
      )

    target_profiles <- target_profiles %>%
      left_join(
        prior_profiles,
        by = c(
          ".year" = ".target_year",
          ".month_number",
          ".day",
          "fuel",
          "sector",
          "estimate",
          "unit"
        ),
        relationship = "many-to-one"
      )

    scale_factors <- target_profiles %>%
      group_by(.month, fuel, sector, estimate, unit) %>%
      summarise(
        .n_days = n(),
        .n_current = sum(!is.na(value)),
        .n_comparable = sum(!is.na(.prior_value)),
        .current_sum = sum_or_na(value),
        .prior_sum = sum_or_na(.prior_value),
        .seasonal_ratio = first(.seasonal_ratio),
        .groups = "drop"
      ) %>%
      mutate(
        .target_sum = .prior_sum * .seasonal_ratio,
        .scale_factor = .target_sum / .current_sum
      ) %>%
      filter(
        .n_current == .n_days,
        .n_comparable == .n_days,
        is.finite(.current_sum),
        .current_sum > 0,
        is.finite(.target_sum),
        .target_sum > 0,
        is.finite(.scale_factor),
        .scale_factor > 0
      ) %>%
      select(.month, fuel, sector, estimate, unit, .scale_factor)

    return(
      target_profiles %>%
        inner_join(
          scale_factors,
          by = c(".month", "fuel", "sector", "estimate", "unit")
        ) %>%
        transmute(
          iso2,
          date,
          fuel,
          sector,
          estimate,
          unit,
          value = value * .scale_factor,
          adjustment_model = "seasonal_yoy",
          adjustment_priority = 10L
        )
    )
  }

  co2_work %>%
    filter(iso2 == "EU", fuel != FUEL_TOTAL) %>%
    group_by(date, iso2, fuel, sector, estimate, unit) %>%
    summarise(value = sum_or_na(value), .groups = "drop") %>%
    mutate(.replacement_date = lubridate::add_with_rollback(date, lubridate::years(1))) %>%
    inner_join(selected_groups, by = c("fuel", "sector", "unit")) %>%
    filter(.replacement_date %in% tail_dates) %>%
    transmute(
      iso2,
      date = .replacement_date,
      fuel,
      sector,
      estimate,
      unit,
      value = value * .seasonal_ratio,
      adjustment_model = "seasonal_yoy",
      adjustment_priority = 10L
    )
}

#' Apply selected EU-tail adjustments
#'
#' Candidate adjustments are resolved by their row key
#' (`iso2`, `date`, `fuel`, `sector`, `estimate`, and `unit`). If multiple
#' candidates target the same row, the candidate with the highest
#' `adjustment_priority` wins.
#'
#' Non-total adjustments are applied first. The function then calls
#' `add_total_co2()` so total-fuel rows reflect the adjusted component fuels.
#' Total-fuel adjustments are applied last, which lets a validated total from a
#' selector replace the recomputed EU total. This ordering affects final results
#' when a component submodel and a total-row submodel both propose changes for
#' the same tail month.
#'
#' In the final data, only rows matching selected adjustment keys have their
#' `value` replaced. Other rows are returned unchanged, except total-fuel rows
#' may be regenerated when non-total adjustments exist.
#'
#' @param co2 CO2 data with `iso2`, `date`, `fuel`, `sector`, `estimate`,
#'   `unit`, and `value` columns.
#' @param adjustments Candidate replacement rows from EU-tail selectors.
#' @return A CO2 data frame with selected adjustment values applied.
#' @noRd
apply_selected_eu_tail_adjustments <- function(co2, adjustments) {
  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "unit", "value")
  if (!all(required_cols %in% names(co2)) || nrow(adjustments) == 0) {
    return(co2)
  }

  key_cols <- required_cols[required_cols != "value"]
  selected_adjustments <- adjustments %>%
    arrange(adjustment_priority) %>%
    group_by(across(all_of(key_cols))) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(all_of(key_cols), .adjustment_value = value)

  apply_adjustments <- function(data, selected) {
    data %>%
      left_join(selected, by = key_cols) %>%
      mutate(value = coalesce(.adjustment_value, value)) %>%
      select(-.adjustment_value)
  }

  non_total_adjustments <- selected_adjustments %>%
    filter(fuel != FUEL_TOTAL)
  total_adjustments <- selected_adjustments %>%
    filter(fuel == FUEL_TOTAL)

  co2_adjusted <- co2

  if (nrow(non_total_adjustments) > 0) {
    co2_adjusted <- co2_adjusted %>%
      filter(fuel != FUEL_TOTAL) %>%
      apply_adjustments(non_total_adjustments) %>%
      add_total_co2()
  }

  if (nrow(total_adjustments) > 0) {
    co2_adjusted <- co2_adjusted %>%
      apply_adjustments(total_adjustments)
  }

  co2_adjusted
}
