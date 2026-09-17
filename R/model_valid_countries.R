get_valid_countries <- function(
  co2,
  validation_data = NULL,
  min_year = 2020,
  min_correlation = 0.9,
  max_mae = 0.03
) {
  get_validity_metrics(co2, validation_data, min_year, min_correlation, max_mae) %>%
    filter(ok) %>%
    pull(iso2)
}

get_validity_metrics <- function(
  co2,
  validation_data = NULL,
  min_year = 2020,
  min_correlation = 0.9,
  max_mae = 0.03
) {
  # Get validation data first (similar to validate_co2 function)
  if (is.null(validation_data) || !all(unique(co2$iso2) %in% unique(validation_data$iso2))) {
    validation_data <- get_validation_data(region = unique(co2$iso2))
  }


  # Calculate complete yearly totals for CREA data. Partial calendar years must
  # never enter a year-on-year comparison.
  co2_crea <- co2 %>%
    filter(
      fuel == FUEL_TOTAL,
      sector == SECTOR_ALL,
      estimate == "central"
    ) %>%
    group_by(iso2, year = year(date)) %>%
    summarise(
      expected_days = if_else(lubridate::leap_year(first(year)), 366L, 365L),
      value = if (n() == expected_days && n_distinct(date) == expected_days &&
        all(is.finite(value))) sum(value) / 1e6 else NA_real_,
      source = "CREA",
      .groups = "drop"
    ) %>% select(-expected_days)

  # Filter GCP2 data from validation
  co2_gcp <- validation_data %>%
    filter(
      source == "Global Carbon Budget 2025",
      sector == SECTOR_ALL,
      fuel == FUEL_TOTAL
    )

  # Calculate YOY changes only across consecutive years with positive prior
  # totals. This avoids manufacturing growth across a missing year.
  yoy_comparison <- bind_rows(
    # CREA YOY
    co2_crea %>%
      group_by(iso2) %>%
      arrange(year) %>%
      mutate(yoy = if_else(year == lag(year) + 1L & lag(value) > 0 & value > 0 &
        is.finite(value),
        value / lag(value) - 1, NA_real_)) %>%
      filter(year >= min_year) %>%
      select(iso2, year, yoy, source),

    # GCP2 YOY
    co2_gcp %>%
      group_by(iso2) %>%
      arrange(year) %>%
      mutate(yoy = if_else(year == lag(year) + 1L & lag(value) > 0 & value > 0 &
        is.finite(value),
        value / lag(value) - 1, NA_real_)) %>%
      filter(year >= min_year) %>%
      select(iso2, year, yoy, source)
  )

  # Calculate metrics by country
  metrics <- yoy_comparison %>%
    group_by(iso2, year) %>%
    filter(n() == 2 & all(!is.na(yoy))) %>%
    pivot_wider(names_from = source, values_from = yoy) %>%
    group_by(iso2) %>%
    summarise(
      rmse = sqrt(mean((CREA - `Global Carbon Budget 2025`)^2, na.rm = TRUE)),
      # Root Mean Square Error
      mae = mean(abs(CREA - `Global Carbon Budget 2025`), na.rm = TRUE), # Mean Absolute Error
      correlation = if (n() >= 2L && sd(CREA) > 0 && sd(`Global Carbon Budget 2025`) > 0) {
        cor(CREA, `Global Carbon Budget 2025`, use = "complete.obs")
      } else {
        NA_real_
      },
      n_years = sum(!is.na(CREA) & !is.na(`Global Carbon Budget 2025`)),
      comparison_years = paste(year, collapse = ","),
      # Number of comparable years
      .groups = "drop"
    )
  metrics <- co2_crea %>% distinct(iso2) %>%
    left_join(metrics, by = "iso2") %>%
    mutate(
      n_years = coalesce(n_years, 0L),
      comparison_years = coalesce(comparison_years, ""),
      correlation_enforced = n_years >= 5L,
      correlation_ok = !correlation_enforced | (!is.na(correlation) & correlation >= min_correlation),
      mae_ok = is.finite(mae) & mae <= max_mae,
      enough_years = n_years >= 3L,
      ok = enough_years & mae_ok & correlation_ok,
      reason = case_when(
        !enough_years ~ "insufficient_comparable_years",
        !mae_ok ~ "mae_above_threshold",
        !correlation_ok ~ "correlation_below_threshold",
        correlation_enforced ~ "passed_mae_and_correlation",
        TRUE ~ "passed_mae_correlation_advisory"
      )
    )

  return(metrics)
}
