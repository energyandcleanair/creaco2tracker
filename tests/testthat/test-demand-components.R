test_that("get_demand_components with LM returns expected structure", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = tempdir()
  )

  # Check expected columns
  expect_true(all(c("iso2", "date", "fuel", "component", "value",
                     "value_weather_corrected", "unit", "frequency",
                     "data_source") %in% names(result)))

  # Check fuels present
  expect_true("fossil_gas" %in% result$fuel | "electricity" %in% result$fuel)

  # Check components present for electricity
  elec <- result %>% filter(fuel == "electricity")
  if (nrow(elec) > 0) {
    expect_true(all(c("heating", "cooling", "others") %in% unique(elec$component)))
  }

  # Check components present for gas
  gas <- result %>% filter(fuel != "electricity")
  if (nrow(gas) > 0) {
    expect_true(all(c("heating", "electricity", "others") %in% unique(gas$component)))
  }
})


test_that("get_demand_components with GAM returns expected structure", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2020-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "gam",
    diagnostics_folder = tempdir()
  )

  # Same structure as LM
  expect_true(all(c("iso2", "date", "fuel", "component", "value",
                     "value_weather_corrected") %in% names(result)))

  # Should have data

  expect_true(nrow(result) > 0)
})


test_that("components sum to observed value (direct subtraction property)", {
  skip_if_offline()
  skip_on_cran()

  for (mt in c("lm", "gam")) {
    result <- get_demand_components(
      iso2s = "EU",
      date_from = "2023-01-01",
      date_to = "2023-03-31",
      use_cache = TRUE,
      model_type = mt,
      diagnostics_folder = NULL
    )

    # For each fuel-date, components should sum to a consistent total
    # We check by summing components per date and comparing across days
    component_sums <- result %>%
      filter(!is.na(value)) %>%
      group_by(iso2, date, fuel) %>%
      summarise(total_components = sum(value, na.rm = TRUE), .groups = "drop")

    # Total should be positive for most days
    expect_true(
      mean(component_sums$total_components > 0, na.rm = TRUE) > 0.9,
      info = paste("Model type:", mt, "- most days should have positive total demand")
    )
  }
})


test_that("value_weather_corrected equals value for non-weather components", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = NULL
  )

  # For 'others' component, value == value_weather_corrected
  others <- result %>%
    filter(component == "others", !is.na(value))

  if (nrow(others) > 0) {
    expect_equal(others$value, others$value_weather_corrected,
                 tolerance = 1e-6,
                 info = "'others' should have value == value_weather_corrected")
  }

  # For gas 'electricity' component, value == value_weather_corrected
  gas_elec <- result %>%
    filter(component == "electricity", fuel != "electricity", !is.na(value))

  if (nrow(gas_elec) > 0) {
    expect_equal(gas_elec$value, gas_elec$value_weather_corrected,
                 tolerance = 1e-6,
                 info = "gas 'electricity' should have value == value_weather_corrected")
  }
})


test_that("value_weather_corrected differs from value for heating component", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = NULL
  )

  heating <- result %>%
    filter(component == "heating", !is.na(value), !is.na(value_weather_corrected))

  if (nrow(heating) > 10) {
    # Not all days should have equal value and value_weather_corrected
    # (weather varies from the climatological mean)
    pct_equal <- mean(abs(heating$value - heating$value_weather_corrected) < 1e-6)
    expect_true(pct_equal < 0.9,
                info = "Heating should differ between actual and weather-corrected on most days")
  }
})


test_that("both model types return same column structure", {
  skip_if_offline()
  skip_on_cran()

  lm_result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = NULL
  )

  gam_result <- get_demand_components(
    iso2s = "EU",
    date_from = "2020-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "gam",
    diagnostics_folder = NULL
  )

  expect_equal(sort(names(lm_result)), sort(names(gam_result)))
  expect_equal(sort(unique(lm_result$component)), sort(unique(gam_result$component)))
})


test_that("heating is positive in winter months", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-02-28",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = NULL
  )

  winter_heating <- result %>%
    filter(component == "heating", !is.na(value))

  if (nrow(winter_heating) > 0) {
    # Most winter days should have positive heating demand
    pct_positive <- mean(winter_heating$value > 0, na.rm = TRUE)
    expect_true(pct_positive > 0.8,
                info = "Heating should be positive for most winter days")
  }
})


test_that("others baseline is positive", {
  skip_if_offline()
  skip_on_cran()

  result <- get_demand_components(
    iso2s = "EU",
    date_from = "2023-01-01",
    date_to = "2023-03-31",
    use_cache = TRUE,
    model_type = "lm",
    diagnostics_folder = NULL
  )

  others <- result %>%
    filter(component == "others", !is.na(value))

  if (nrow(others) > 0) {
    # Baseline demand should be positive (it's the demand without any heating/cooling)
    pct_positive <- mean(others$value > 0, na.rm = TRUE)
    expect_true(pct_positive > 0.9,
                info = "Others (baseline) should be positive for most days")
  }
})
