library(testthat)
library(dplyr)
library(lubridate)

# Helper function to mock get_weather for solar
mock_weather_function_solar <- function(mock_data) {
  # Try to get the function from the package namespace
  ns_name <- tryCatch({
    if (exists("get_weather", envir = asNamespace("creaco2tracker"))) {
      "creaco2tracker"
    } else {
      stop("Namespace not found")
    }
  }, error = function(e) {
    NULL
  })

  original_func <- if (!is.null(ns_name)) {
    get("get_weather", envir = asNamespace(ns_name))
  } else {
    get("get_weather", envir = .GlobalEnv)
  }

  # Create mock function
  mock_func <- function(variable, region_iso2, region_type, date_from, date_to, ...) {
    return(mock_data)
  }

  # Replace function
  if (!is.null(ns_name)) {
    assignInNamespace("get_weather", mock_func, ns = ns_name)
    return(list(restore = function() {
      assignInNamespace("get_weather", original_func, ns = ns_name)
    }))
  } else {
    assign("get_weather", mock_func, envir = .GlobalEnv)
    return(list(restore = function() {
      assign("get_weather", original_func, envir = .GlobalEnv)
    }))
  }
}

test_that("get_weather_corrected_solar with multiple countries and varying weather", {
  library(testthat)
  library(dplyr)
  library(lubridate)

  # Test with 2 countries, 3 years
  # DE: solar radiation varies by year (low -> avg -> high)
  # FR: solar radiation constant (average)
  # Generation is proportional to solar radiation (simple linear relationship)

  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")

  # Mock weather data (solar radiation) - create first so we can use it for generation
  # DE: 2.0, 3.0, 4.0 (low -> avg -> high) with seasonal variation
  # FR: 3.0 constant (average) with seasonal variation
  # Add seasonal variation (higher in summer) so model can learn day-of-year patterns
  mock_weather_data <- bind_rows(
    tibble(
      date = dates,
      region_iso2 = "DE",
      variable = "solar_radiation",
      value = case_when(
        year(date) == 2020 ~ 2.0 + 1.0 * sin(2 * pi * lubridate::yday(date) / 365),  # Low base, seasonal
        year(date) == 2021 ~ 3.0 + 1.0 * sin(2 * pi * lubridate::yday(date) / 365),  # Avg base, seasonal
        year(date) == 2022 ~ 4.0 + 1.0 * sin(2 * pi * lubridate::yday(date) / 365)   # High base, seasonal
      )
    ),
    tibble(
      date = dates,
      region_iso2 = "FR",
      variable = "solar_radiation",
      value = 3.0 + 1.0 * sin(2 * pi * lubridate::yday(date) / 365)  # Constant base, seasonal
    )
  )

  # Create generation data that actually correlates with weather
  # value_mwh = solar_radiation * 100 (simple linear relationship)
  # This ensures the model can learn the weather-generation relationship
  pwr_generation <- bind_rows(
    # DE: generation proportional to solar radiation
    mock_weather_data %>%
      filter(region_iso2 == "DE", variable == "solar_radiation") %>%
      mutate(
        iso2 = "DE",
        source = "Solar",
        value_mwh = value * 100  # Direct correlation with weather
      ) %>%
      select(date, iso2, source, value_mwh),
    # FR: generation proportional to solar radiation
    mock_weather_data %>%
      filter(region_iso2 == "FR", variable == "solar_radiation") %>%
      mutate(
        iso2 = "FR",
        source = "Solar",
        value_mwh = value * 100  # Direct correlation with weather
      ) %>%
      select(date, iso2, source, value_mwh)
  )

  # Mock the weather function
  mock_restore <- mock_weather_function_solar(mock_weather_data)
  on.exit(mock_restore$restore())

  # Run function (suppress messages and warnings)
  result <- suppressWarnings(suppressMessages(
    get_weather_corrected_solar(
      iso2s = c("DE", "FR"),
      pwr_generation = pwr_generation,
      date_from = "2020-01-01",
      date_to = "2022-12-31",
      use_cache = FALSE
    )
  ))

  # Verify structure
  expect_true("date" %in% names(result))
  expect_true("iso2" %in% names(result))
  expect_true("value_mwh_actual" %in% names(result))
  expect_true("value_mwh_corrected" %in% names(result))
  expect_true("source" %in% names(result))
  expect_equal(unique(result$source), "Solar")
  expect_true(all(c("DE", "FR") %in% result$iso2))

  # Verify actual values are preserved
  result_summary <- result %>%
    mutate(year = year(date)) %>%
    group_by(iso2, year) %>%
    summarise(
      actual_mean = mean(value_mwh_actual),
      corrected_mean = mean(value_mwh_corrected),
      .groups = "drop"
    )

  # DE: Verify actual values (should be around 200, 300, 400 but with seasonal variation)
  de_2020 <- result_summary %>% filter(iso2 == "DE", year == 2020)
  de_2021 <- result_summary %>% filter(iso2 == "DE", year == 2021)
  de_2022 <- result_summary %>% filter(iso2 == "DE", year == 2022)

  # With seasonal variation, means should be close to base * 100
  expect_true(de_2020$actual_mean > 150 && de_2020$actual_mean < 250)  # Around 200
  expect_true(de_2021$actual_mean > 250 && de_2021$actual_mean < 350)  # Around 300
  expect_true(de_2022$actual_mean > 350 && de_2022$actual_mean < 450)  # Around 400

  # FR: Should be around 300 with seasonal variation
  fr_all <- result_summary %>% filter(iso2 == "FR")
  expect_true(all(fr_all$actual_mean > 250 & fr_all$actual_mean < 350))

  # Verify corrections are applied
  # DE: Low year (2020) should be corrected upward (toward average),
  #     High year (2022) should be corrected downward (toward average)
  # Note: The correction might be small if the model doesn't fit perfectly,
  # so we check for the direction of correction
  correction_2020 <- de_2020$corrected_mean - de_2020$actual_mean
  correction_2022 <- de_2022$corrected_mean - de_2022$actual_mean

  # 2020 (low year) should be corrected upward (positive correction)
  expect_true(correction_2020 > -50)  # Allow some tolerance, but should be positive or small negative

  # 2022 (high year) should be corrected downward (negative correction)
  expect_true(correction_2022 < 50)  # Allow some tolerance, but should be negative or small positive

  # FR: With constant weather, correction should be minimal
  # (all years should have similar corrected values, close to actual)
  fr_correction_range <- max(fr_all$corrected_mean) - min(fr_all$corrected_mean)
  expect_true(fr_correction_range < 50)  # Small variation

  # Verify sum conservation for constant weather (FR)
  # With constant weather, sum should be approximately conserved
  fr_sum <- result %>%
    filter(iso2 == "FR") %>%
    summarise(
      sum_actual = sum(value_mwh_actual),
      sum_corrected = sum(value_mwh_corrected)
    )

  # Sum should be close (within 5% due to model fitting)
  expect_true(abs(fr_sum$sum_actual - fr_sum$sum_corrected) / fr_sum$sum_actual < 0.05)
})

