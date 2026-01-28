library(testthat)
library(dplyr)
library(lubridate)

# Helper function to mock get_weather for wind
mock_weather_function_wind <- function(mock_data) {
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

test_that("get_weather_corrected_wind with multiple countries and varying weather", {
  library(testthat)
  library(dplyr)
  library(lubridate)

  # Test with 2 countries, 3 years
  # DE: wind speed varies by year (low -> avg -> high) with daily variation
  # FR: wind speed around average with daily variation
  # Generation is related to wind speed (cubic relationship typical for wind)

  set.seed(42)  # For reproducibility
  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
  n_days <- length(dates)

  # Create wind speed with daily variation
  # DE: yearly means of 5, 7, 9 m/s with +/- 3 m/s daily variation
  de_wind <- case_when(
    year(dates) == 2020 ~ 5.0 + rnorm(n_days, 0, 1.5),
    year(dates) == 2021 ~ 7.0 + rnorm(n_days, 0, 1.5),
    year(dates) == 2022 ~ 9.0 + rnorm(n_days, 0, 1.5)
  )
  de_wind <- pmax(de_wind, 1)  # Minimum wind speed of 1 m/s

  # FR: constant mean of 7 m/s with daily variation
  fr_wind <- 7.0 + rnorm(n_days, 0, 1.5)
  fr_wind <- pmax(fr_wind, 1)

  # Create generation data: value_mwh related to wind speed (cubic relationship)
  # P = k * v^3 (simplified wind power formula)
  pwr_generation <- bind_rows(
    tibble(
      date = dates,
      iso2 = "DE",
      source = "Wind",
      value_mwh = 10 * de_wind^3  # Cubic relationship
    ),
    tibble(
      date = dates,
      iso2 = "FR",
      source = "Wind",
      value_mwh = 10 * fr_wind^3  # Cubic relationship
    )
  )

  # Mock weather data with daily variation
  mock_weather_data <- bind_rows(
    tibble(
      date = dates,
      region_iso2 = "DE",
      variable = "ws50m_daily",
      value = de_wind
    ),
    tibble(
      date = dates,
      region_iso2 = "DE",
      variable = "t10m_daily",
      value = 10.0  # Constant temperature
    ),
    tibble(
      date = dates,
      region_iso2 = "FR",
      variable = "ws50m_daily",
      value = fr_wind
    ),
    tibble(
      date = dates,
      region_iso2 = "FR",
      variable = "t10m_daily",
      value = 10.0  # Constant temperature
    )
  )
  
  # Mock the weather function
  mock_restore <- mock_weather_function_wind(mock_weather_data)
  on.exit(mock_restore$restore())
  
  # Run function (suppress messages and warnings)
  result <- suppressWarnings(suppressMessages(
    get_weather_corrected_wind(
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
  expect_equal(unique(result$source), "Wind")
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
  
  # DE: Verify actual values follow expected pattern (2020 < 2021 < 2022)
  de_2020 <- result_summary %>% filter(iso2 == "DE", year == 2020)
  de_2021 <- result_summary %>% filter(iso2 == "DE", year == 2021)
  de_2022 <- result_summary %>% filter(iso2 == "DE", year == 2022)

  # Actual values should increase with wind speed (cubic relationship)
  expect_true(de_2020$actual_mean < de_2021$actual_mean)
  expect_true(de_2021$actual_mean < de_2022$actual_mean)

  # FR: Generation should be relatively constant across years
  fr_all <- result_summary %>% filter(iso2 == "FR")
  fr_cv <- sd(fr_all$actual_mean) / mean(fr_all$actual_mean)
  expect_true(fr_cv < 0.15)  # Coefficient of variation < 15%

  # Verify corrections are applied
  # DE: Low wind year should be corrected upward, high wind year downward
  expect_true(de_2020$corrected_mean > de_2020$actual_mean)
  expect_true(de_2022$corrected_mean < de_2022$actual_mean)

  # FR: With similar weather each year, correction should reduce year-to-year variance
  fr_actual_range <- max(fr_all$actual_mean) - min(fr_all$actual_mean)
  fr_corrected_range <- max(fr_all$corrected_mean) - min(fr_all$corrected_mean)
  # Corrected values should have similar or smaller range than actual
  expect_true(fr_corrected_range <= fr_actual_range * 1.5)

  # Verify sum conservation (total energy should be approximately preserved)
  fr_sum <- result %>%
    filter(iso2 == "FR") %>%
    summarise(
      sum_actual = sum(value_mwh_actual),
      sum_corrected = sum(value_mwh_corrected)
    )

  # Sum should be close (within 10% due to model fitting)
  expect_true(abs(fr_sum$sum_actual - fr_sum$sum_corrected) / fr_sum$sum_actual < 0.10)
})

