library(testthat)
library(dplyr)
library(lubridate)
library(glue)
library(creahelpers)

# Source the weather.R file to get the latest version of get_weather
# This allows testing without rebuilding the package
# source("../../R/weather.R")
# source("../../R/helpers.R")  # For get_eu_iso2s()

test_that("get_weather works with HDD and CDD for EU", {
  result <- get_weather(
    variable = "HDD,CDD",
    region_id = "EU",
    date_from = "2024-01-01",
    date_to = "2024-01-31"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("date", "variable", "value") %in% colnames(result)))
  expect_true(all(result$variable %in% c("hdd", "cdd")))
  expect_equal(min(as.Date(result$date)), as.Date("2024-01-01"))
})

test_that("get_weather works with solar_radiation for single country", {
  result <- get_weather(
    variable = "solar_radiation",
    region_iso2 = "DE",
    region_type = "country",
    date_from = "2024-01-01",
    date_to = "2024-01-31"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("solar_radiation" %in% result$variable)
  expect_true(all(result$region_iso2 == "DE"))
})

test_that("get_weather works with wind speed from stations", {
  skip_if_not(Sys.getenv("TEST_LOCAL_API") == "TRUE",
              "Skipping local API test - set TEST_LOCAL_API=TRUE to run")

  result <- get_weather(
    variable = "ws50m_daily",
    region_iso2 = "DE",
    region_type = "station",
    station_source = "gem",
    date_from = "2024-01-01",
    date_to = "2024-01-31",
    use_local = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("ws50m_daily" %in% result$variable)
  expect_true(length(unique(result$region_id)) > 1)  # Multiple stations
})

test_that("get_weather works with multiple region_ids", {
  # Get a few EU countries
  eu_countries <- get_eu_iso2s()[1:3]  # Just test with first 3 for speed

  result <- get_weather(
    variable = "solar_radiation",
    region_iso2 = paste(eu_countries, collapse = ","),
    region_type = "country",
    date_from = "2024-01-01",
    date_to = "2024-01-10"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check that we got data for multiple countries
  unique_countries <- unique(result$region_iso2)
  expect_true(length(unique_countries) > 1)

  # Check that all returned countries are in our requested list
  expect_true(all(unique_countries %in% eu_countries))
})

test_that("get_weather works with all EU countries", {
  # Test with all EU ISO2s
  eu_iso2s <- get_eu_iso2s()

  result <- get_weather(
    variable = "HDD",
    region_iso2 = paste(eu_iso2s, collapse = ","),
    region_type = "country",
    date_from = "2024-01-01",
    date_to = "2024-01-07"  # Short range for speed
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check that we got data for multiple countries
  unique_countries <- unique(result$region_iso2)
  expect_true(length(unique_countries) >= 10)  # Should have at least 10 EU countries

  # All results should be for HDD
  expect_true(all(result$variable == "hdd"))
})

test_that("get_weather caching works correctly", {
  params <- list(
    variable = "CDD",
    region_iso2 = "ES",
    region_type = "country",
    date_from = "2024-06-01",
    date_to = "2024-06-07"
  )

  # First call
  result1 <- do.call(get_weather, c(params, list(use_cache = TRUE)))

  # Second call (should use cache)
  result2 <- do.call(get_weather, c(params, list(use_cache = TRUE)))

  # Results should be identical
  expect_identical(result1, result2)

  # Third call with refresh
  result3 <- do.call(get_weather, c(params, list(use_cache = FALSE)))

  # Should still have same data (but freshly fetched)
  expect_equal(nrow(result1), nrow(result3))
})

test_that("get_weather handles multiple variables", {
  result <- get_weather(
    variable = "HDD,CDD",
    region_iso2 = "FR",
    region_type = "country",
    date_from = "2024-01-01",
    date_to = "2024-01-15"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Should have both variables
  unique_vars <- unique(result$variable)
  expect_true("hdd" %in% unique_vars)
  expect_true("cdd" %in% unique_vars)
})

test_that("get_weather returns lowercase variable names", {
  result <- get_weather(
    variable = "HDD,CDD",
    region_id = "EU",
    date_from = "2024-01-01",
    date_to = "2024-01-05"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(result$variable %in% c("hdd", "cdd")))
  expect_false(any(result$variable %in% c("HDD", "CDD")))
})

test_that("get_weather handles date range correctly", {
  result <- get_weather(
    variable = "HDD",
    region_iso2 = "IT",
    region_type = "country",
    date_from = "2024-02-01",
    date_to = "2024-02-29"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  result_dates <- as.Date(result$date)
  expect_true(min(result_dates) >= as.Date("2024-02-01"))
  expect_true(max(result_dates) <= as.Date("2024-02-29"))
})

test_that("get_weather required columns are present", {
  result <- get_weather(
    variable = "solar_radiation",
    region_iso2 = "PT",
    region_type = "country",
    date_from = "2024-01-01",
    date_to = "2024-01-10"
  )

  # Check required columns exist
  required_cols <- c("date", "variable", "value")
  expect_true(all(required_cols %in% colnames(result)))

  # Check no missing values in key columns
  expect_false(any(is.na(result$date)))
  expect_false(any(is.na(result$variable)))
})
