library(testthat)
library(dplyr)
library(lubridate)

# Test data setup
test_data <- function() {
  tribble(
    ~time, ~unit, ~siec_code, ~values,
    "2020-01-01", "TJ", "C0100", 100,  # SIEC_HARD_COAL
    "2020-01-01", "TJ", "C0200", 50,   # SIEC_BROWN_COAL
    "2020-01-01", "TJ", "C0330", 25,   # SIEC_BROWN_COAL_BRIQUETTES
    "2020-02-01", "TJ", "C0100", 110,
    "2020-02-01", "TJ", "C0200", 55,
    "2020-02-01", "TJ", "C0330", 30
  ) %>%
    mutate(time = as.Date(time)) %>%
    mutate(iso2 = "DE")
}

test_that("create_siec_fuel_mapping returns correct structure", {
  mapping <- create_siec_fuel_mapping()

  expect_true(is.data.frame(mapping))
  expect_equal(ncol(mapping), 2)
  expect_equal(colnames(mapping), c("siec_code", "product_raw"))
  expect_true(all(c("C0100", "C0200", "C0330") %in% mapping$siec_code))
})


test_that("validate_ncv_completeness catches missing NCV values", {
  # Test data with missing NCV
  test_data_with_ncv <- test_data() %>%
    mutate(
      iso2 = "DE",
      ncv_kjkg = c(25000, 15000, 18000, 25000, 15000, 18000)
    )

  # This should pass
  expect_true(validate_ncv_completeness(test_data_with_ncv))

  # Test data with missing NCV values
  test_data_missing_ncv <- test_data() %>%
    mutate(
      iso2 = "DE",
      ncv_kjkg = c(25000, NA, 18000, 25000, 15000, NA)
    )

  # This should fail
  expect_error(validate_ncv_completeness(test_data_missing_ncv))
})

test_that("validate_ncv_completeness handles ME country code correctly", {
  # Test data with ME country (should be excluded from validation)
  test_data_me <- test_data() %>%
    mutate(
      iso2 = "ME",
      ncv_kjkg = c(25000, NA, 18000, 25000, 15000, NA)
    )

  # This should pass because ME is excluded
  expect_true(validate_ncv_completeness(test_data_me))
})

test_that("validate_ncv_completeness handles Terajoule units correctly", {
  # Test data with Terajoule units (should be excluded from validation)
  test_data_terajoule <- test_data() %>%
    mutate(
      iso2 = "DE",
      unit = "Terajoule",
      ncv_kjkg = c(25000, NA, 18000, 25000, 15000, NA)
    )

  # This should pass because Terajoule units are excluded
  expect_true(validate_ncv_completeness(test_data_terajoule))
})

test_that("make_ncv_time_insensitive works correctly", {
  test_data_with_ncv <- test_data() %>%
    mutate(
      iso2 = "DE",
      ncv_kjkg = c(25000, 15000, 18000, 26000, 16000, 19000)
    )

  result <- make_ncv_time_insensitive(test_data_with_ncv)

  # Check that NCV values are averaged by iso2 and siec_code
  expect_equal(nrow(result), nrow(test_data_with_ncv))

  # Check that NCV values are consistent within groups
  ncv_by_group <- result %>%
    group_by(iso2, siec_code) %>%
    summarise(
      ncv_mean = mean(ncv_kjkg),
      ncv_sd = sd(ncv_kjkg),
      .groups = "drop"
    )

  expect_true(all(ncv_by_group$ncv_sd == 0))
})

test_that("add_ncv_to_data handles missing conversion factors gracefully", {
  # Mock conversion factors data
  mock_conversion_filled <- tribble(
    ~iso2, ~siec_code, ~year, ~ncv_kjkg, ~source,
    "DE", "C0100", 2020, 25000, "IEA",
    "DE", "C0200", 2020, 15000, "IEA"
    # Missing C0330
  )

  test_data_subset <- test_data() %>%
    filter(siec_code %in% c("C0100", "C0200"))

  result <- add_ncv_to_data(test_data_subset, mock_conversion_filled)

  expect_true("ncv_kjkg" %in% colnames(result))
  expect_equal(nrow(result), nrow(test_data_subset))
})

test_that("calculate_weighted_means works correctly", {
  # Mock conversion data
  mock_conversion <- tribble(
    ~iso2, ~siec_code, ~year, ~ncv_kjkg,
    "DE", "C0100", 2020, 25000,
    "DE", "C0200", 2020, 15000
  )

  test_data_subset <- test_data() %>%
    filter(siec_code %in% c("C0100", "C0200"))

  result <- calculate_weighted_means(mock_conversion, test_data_subset)

  expect_true("ncv_kjkg_wmean" %in% colnames(result))
  expect_true(all(!is.na(result$ncv_kjkg_wmean)))
})

test_that("fill_missing_conversion_factors handles edge cases", {
  # Mock conversion data with gaps
  mock_conversion <- tribble(
    ~iso2, ~siec_code, ~year, ~ncv_kjkg, ~source,
    "DE", "C0100", 2020, 25000, "IEA"
    # Missing 2021 data
  )

  # Mock weighted means
  mock_wmean <- tribble(
    ~siec_code, ~year, ~ncv_kjkg_wmean,
    "C0100", 2021, 25000
  )

  test_data_subset <- test_data() %>%
    filter(siec_code == "C0100")

  result <- fill_missing_conversion_factors(mock_conversion, mock_wmean, test_data_subset)

  expect_true("ncv_kjkg" %in% colnames(result))
  expect_true(all(!is.na(result$ncv_kjkg)))
})

# Integration test for the main function
test_that("add_ncv_iea function structure is correct", {
  # This test checks the function structure without running the full pipeline
  # which requires external IEA data

  # Check that the function exists and has the right signature
  expect_true(exists("add_ncv_iea"))
  expect_true(is.function(add_ncv_iea))

  # Check that helper functions exist
  expect_true(exists("create_siec_fuel_mapping"))
  expect_true(exists("add_siec_code_to_iea"))
  expect_true(exists("process_conversion_factors"))
  expect_true(exists("calculate_weighted_means"))
  expect_true(exists("fill_missing_conversion_factors"))
  expect_true(exists("add_ncv_to_data"))
  expect_true(exists("validate_ncv_completeness"))
  expect_true(exists("make_ncv_time_insensitive"))
})

# Test for common failure scenarios
test_that("identify potential failure scenarios", {
  # Test 1: Check if all SIEC codes in test data have mappings
  test_siec_codes <- unique(test_data()$siec_code)
  mapping <- create_siec_fuel_mapping()

  unmapped_siec <- setdiff(test_siec_codes, mapping$siec_code)
  if(length(unmapped_siec) > 0) {
    cat("Unmapped SIEC codes in test data:", paste(unmapped_siec, collapse=", "), "\n")
  }

  # Test 2: Check if all required columns exist in test data
  required_cols <- c("time", "unit", "siec_code", "values")
  missing_cols <- setdiff(required_cols, colnames(test_data()))
  expect_equal(length(missing_cols), 0,
               info = paste("Missing required columns:", paste(missing_cols, collapse=", ")))

  # Test 3: Check data types
  expect_true(is.Date(test_data()$time))
  expect_true(is.character(test_data()$unit))
  expect_true(is.character(test_data()$siec_code))
  expect_true(is.numeric(test_data()$values))
})
