library(testthat)
library(dplyr)
library(tibble)

# validate_co2_no_missing_required_keys -------------------------------------

test_that("validate_co2_no_missing_required_keys errors on missing sector keys", {
  co2 <- tibble(
    iso2 = "CY",
    date = as.Date("2023-01-01"),
    fuel = FUEL_OIL,
    sector = NA_character_,
    estimate = "central",
    unit = "t",
    value = 100
  )

  expect_error(
    validate_co2_no_missing_required_keys(co2),
    regexp = "missing required key values"
  )
})

test_that("validate_co2_no_missing_required_keys allows explicit unknown sector", {
  co2 <- tibble(
    iso2 = "CY",
    date = as.Date("2023-01-01"),
    fuel = FUEL_OIL,
    sector = SECTOR_UNKNOWN,
    estimate = "central",
    unit = "t",
    value = 100
  )

  expect_no_error(validate_co2_no_missing_required_keys(co2))
})

# validate_co2_no_sector_all_for_non_total_fuels ----------------------------

test_that("validate_co2_no_sector_all_for_non_total_fuels passes for correct output", {
  co2 <- tibble(
    iso2 = "DE",
    date = as.Date("2023-01-01"),
    fuel = c("oil", "oil", "oil", "oil", "total"),
    sector = c("electricity", "others", "transport_domestic",
               "transport_international_aviation", "all"),
    estimate = "central",
    value = c(100, 200, 500, 50, 850)
  )
  expect_no_error(validate_co2_no_sector_all_for_non_total_fuels(co2))
  expect_no_condition(validate_co2_no_sector_all_for_non_total_fuels(co2))
})

test_that("validate_co2_no_sector_all_for_non_total_fuels errors on oil/all", {
  co2 <- tibble(
    iso2 = "IT",
    date = as.Date("2023-10-01"),
    fuel = c("oil", "oil", "oil"),
    sector = c("all", "transport_domestic", "others"),
    estimate = "central",
    value = c(11000000, 9000000, 0)
  )
  expect_error(
    validate_co2_no_sector_all_for_non_total_fuels(co2),
    regexp = "sector='all' found for non-total fuel"
  )
})

test_that("validate_co2_no_sector_all_for_non_total_fuels errors on gas/all", {
  co2 <- tibble(
    iso2 = "DE",
    date = as.Date("2023-01-01"),
    fuel = c("gas", "gas", "total"),
    sector = c("all", "electricity", "all"),
    estimate = "central",
    value = c(500, 200, 700)
  )
  expect_error(
    validate_co2_no_sector_all_for_non_total_fuels(co2),
    regexp = "sector='all' found for non-total fuel"
  )
})

test_that("validate_co2_no_sector_all_for_non_total_fuels allows aggregate-only non-total rows", {
  co2 <- tibble(
    iso2 = "AT",
    date = as.Date(c("1990-01-01", "1990-02-01")),
    fuel = FUEL_OIL,
    sector = SECTOR_ALL,
    estimate = "central",
    unit = "t",
    value = c(100, 110)
  )

  expect_no_error(validate_co2_no_sector_all_for_non_total_fuels(co2))
})

test_that("validate_co2_no_sector_all_for_non_total_fuels is silent for total/all", {
  co2 <- tibble(
    iso2 = "EU",
    date = as.Date("2023-01-01"),
    fuel = "total",
    sector = "all",
    estimate = "central",
    value = 1e9
  )
  expect_no_error(validate_co2_no_sector_all_for_non_total_fuels(co2))
})

# validate_co2 integration --------------------------------------------------

test_that("validate_co2 stops the pipeline when sector=all exists for non-total fuel", {
  co2_bad <- tibble(
    iso2 = "EU",
    date = as.Date("2023-10-01"),
    fuel = c("oil", "oil", "total"),
    sector = c("all", "others", "all"),
    estimate = "central",
    value = c(11000000, 0, 11000000)
  )
  expect_error(
    validate_co2(co2_bad, diagnostics_folder = NULL),
    regexp = "sector='all' found for non-total fuel"
  )
})

# detotalise_co2 + validate integration -------------------------------------

test_that("detotalise_co2 removes oil/all and validate_co2 then passes", {
  # Build a data frame that mimics the broken June vintage structure:
  # oil/all present alongside oil/others = 0.
  co2_broken <- tibble(
    iso2 = "IT",
    date = rep(as.Date(c("2023-10-01", "2023-11-01")), each = 5),
    fuel = "oil",
    sector = rep(c("all", "electricity", "others",
                   "transport_domestic", "transport_international_aviation"), 2),
    estimate = "central",
    unit = "t",
    value = c(
      11661847, 0, 0, 9006529, 1113451,  # Oct: others wrongly 0
      11049026, 0, 0, 8539378,  820629   # Nov: others wrongly 0
    )
  )

  # Before detotalise: validator should error
  expect_error(
    validate_co2_no_sector_all_for_non_total_fuels(co2_broken),
    regexp = "sector='all' found for non-total fuel"
  )

  # After detotalise: validator should pass and oil/others is the residual
  co2_fixed <- detotalise_co2(co2_broken)

  expect_no_error(validate_co2_no_sector_all_for_non_total_fuels(co2_fixed))

  others_oct <- co2_fixed %>%
    filter(date == as.Date("2023-10-01"), sector == "others") %>%
    pull(value)

  expect_equal(others_oct, 11661847 - 0 - 9006529 - 1113451, tolerance = 1)
})
