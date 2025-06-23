# Setup constants for testing
SIEC_NATURAL_GAS <- "G3000"
SIEC_COKE_OVEN_COKE <- "C0311"
SIEC_KEROSENE_XBIO <- "O4661XR5230B"
SIEC_HARD_COAL <- "C0100"
SIEC_FUEL_OIL <- "O4680"
FUEL_OIL <- "oil"


test_that("combine_monthly_yearly_with_cutoff works correctly with valid data", {
  # Create test data
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2019-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 100,
    "DE", "A", "2019-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 110,
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 120,
    "DE", "A", "2020-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 130,
    "DE", "A", "2021-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 140,
    "DE", "A", "2021-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 150
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2019-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 95,   # Before cutoff, should be filtered out
    "DE", "A", "2019-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 105,  # Before cutoff, should be filtered out
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 125,  # After cutoff, should be kept
    "DE", "A", "2020-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 135,  # After cutoff, should be kept
    "DE", "A", "2021-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 145,  # After cutoff, should be kept
    "DE", "A", "2021-02-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 155   # After cutoff, should be kept
  ) %>%
    mutate(time = as.Date(time))

  # Apply cutoff filtering
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should get 6 rows (3 time periods * 2 sources, but monthly filtered for early dates)
  expect_equal(nrow(result), 6)

  # Check that monthly data before cutoff date (2020-01-01) uses yearly data
  early_dates <- result %>%
    filter(time < as.Date("2020-01-01")) %>%
    arrange(time)

  expect_equal(early_dates$values, c(100, 110))  # Should be yearly values
  expect_equal(early_dates$source, c("yearly", "yearly"))

  # Check that monthly data after cutoff date uses monthly data
  late_dates <- result %>%
    filter(time >= as.Date("2020-01-01")) %>%
    arrange(time)

  expect_equal(late_dates$values, c(125, 135, 145, 155))  # Should be monthly values
  expect_equal(late_dates$source, c("monthly", "monthly", "monthly", "monthly"))
})

test_that("combine_monthly_yearly_with_cutoff handles different fuel types correctly", {
  # Create test data with different fuel types
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2018-01-01", "TJ", SIEC_NATURAL_GAS, "GAS", 100,   # Natural gas, cutoff 2020-01-01
    "DE", "A", "2019-01-01", "TJ", SIEC_HARD_COAL, "COAL", 200,  # Hard coal, cutoff 2020-01-01
    "DE", "A", "2018-01-01", "TJ", SIEC_HARD_COAL, "COAL", 300   # Hard coal, cutoff 2020-01-01
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2018-01-01", "TJ", SIEC_NATURAL_GAS, "GAS", 95,    # Before cutoff, should be filtered
    "DE", "A", "2019-01-01", "TJ", SIEC_HARD_COAL, "COAL", 205,  # Before cutoff, should be filtered
    "DE", "A", "2018-01-01", "TJ", SIEC_HARD_COAL, "COAL", 305   # Before cutoff, should be filtered
  ) %>%
    mutate(time = as.Date(time))

  # Apply cutoff filtering
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should get 3 rows (all should use yearly data due to cutoff dates)
  expect_equal(nrow(result), 3)
  expect_true(all(result$source == "yearly"))
})

test_that("combine_monthly_yearly_with_cutoff handles country-specific cutoffs", {
  # Create test data for Portugal fuel oil (special case)
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "PT", "A", "2022-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 100,  # Before Portugal cutoff (2023-01-01)
    "PT", "A", "2023-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 110,  # After Portugal cutoff
    "DE", "A", "2022-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 200,  # Germany, normal cutoff (2020-01-01)
    "DE", "A", "2023-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 210   # Germany, after normal cutoff
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "PT", "A", "2022-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 95,   # Before Portugal cutoff, should be filtered
    "PT", "A", "2023-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 115,  # After Portugal cutoff, should be kept
    "DE", "A", "2022-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 205,  # After Germany cutoff, should be kept
    "DE", "A", "2023-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 215   # After Germany cutoff, should be kept
  ) %>%
    mutate(time = as.Date(time))

  # Apply cutoff filtering
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should get 4 rows
  expect_equal(nrow(result), 4)

  # Portugal 2022 should use yearly (before Portugal cutoff)
  pt_2022 <- result %>%
    filter(iso2 == "PT", time == as.Date("2022-01-01"))
  expect_equal(pt_2022$values, 100)
  expect_equal(pt_2022$source, "yearly")

  # Portugal 2023 should use monthly (after Portugal cutoff)
  pt_2023 <- result %>%
    filter(iso2 == "PT", time == as.Date("2023-01-01"))
  expect_equal(pt_2023$values, 115)
  expect_equal(pt_2023$source, "monthly")

  # Germany 2022 should use monthly (after Germany cutoff)
  de_2022 <- result %>%
    filter(iso2 == "DE", time == as.Date("2022-01-01"))
  expect_equal(de_2022$values, 205)
  expect_equal(de_2022$source, "monthly")
})

test_that("combine_monthly_yearly_with_cutoff handles missing data gracefully", {
  # Create test data with some missing combinations
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 100
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 110
  ) %>%
    mutate(time = as.Date(time))

  # Should work without errors
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should get 1 row (monthly data after cutoff)
  expect_equal(nrow(result), 1)
  expect_equal(result$values, 110)
  expect_equal(result$source, "monthly")
})

test_that("combine_monthly_yearly_with_cutoff prioritizes monthly over yearly when both available", {
  # Create test data where both monthly and yearly are available after cutoff
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 100
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", SIEC_FUEL_OIL, FUEL_OIL, 110
  ) %>%
    mutate(time = as.Date(time))

  # Apply cutoff filtering
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should prioritize monthly data (source = "monthly" comes first in arrange)
  expect_equal(result$values, 110)
  expect_equal(result$source, "monthly")
})

test_that("combine_monthly_yearly_with_cutoff handles edge cases with no monthly data", {
  # Create test data with only yearly data
  cons_yearly_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 100,
    "DE", "A", "2021-01-01", "TJ", "O4000", "OIL", 110
  ) %>%
    mutate(time = as.Date(time))

  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values
  ) %>%
    mutate(time = as.Date(character(0)))

  # Should work and return yearly data
  result <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  # Should get 2 rows (all yearly data)
  expect_equal(nrow(result), 2)
  expect_true(all(result$source == "yearly"))
  expect_equal(result$values, c(100, 110))
})


