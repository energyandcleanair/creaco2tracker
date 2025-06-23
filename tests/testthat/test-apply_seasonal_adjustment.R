test_that("apply_seasonal_adjustment works correctly with valid data", {
  # Create test data with complete monthly data for seasonal pattern calculation
  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    # Complete year 2020 for DE, sector A
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 100,
    "DE", "A", "2020-02-01", "TJ", "O4000", "OIL", 110,
    "DE", "A", "2020-03-01", "TJ", "O4000", "OIL", 120,
    "DE", "A", "2020-04-01", "TJ", "O4000", "OIL", 130,
    "DE", "A", "2020-05-01", "TJ", "O4000", "OIL", 140,
    "DE", "A", "2020-06-01", "TJ", "O4000", "OIL", 150,
    "DE", "A", "2020-07-01", "TJ", "O4000", "OIL", 160,
    "DE", "A", "2020-08-01", "TJ", "O4000", "OIL", 170,
    "DE", "A", "2020-09-01", "TJ", "O4000", "OIL", 180,
    "DE", "A", "2020-10-01", "TJ", "O4000", "OIL", 190,
    "DE", "A", "2020-11-01", "TJ", "O4000", "OIL", 200,
    "DE", "A", "2020-12-01", "TJ", "O4000", "OIL", 210,
    # Complete year 2021 for DE, sector A
    "DE", "A", "2021-01-01", "TJ", "O4000", "OIL", 105,
    "DE", "A", "2021-02-01", "TJ", "O4000", "OIL", 115,
    "DE", "A", "2021-03-01", "TJ", "O4000", "OIL", 125,
    "DE", "A", "2021-04-01", "TJ", "O4000", "OIL", 135,
    "DE", "A", "2021-05-01", "TJ", "O4000", "OIL", 145,
    "DE", "A", "2021-06-01", "TJ", "O4000", "OIL", 155,
    "DE", "A", "2021-07-01", "TJ", "O4000", "OIL", 165,
    "DE", "A", "2021-08-01", "TJ", "O4000", "OIL", 175,
    "DE", "A", "2021-09-01", "TJ", "O4000", "OIL", 185,
    "DE", "A", "2021-10-01", "TJ", "O4000", "OIL", 195,
    "DE", "A", "2021-11-01", "TJ", "O4000", "OIL", 205,
    "DE", "A", "2021-12-01", "TJ", "O4000", "OIL", 215
  ) %>%
    mutate(time = as.Date(time))

  # Create yearly data to be converted to monthly
  cons_yearly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 1860,  # Sum of 2020 monthly values
    "DE", "A", "2021-01-01", "TJ", "O4000", "OIL", 1920   # Sum of 2021 monthly values
  ) %>%
    mutate(time = as.Date(time))

  # Apply seasonal adjustment
  result <- apply_seasonal_adjustment(cons_yearly, cons_monthly)

  # Check that we get 24 rows (2 years * 12 months)
  expect_equal(nrow(result), 24)

  # Check that the monthly values sum back to the original yearly values
  yearly_sums <- result %>%
    group_by(iso2, sector, lubridate::year(time), unit, siec_code, fuel) %>%
    summarise(total = sum(values), .groups = "drop") %>%
    arrange(`lubridate::year(time)`)

  expect_equal(yearly_sums$total, c(1860, 1920), tolerance = 1e-10)

  # Check that the seasonal pattern is preserved
  # January should have the lowest values, December the highest
  jan_values <- result %>% filter(lubridate::month(time) == 1) %>% pull(values)
  dec_values <- result %>% filter(lubridate::month(time) == 12) %>% pull(values)
  expect_true(all(jan_values < dec_values))

  # Check that all required columns are present
  expected_cols <- c("iso2", "sector", "time", "unit", "siec_code", "fuel", "values")
  expect_equal(sort(colnames(result)), sort(expected_cols))
})

test_that("apply_seasonal_adjustment handles missing values correctly", {
  # Create test data with some missing values in monthly data
  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    # Incomplete year 2020 (missing December)
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 100,
    "DE", "A", "2020-02-01", "TJ", "O4000", "OIL", 110,
    "DE", "A", "2020-03-01", "TJ", "O4000", "OIL", 120,
    "DE", "A", "2020-04-01", "TJ", "O4000", "OIL", 130,
    "DE", "A", "2020-05-01", "TJ", "O4000", "OIL", 140,
    "DE", "A", "2020-06-01", "TJ", "O4000", "OIL", 150,
    "DE", "A", "2020-07-01", "TJ", "O4000", "OIL", 160,
    "DE", "A", "2020-08-01", "TJ", "O4000", "OIL", 170,
    "DE", "A", "2020-09-01", "TJ", "O4000", "OIL", 180,
    "DE", "A", "2020-10-01", "TJ", "O4000", "OIL", 190,
    "DE", "A", "2020-11-01", "TJ", "O4000", "OIL", 200,
    # December is missing
    # Complete year 2021
    "DE", "A", "2021-01-01", "TJ", "O4000", "OIL", 105,
    "DE", "A", "2021-02-01", "TJ", "O4000", "OIL", 115,
    "DE", "A", "2021-03-01", "TJ", "O4000", "OIL", 125,
    "DE", "A", "2021-04-01", "TJ", "O4000", "OIL", 135,
    "DE", "A", "2021-05-01", "TJ", "O4000", "OIL", 145,
    "DE", "A", "2021-06-01", "TJ", "O4000", "OIL", 155,
    "DE", "A", "2021-07-01", "TJ", "O4000", "OIL", 165,
    "DE", "A", "2021-08-01", "TJ", "O4000", "OIL", 175,
    "DE", "A", "2021-09-01", "TJ", "O4000", "OIL", 185,
    "DE", "A", "2021-10-01", "TJ", "O4000", "OIL", 195,
    "DE", "A", "2021-11-01", "TJ", "O4000", "OIL", 205,
    "DE", "A", "2021-12-01", "TJ", "O4000", "OIL", 215
  ) %>%
    mutate(time = as.Date(time))

  cons_yearly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 1860,
    "DE", "A", "2021-01-01", "TJ", "O4000", "OIL", 1920
  ) %>%
    mutate(time = as.Date(time))

  # Should work because 2021 has complete data for seasonal pattern
  result <- apply_seasonal_adjustment(cons_yearly, cons_monthly)

  # Should get 24 rows (2 years * 12 months)
  expect_equal(nrow(result), 24)

  # 2021 should sum to original value
  year_2021_sum <- result %>%
    filter(lubridate::year(time) == 2021) %>%
    summarise(total = sum(values)) %>%
    pull(total)
  expect_equal(year_2021_sum, 1920, tolerance = 1e-10)
})

test_that("apply_seasonal_adjustment throws error for incomplete data", {
  # Create test data with NA values
  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 100,
    "DE", "A", "2020-02-01", "TJ", "O4000", "OIL", NA,  # NA value
    "DE", "A", "2020-03-01", "TJ", "O4000", "OIL", 120
  ) %>%
    mutate(time = as.Date(time))

  cons_yearly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 1860
  ) %>%
    mutate(time = as.Date(time))

  # Should throw error due to NA values
  expect_error(
    apply_seasonal_adjustment(cons_yearly, cons_monthly),
    "There are NA values in the data"
  )
})

test_that("apply_seasonal_adjustment handles multiple countries and sectors", {
  # Create test data with multiple countries and sectors
  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    # DE, sector A, 2020 (complete)
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 100,
    "DE", "A", "2020-02-01", "TJ", "O4000", "OIL", 110,
    "DE", "A", "2020-03-01", "TJ", "O4000", "OIL", 120,
    "DE", "A", "2020-04-01", "TJ", "O4000", "OIL", 130,
    "DE", "A", "2020-05-01", "TJ", "O4000", "OIL", 140,
    "DE", "A", "2020-06-01", "TJ", "O4000", "OIL", 150,
    "DE", "A", "2020-07-01", "TJ", "O4000", "OIL", 160,
    "DE", "A", "2020-08-01", "TJ", "O4000", "OIL", 170,
    "DE", "A", "2020-09-01", "TJ", "O4000", "OIL", 180,
    "DE", "A", "2020-10-01", "TJ", "O4000", "OIL", 190,
    "DE", "A", "2020-11-01", "TJ", "O4000", "OIL", 200,
    "DE", "A", "2020-12-01", "TJ", "O4000", "OIL", 210,
    # FR, sector B, 2020 (complete)
    "FR", "B", "2020-01-01", "TJ", "G3000", "GAS", 50,
    "FR", "B", "2020-02-01", "TJ", "G3000", "GAS", 55,
    "FR", "B", "2020-03-01", "TJ", "G3000", "GAS", 60,
    "FR", "B", "2020-04-01", "TJ", "G3000", "GAS", 65,
    "FR", "B", "2020-05-01", "TJ", "G3000", "GAS", 70,
    "FR", "B", "2020-06-01", "TJ", "G3000", "GAS", 75,
    "FR", "B", "2020-07-01", "TJ", "G3000", "GAS", 80,
    "FR", "B", "2020-08-01", "TJ", "G3000", "GAS", 85,
    "FR", "B", "2020-09-01", "TJ", "G3000", "GAS", 90,
    "FR", "B", "2020-10-01", "TJ", "G3000", "GAS", 95,
    "FR", "B", "2020-11-01", "TJ", "G3000", "GAS", 100,
    "FR", "B", "2020-12-01", "TJ", "G3000", "GAS", 105
  ) %>%
    mutate(time = as.Date(time))

  cons_yearly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 1860,
    "FR", "B", "2020-01-01", "TJ", "G3000", "GAS", 930
  ) %>%
    mutate(time = as.Date(time))

  # Apply seasonal adjustment
  result <- apply_seasonal_adjustment(cons_yearly, cons_monthly)

  # Should get 24 rows (2 combinations * 12 months)
  expect_equal(nrow(result), 24)

  # Check that each country-sector combination sums to original yearly value
  yearly_sums <- result %>%
    group_by(iso2, sector, unit, siec_code, fuel) %>%
    summarise(total = sum(values), .groups = "drop") %>%
    arrange(iso2, sector)

  expect_equal(yearly_sums$total, c(1860, 930), tolerance = 1e-10)

  # Check that different sectors have different seasonal patterns
  de_pattern <- result %>%
    filter(iso2 == "DE", sector == "A") %>%
    arrange(time) %>%
    pull(values)

  fr_pattern <- result %>%
    filter(iso2 == "FR", sector == "B") %>%
    arrange(time) %>%
    pull(values)

  # Patterns should be different (different fuels/sectors)
  expect_false(identical(de_pattern, fr_pattern))
})

test_that("apply_seasonal_adjustment handles edge cases", {
  # Test with zero values
  cons_monthly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-02-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-03-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-04-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-05-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-06-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-07-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-08-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-09-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-10-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-11-01", "TJ", "O4000", "OIL", 0,
    "DE", "A", "2020-12-01", "TJ", "O4000", "OIL", 0
  ) %>%
    mutate(time = as.Date(time))

  cons_yearly <- tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec_code, ~fuel, ~values,
    "DE", "A", "2020-01-01", "TJ", "O4000", "OIL", 0
  ) %>%
    mutate(time = as.Date(time))

  # Should handle zero values gracefully
  result <- apply_seasonal_adjustment(cons_yearly, cons_monthly)

  expect_equal(nrow(result), 12)
  expect_true(all(result$values == 0))
})
