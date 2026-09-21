validation_series <- function(iso2, first_year, growth, source) {
  values <- Reduce(function(value, rate) value * (1 + rate), growth,
    init = 100, accumulate = TRUE)
  tibble::tibble(iso2 = iso2, year = first_year + seq_along(values) - 1L,
    value = values, source = source, sector = SECTOR_ALL, fuel = FUEL_TOTAL)
}

co2_validation_series <- function(iso2, first_year, growth) {
  annual <- validation_series(iso2, first_year, growth, "CREA")
  purrr::map_df(seq_len(nrow(annual)), function(index) {
    dates <- seq(as.Date(sprintf("%04d-01-01", annual$year[[index]])),
      as.Date(sprintf("%04d-12-31", annual$year[[index]])), by = "day")
    tibble::tibble(iso2 = iso2, date = dates, fuel = FUEL_TOTAL,
      sector = SECTOR_ALL, estimate = "central",
      value = annual$value[[index]] * 1e6 / length(dates))
  })
}

test_that("short validation windows enforce error and make correlation advisory", {
  crea_growth <- c(-0.037240132, -0.067851616, -0.046606218)
  gcb_growth <- c(-0.0403, -0.0854, -0.0234)
  co2 <- co2_validation_series("FR", 2021, crea_growth)
  validation <- validation_series("FR", 2021, gcb_growth, "Global Carbon Budget 2025")

  result <- get_validity_metrics(co2, validation, min_year = 2022)

  expect_equal(result$n_years, 3)
  expect_lt(result$correlation, 0.9)
  expect_lt(result$mae, 0.03)
  expect_false(result$correlation_enforced)
  expect_true(result$ok)
  expect_equal(result$reason, "passed_mae_correlation_advisory")
})

test_that("five-year validation windows enforce correlation", {
  crea_growth <- c(0.01, 0.02, 0.01, 0.02, 0.01)
  gcb_growth <- c(0.02, 0.01, 0.02, 0.01, 0.02)
  co2 <- co2_validation_series("AA", 2019, crea_growth)
  validation <- validation_series("AA", 2019, gcb_growth, "Global Carbon Budget 2025")

  result <- get_validity_metrics(co2, validation, min_year = 2020)

  expect_equal(result$n_years, 5)
  expect_true(result$mae_ok)
  expect_true(result$correlation_enforced)
  expect_false(result$correlation_ok)
  expect_false(result$ok)
  expect_equal(result$reason, "correlation_below_threshold")
})

test_that("incomplete calendar years and nonconsecutive comparisons are excluded", {
  co2 <- co2_validation_series("AA", 2020, c(0.01, 0.01, 0.01)) %>%
    filter(!(lubridate::year(date) == 2022 & lubridate::month(date) == 12))
  validation <- validation_series("AA", 2020, c(0.01, 0.01, 0.01),
    "Global Carbon Budget 2025")

  result <- get_validity_metrics(co2, validation, min_year = 2021)

  expect_equal(result$n_years, 1)
  expect_false(result$ok)
  expect_equal(result$reason, "insufficient_comparable_years")
})
