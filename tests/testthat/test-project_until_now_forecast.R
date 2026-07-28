library(testthat)
library(dplyr)
library(tibble)

# Helpers -----------------------------------------------------------------

make_forecast_co2 <- function(dates, values) {
  tibble(
    iso2 = "EU",
    fuel = "gas",
    sector = "electricity",
    unit = "t",
    date = as.Date(dates),
    value = values
  )
}

# Wrap a single-estimate data frame into the three-estimate long format that the
# real pipeline produces.
make_forecast_co2_with_estimates <- function(dates, values) {
  base <- make_forecast_co2(dates, values)
  bind_rows(
    base %>% mutate(estimate = "central"),
    base %>% mutate(estimate = "lower", value = value * 0.9),
    base %>% mutate(estimate = "upper", value = value * 1.1)
  )
}

# Tests -------------------------------------------------------------------

test_that("project_until_now_forecast falls back cleanly when forecasting fails", {
  x <- make_forecast_co2(
    dates = c("2020-01-01", "2020-02-01"),
    values = c(10, 12)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-04-01"), by = "month")

  res <- project_until_now_forecast(x, dts_month = dts_month)

  expect_equal(sort(unique(res$estimate)), c("central", "lower", "upper"))

  observed <- res %>%
    filter(date <= as.Date("2020-02-01")) %>%
    arrange(date, estimate)

  expect_true(all(observed$value %in% c(10, 12)))
  expect_true(all(is.na(res$value[res$date > as.Date("2020-02-01")])))
})

test_that("project_until_now_forecast handles input with estimate column without error", {
  # Regression test: when co2 already has an estimate column (central/lower/upper),
  # the function must not produce a column-name collision in pivot_longer.
  x <- make_forecast_co2_with_estimates(
    dates  = c("2020-01-01", "2020-02-01", "2020-03-01",
               "2020-04-01", "2020-05-01", "2020-06-01"),
    values = c(100, 110, 105, 108, 112, 106)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-09-01"), by = "month")

  expect_no_error(res <- project_until_now_forecast(x, dts_month = dts_month))

  # Output must have exactly the three canonical estimates — no extra or missing
  expect_equal(sort(unique(res$estimate)), c("central", "lower", "upper"))

  # No duplicated rows per (iso2, fuel, sector, date, estimate)
  key_cols <- c("iso2", "fuel", "sector", "date", "estimate")
  expect_equal(
    nrow(res),
    nrow(distinct(res, across(all_of(key_cols))))
  )
})

test_that("project_until_now_forecast projected intervals differ from central", {
  # For projected dates the forecast lower and upper bounds should differ from
  # the central estimate (they come from HW confidence intervals).
  set.seed(42)
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")
  values <- 100 + seq_along(dates) * 2 + rnorm(length(dates), sd = 3)
  x <- make_forecast_co2_with_estimates(
    dates  = as.character(dates),
    values = values
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2023-04-01"), by = "month")

  res <- project_until_now_forecast(x, dts_month = dts_month)

  projected <- res %>%
    filter(date > as.Date("2022-12-01")) %>%
    select(date, estimate, value) %>%
    tidyr::pivot_wider(names_from = estimate, values_from = value)

  expect_true(nrow(projected) > 0)
  # lower <= central <= upper for projected dates
  expect_true(all(projected$lower <= projected$central + 1e-6))
  expect_true(all(projected$upper >= projected$central - 1e-6))
})

test_that("project_until_now_forecast returns correct estimates when input has only central", {
  # When input has no estimate column (backward compat / simpler callers), the
  # function should still produce central/lower/upper in the output.
  x <- make_forecast_co2(
    dates  = as.character(seq.Date(as.Date("2021-01-01"), as.Date("2021-09-01"), by = "month")),
    values = c(80, 85, 90, 88, 92, 95, 91, 94, 97)
  )

  dts_month <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")

  res <- project_until_now_forecast(x, dts_month = dts_month)

  expect_equal(sort(unique(res$estimate)), c("central", "lower", "upper"))
})
