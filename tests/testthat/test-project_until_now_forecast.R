library(testthat)
library(dplyr)
library(tibble)

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
