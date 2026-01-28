library(testthat)
library(dplyr)
library(tibble)
library(creahelpers)


make_co2 <- function(dates, values){
  tibble(
    iso2 = "EU",
    fuel = "gas",
    sector = "all",
    unit = "t",
    date = as.Date(dates),
    value = values
  )
}

make_proxy <- function(dates, values){
  tibble(
    iso2 = "EU",
    fuel = "gas",
    sector = "all",
    date = as.Date(dates),
    value_proxy = values
  )
}

test_that("project_until_now_zeros fills future values with zero when proxies stay at zero", {
  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01"),
    values = c(5, 0)
  )

  proxy <- make_proxy(
    dates = c("2020-02-01", "2020-03-01", "2020-04-01"),
    values = c(0, 0, 0)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-04-01"), by = "month")
  result <- project_until_now_zeros(x, proxy, dts_month)

  future_values <- result %>%
    filter(date > as.Date("2020-02-01")) %>%
    pull(value)

  expect_true(all(future_values == 0))
  expect_equal(result$value[result$date == as.Date("2020-01-01")], 5)
})

test_that("project_until_now_zeros does not fill when future proxy values are non-zero", {
  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01"),
    values = c(5, 0)
  )

  proxy <- make_proxy(
    dates = c("2020-02-01", "2020-03-01"),
    values = c(0, 1)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-03-01"), by = "month")
  result <- project_until_now_zeros(x, proxy, dts_month)

  expect_true(is.na(result$value[result$date == as.Date("2020-03-01")]))
})

test_that("project_until_now_zeros does not fill when latest value is non-zero", {
  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01"),
    values = c(5, 2)
  )

  proxy <- make_proxy(
    dates = c("2020-02-01", "2020-03-01"),
    values = c(0, 0)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-03-01"), by = "month")
  result <- project_until_now_zeros(x, proxy, dts_month)

  expect_true(is.na(result$value[result$date == as.Date("2020-03-01")]))
})
