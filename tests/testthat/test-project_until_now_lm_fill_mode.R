library(testthat)
library(dplyr)
library(tibble)
library(creahelpers)

devtools::load_all(".")

make_co2 <- function(dates, values){
  tibble(
    iso2 = "BE",
    fuel = "gas",
    sector = "all",
    unit = "t",
    date = as.Date(dates),
    value = values
  )
}

make_proxy <- function(dates, values){
  tibble(
    iso2 = "BE",
    fuel = "gas",
    sector = "all",
    date = as.Date(dates),
    value_proxy = values
  )
}

test_that("project_until_now_lm supports overwrite and missing fill modes", {

  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01", "2020-04-01"),
    values = c(10, 20, 40)
  )

  proxy <- make_proxy(
    dates = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01"),
    values = c(1, 2, 3, 4, 5)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-05-01"), by = "month")

  res_overwrite <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "overwrite"
  )

  res_missing <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "missing"
  )

  expected <- tibble(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01")),
    value = c(10, 20, 30, 40, 50)
  )

  normalize <- function(df){
    df %>% arrange(date) %>% mutate(value = unname(value)) %>% select(date, value)
  }

  expect_equal(normalize(res_overwrite), expected)
  expect_equal(normalize(res_missing), expected)
})

test_that("project_until_now_lm ratio fill propagates trends across gaps", {
  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01", "2020-04-01"),
    values = c(10, 20, 40)
  )

  proxy <- make_proxy(
    dates = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01"),
    values = c(1, 2, 3, 4, 5)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-05-01"), by = "month")

  res_ratio <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "ratio"
  )

  expected <- tibble(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01")),
    value = c(10, 20, 30, 40, 50)
  )

  expect_equal(res_ratio %>% arrange(date) %>% mutate(value = unname(value)) %>% select(date, value), expected)
})

test_that("project_until_now_lm fill modes diverge when observed values differ from model predictions", {

  x <- make_co2(
    dates = c("2020-01-01", "2020-02-01", "2020-04-01"),
    values = c(100, 200, 250)
  )

  proxy <- make_proxy(
    dates = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01"),
    values = c(1, 2, 3, 4, 5, 6)
  )

  dts_month <- seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month")

  res_overwrite <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "overwrite"
  ) %>% arrange(date)

  res_missing <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "missing"
  ) %>% arrange(date)

  res_ratio <- project_until_now_lm(
    x, proxy, dts_month,
    min_r2 = 0,
    fill_mode = "ratio"
  ) %>% arrange(date)

  preds <- unname(res_overwrite$value)
  dates <- unname(res_overwrite$date)

  # Existing values
  expect_true(all(res_missing$value[c(1,2,4)] == x$value[c(1,2,3)]))
  expect_true(all(res_overwrite$value[c(1,2,4)] != x$value[c(1,2,3)]))
  expect_true(all(res_ratio$value[c(1,2,4)] == x$value[c(1,2,3)]))


  # Filled gaps
  expect_true(all(res_missing$value[c(3,5,6)] == preds[c(3,5,6)]))
  expect_true(all(res_ratio$value[c(3,5,6)] != preds[c(3,5,6)]))

  expect_true(res_ratio$value[3] / res_ratio$value[2] ==
                preds[3] / preds[2])

  expect_true(res_ratio$value[5] / res_ratio$value[4] ==
                preds[5] / preds[4])

  expect_true(res_ratio$value[6] / res_ratio$value[5] ==
                preds[6] / preds[5])

})
