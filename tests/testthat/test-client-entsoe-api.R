library(testthat)
library(dplyr)
library(tibble)

test_that("entsoe.get_power_generation calls the API and recalculates totals", {
  api_calls <- list()
  api_data <- tibble(
    iso2 = "DE",
    region = "Europe",
    country = "Germany",
    date = rep(as.Date(c("2023-01-01", "2023-01-02")), each = 3),
    source = rep(c("Fossil Gas", "Solar", "Total"), 2),
    data_source = "entsoe",
    value_mw = c(100, 50, 999, 110, 55, 999),
    value_mwh = c(2400, 1200, 9999, 2640, 1320, 9999),
    frequency = "daily"
  )

  local_mocked_bindings(
    `api.get` = function(endpoint, ...) {
      api_calls[[length(api_calls) + 1]] <<- c(list(endpoint = endpoint), list(...))
      api_data
    },
    .package = "creahelpers"
  )

  result <- entsoe.get_power_generation(
    date_from = "2023-01-01",
    date_to = "2023-01-02",
    iso2s = "DE",
    use_cache = FALSE,
    use_local = TRUE
  )

  expect_length(api_calls, 1)
  expect_equal(api_calls[[1]]$endpoint, "http://localhost:8080/energy/power_generation")
  expect_equal(api_calls[[1]]$date_from, "2023-01-01")
  expect_equal(api_calls[[1]]$date_to, "2023-01-02")
  expect_equal(api_calls[[1]]$country, "DE")
  expect_equal(api_calls[[1]]$aggregate_by, "country,source,date")
  expect_equal(api_calls[[1]]$data_source, "entsoe")
  expect_true(api_calls[[1]]$use_cache)
  expect_true(api_calls[[1]]$refresh_cache)

  totals <- result %>%
    filter(.data$source == "Total") %>%
    arrange(.data$date)

  expect_equal(totals$value_mw, c(150, 165))
  expect_equal(totals$value_mwh, c(3600, 3960))
  expect_false(any(totals$value_mw == 999))
})

test_that("entsoe.get_installed_capacity calls the installed-capacity API", {
  api_calls <- list()
  api_data <- tibble(
    iso2 = "DE",
    region = "Europe",
    country = "Germany",
    date = as.Date("2023-01-01"),
    source = "Solar",
    data_source = "entsoe",
    value_mw = 1000
  )

  local_mocked_bindings(
    `api.get` = function(endpoint, ...) {
      api_calls[[length(api_calls) + 1]] <<- c(list(endpoint = endpoint), list(...))
      api_data
    },
    .package = "creahelpers"
  )

  result <- entsoe.get_installed_capacity(
    date_from = "2023-01-01",
    date_to = "2023-01-31",
    iso2s = "DE",
    use_cache = FALSE,
    use_local = TRUE
  )

  expect_length(api_calls, 1)
  expect_equal(api_calls[[1]]$endpoint, "http://localhost:8080/power/installed_capacity")
  expect_equal(api_calls[[1]]$date_from, "2023-01-01")
  expect_equal(api_calls[[1]]$date_to, "2023-01-31")
  expect_equal(api_calls[[1]]$country, "DE")
  expect_true(api_calls[[1]]$refresh_cache)
  expect_equal(result$value_mw, 1000)
  expect_equal(result$source, "Solar")
})
