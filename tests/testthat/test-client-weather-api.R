library(testthat)
library(tibble)

test_that("get_weather calls the weather API and normalizes variable names", {
  api_calls <- list()
  api_data <- tibble(
    date = as.Date(c("2023-01-01", "2023-01-01")),
    variable = c("HDD", "CDD"),
    value = c(12, 0),
    region_id = "EU",
    region_type = "region",
    region_iso2 = "EU",
    unit = "degree_days",
    averaging_period = "daily",
    source = "mock",
    region_name = "EU"
  )

  local_mocked_bindings(
    `api.get` = function(endpoint, ...) {
      api_calls[[length(api_calls) + 1]] <<- c(list(endpoint = endpoint), list(...))
      api_data
    },
    .package = "creahelpers"
  )

  result <- get_weather(
    variable = "HDD,CDD",
    region_type = "region",
    region_id = "EU",
    date_from = "2023-01-01",
    date_to = "2023-01-02",
    use_local = TRUE,
    use_cache = FALSE,
    aggregate_by = "region_iso2",
    aggregate_fn = "mean"
  )

  expect_length(api_calls, 1)
  expect_equal(api_calls[[1]]$endpoint, "http://localhost:8080/v1/weather")
  expect_equal(api_calls[[1]]$variable, "HDD,CDD")
  expect_equal(api_calls[[1]]$region_type, "region")
  expect_equal(api_calls[[1]]$region_id, "EU")
  expect_equal(api_calls[[1]]$date_from, "2023-01-01")
  expect_equal(api_calls[[1]]$date_to, "2023-01-02")
  expect_equal(api_calls[[1]]$aggregate_by, "region_iso2")
  expect_equal(api_calls[[1]]$aggregate_fn, "mean")
  expect_true(api_calls[[1]]$use_cache)
  expect_true(api_calls[[1]]$refresh_cache)
  expect_equal(result$variable, c("hdd", "cdd"))
})
