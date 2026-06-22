library(testthat)

local_temp_cache_dir <- function() {
  temp_dir <- tempfile("iea-client-test-")
  dir.create(temp_dir, recursive = TRUE)
  old_dir <- setwd(temp_dir)
  list(temp_dir = temp_dir, old_dir = old_dir)
}

test_that("iea balance uses parquet cache and tempdir-backed misses", {
  temp_cache <- local_temp_cache_dir()
  on.exit(
    {
      setwd(temp_cache$old_dir)
      unlink(temp_cache$temp_dir, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  local_mocked_bindings(
    api.get = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      tibble::tibble(year = 2023, value = 1)
    },
    .package = "creahelpers"
  )

  result <- iea.get_balance(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = FALSE)
  expect_length(list.files("cache", pattern = "^ieabalance_.*\\.parquet$"), 0)

  cached_result <- iea.get_balance(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = TRUE)
  cached_again <- iea.get_balance(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = TRUE)

  expect_length(calls, 2)
  expect_equal(result, tibble::tibble(year = 2023, value = 1))
  expect_equal(cached_result, result)
  expect_equal(cached_again, result)
  expect_length(list.files("cache", pattern = "^ieabalance_.*\\.parquet$"), 1)
})

test_that("iea conversion factors use parquet cache", {
  temp_cache <- local_temp_cache_dir()
  on.exit(
    {
      setwd(temp_cache$old_dir)
      unlink(temp_cache$temp_dir, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  local_mocked_bindings(
    api.get = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      tibble::tibble(factor = 2)
    },
    .package = "creahelpers"
  )

  result <- iea.get_conversion_factors(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = FALSE)
  cached_result <- iea.get_conversion_factors(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = TRUE)
  cached_again <- iea.get_conversion_factors(year_from = 2023, year_to = 2023, iso2 = "DE", use_cache = TRUE)

  expect_length(calls, 2)
  expect_equal(result, tibble::tibble(factor = 2))
  expect_equal(cached_result, result)
  expect_equal(cached_again, result)
  expect_length(list.files("cache", pattern = "^ieaconversion_.*\\.parquet$"), 1)
})