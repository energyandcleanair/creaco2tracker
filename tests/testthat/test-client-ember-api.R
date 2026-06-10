library(testthat)

local_temp_cache_dir <- function() {
  temp_dir <- tempfile("ember-client-test-")
  dir.create(temp_dir, recursive = TRUE)
  old_dir <- setwd(temp_dir)
  file.create(".Renviron")
  list(temp_dir = temp_dir, old_dir = old_dir)
}

local_ember_key <- function(value) {
  old_key <- Sys.getenv("EMBER_KEY", unset = NA_character_)
  Sys.setenv(EMBER_KEY = value)
  old_key
}

restore_ember_key <- function(old_key) {
  if (is.na(old_key)) {
    Sys.unsetenv("EMBER_KEY")
  } else {
    Sys.setenv(EMBER_KEY = old_key)
  }
}

test_that("ember.get_power_generation parses generation data and uses cache", {
  temp_cache <- local_temp_cache_dir()
  on.exit(
    {
      setwd(temp_cache$old_dir)
      unlink(temp_cache$temp_dir, recursive = TRUE)
    },
    add = TRUE
  )
  old_key <- local_ember_key("test-key")
  on.exit(restore_ember_key(old_key), add = TRUE)

  urls <- character()

  local_mocked_bindings(
    GET = function(url, ...) {
      urls <<- c(urls, url)
      list(url = url)
    },
    content = function(response, type = "text", encoding = "UTF-8", ...) {
      paste0(
        '{"data":[',
        '{"date":"2023","series":"Gas","generation_twh":1.25}',
        ']}'
      )
    },
    .package = "httr"
  )

  result <- ember.get_power_generation(
    frequency = "yearly",
    iso2s = "DE",
    use_cache = FALSE
  )
  expect_length(list.files("cache", pattern = "^ember_power_yearly_.*\\.parquet$"), 0)

  cached_result <- ember.get_power_generation(
    frequency = "yearly",
    iso2s = "DE",
    use_cache = TRUE
  )
  cached_again <- ember.get_power_generation(
    frequency = "yearly",
    iso2s = "DE",
    use_cache = TRUE
  )

  expect_length(urls, 2)
  expect_match(urls[1], "/v1/electricity-generation/yearly/")
  expect_match(urls[1], "entity_code=DEU")
  expect_match(urls[1], "api_key=test-key")
  expect_equal(result$iso2, "DE")
  expect_equal(result$source, "Gas")
  expect_equal(result$value_mwh, 1.25e6)
  expect_equal(result$date, as.Date("2023-01-01"))
  expect_equal(cached_result, result)
  expect_equal(cached_again, result)
  expect_length(list.files("cache", pattern = "^ember_power_yearly_.*\\.parquet$"), 1)
})

test_that("ember.get_installed_capacity parses capacity data", {
  temp_cache <- local_temp_cache_dir()
  on.exit(
    {
      setwd(temp_cache$old_dir)
      unlink(temp_cache$temp_dir, recursive = TRUE)
    },
    add = TRUE
  )
  old_key <- local_ember_key("test-key")
  on.exit(restore_ember_key(old_key), add = TRUE)

  urls <- character()

  local_mocked_bindings(
    GET = function(url, ...) {
      urls <<- c(urls, url)
      list(url = url)
    },
    content = function(response, type = "text", encoding = "UTF-8", ...) {
      '{"data":[{"date":"2023-01-01","series":"Solar","capacity_gw":2.5}]}'
    },
    .package = "httr"
  )

  result <- ember.get_installed_capacity(
    frequency = "monthly",
    iso2s = "EU",
    use_cache = FALSE
  )
  expect_length(list.files("cache", pattern = "^ember_capacity_monthly_.*\\.parquet$"), 0)

  cached_result <- ember.get_installed_capacity(
    frequency = "monthly",
    iso2s = "EU",
    use_cache = TRUE
  )
  cached_again <- ember.get_installed_capacity(
    frequency = "monthly",
    iso2s = "EU",
    use_cache = TRUE
  )

  expect_length(urls, 2)
  expect_match(urls[1], "/v1/installed-capacity/monthly/")
  expect_match(urls[1], "entity=EU")
  expect_match(urls[1], "api_key=test-key")
  expect_equal(result$iso2, "EU")
  expect_equal(result$source, "Solar")
  expect_equal(result$value_mw, 2500)
  expect_equal(result$date, as.Date("2023-01-01"))
  expect_equal(cached_result, result)
  expect_equal(cached_again, result)
  expect_length(list.files("cache", pattern = "^ember_capacity_monthly_.*\\.parquet$"), 1)
})
