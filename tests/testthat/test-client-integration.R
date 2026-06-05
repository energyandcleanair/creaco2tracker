library(testthat)
library(dplyr)

test_that("ENTSOE power-generation client matches known German daily results", {
  skip_if_client_integration_disabled()

  workdir <- client_integration_workdir()
  on.exit(cleanup_client_integration_workdir(workdir), add = TRUE)

  expected <- read_client_integration_fixture(
    "entsoe_power_generation_de_2023-01-01_2023-01-02.csv"
  )

  actual <- suppressMessages(
    entsoe.get_power_generation(
      date_from = "2023-01-01",
      date_to = "2023-01-02",
      iso2s = "DE",
      use_cache = FALSE
    )
  ) %>%
    select(all_of(names(expected))) %>%
    arrange(.data$date, .data$source) %>%
    as.data.frame()

  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("weather client matches known French HDD/CDD country results", {
  skip_if_client_integration_disabled()

  workdir <- client_integration_workdir()
  on.exit(cleanup_client_integration_workdir(workdir), add = TRUE)

  expected <- read_client_integration_fixture(
    "weather_fr_hdd_cdd_2024-01-01_2024-01-02.csv"
  )

  actual <- suppressMessages(
    get_weather(
      variable = "HDD,CDD",
      region_iso2 = "FR",
      region_type = "country",
      date_from = "2024-01-01",
      date_to = "2024-01-02",
      use_cache = FALSE
    )
  ) %>%
    select(all_of(names(expected))) %>%
    arrange(.data$date, .data$variable) %>%
    as.data.frame()

  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("EMBER generation client matches known German monthly results", {
  skip_if_client_integration_disabled()
  skip_if_client_envvar_missing("EMBER_KEY")

  workdir <- client_integration_workdir()
  on.exit(cleanup_client_integration_workdir(workdir), add = TRUE)

  expected <- read_client_integration_fixture(
    "ember_power_generation_de_monthly_2023-01.csv"
  )

  actual <- suppressWarnings(
    suppressMessages(
      ember.get_power_generation(
        frequency = "monthly",
        iso2s = "DE",
        use_cache = FALSE
      )
    )
  ) %>%
    filter(.data$date == as.Date("2023-01-01")) %>%
    select(all_of(names(expected))) %>%
    arrange(.data$source) %>%
    as.data.frame()

  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("EMBER installed-capacity client matches known German monthly results", {
  skip_if_client_integration_disabled()
  skip_if_client_envvar_missing("EMBER_KEY")

  workdir <- client_integration_workdir()
  on.exit(cleanup_client_integration_workdir(workdir), add = TRUE)

  expected <- read_client_integration_fixture(
    "ember_installed_capacity_de_monthly_2023-01.csv"
  )

  actual <- suppressWarnings(
    suppressMessages(
      ember.get_installed_capacity(
        frequency = "monthly",
        iso2s = "DE",
        use_cache = FALSE
      )
    )
  ) %>%
    filter(.data$date == as.Date("2023-01-01")) %>%
    select(all_of(names(expected))) %>%
    arrange(.data$source) %>%
    as.data.frame()

  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("AGSI storage-change client matches known German daily results", {
  skip_if_client_integration_disabled()
  skip_if_client_envvar_missing("AGSI_API_KEY")

  workdir <- client_integration_workdir()
  on.exit(cleanup_client_integration_workdir(workdir), add = TRUE)

  expected <- read_client_integration_fixture(
    "agsi_storage_change_de_2023-01-01_2023-01-03.csv"
  )

  actual <- suppressWarnings(
    suppressMessages(
      agsi.get_storage_change(
        date_from = "2023-01-01",
        date_to = "2023-01-03",
        iso2 = "DE"
      )
    )
  ) %>%
    select(all_of(names(expected))) %>%
    arrange(.data$date) %>%
    as.data.frame()

  expect_equal(actual, expected, tolerance = 1e-6)
})
