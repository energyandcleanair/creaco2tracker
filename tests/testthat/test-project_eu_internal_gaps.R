library(testthat)
library(dplyr)
library(tibble)

make_eu_gap_co2 <- function(eu_values, country_values = NULL, dates = NULL) {
  if (is.null(dates)) {
    dates <- seq.Date(as.Date("2025-04-01"), by = "month", length.out = length(eu_values))
  }
  if (is.null(country_values)) {
    country_values <- list(
      DE = eu_values / 2,
      FR = eu_values / 2
    )
  }

  eu_rows <- tibble(
    iso2 = "EU",
    date = dates,
    fuel = FUEL_GAS,
    sector = SECTOR_ELEC,
    unit = "t",
    value = eu_values
  )

  country_rows <- bind_rows(lapply(names(country_values), function(country_iso2) {
    values <- country_values[[country_iso2]]
    tibble(
      iso2 = country_iso2,
      date = dates,
      fuel = FUEL_GAS,
      sector = SECTOR_ELEC,
      unit = "t",
      value = values
    )
  }))

  bind_rows(eu_rows, country_rows)
}

test_that("fill_eu_internal_gaps fills internal EU gaps from validated member-state sums", {
  dates <- seq.Date(as.Date("2025-04-01"), as.Date("2025-07-01"), by = "month")
  co2 <- make_eu_gap_co2(
    eu_values = c(30, NA_real_, 34, 36),
    country_values = list(
      DE = c(10, 11, 12, 13),
      FR = c(20, 21, 22, 23)
    ),
    dates = dates
  )

  result <- fill_eu_internal_gaps(
    co2,
    min_countries = 2,
    min_points = 2,
    max_rel_diff = 0.01
  )

  may_value <- result %>%
    filter(iso2 == "EU", date == as.Date("2025-05-01")) %>%
    pull(value)

  expect_equal(may_value, 32)
})

test_that("fill_eu_internal_gaps does not fill tail EU gaps", {
  dates <- seq.Date(as.Date("2025-04-01"), as.Date("2025-07-01"), by = "month")
  co2 <- make_eu_gap_co2(
    eu_values = c(30, 32, 34, NA_real_),
    country_values = list(
      DE = c(10, 11, 12, 13),
      FR = c(20, 21, 22, 23)
    ),
    dates = dates
  )

  result <- fill_eu_internal_gaps(
    co2,
    min_countries = 2,
    min_points = 2,
    max_rel_diff = 0.01
  )

  july_value <- result %>%
    filter(iso2 == "EU", date == as.Date("2025-07-01")) %>%
    pull(value)

  expect_true(is.na(july_value))
})

test_that("fill_eu_internal_gaps does not use country sums below coverage threshold", {
  dates <- seq.Date(as.Date("2025-04-01"), as.Date("2025-06-01"), by = "month")
  co2 <- make_eu_gap_co2(
    eu_values = c(10, NA_real_, 30),
    country_values = list(
      DE = c(5, 500, 15),
      FR = c(5, 500, 15)
    ),
    dates = dates
  )

  result <- fill_eu_internal_gaps(
    co2,
    min_countries = 3,
    min_points = 2,
    max_rel_diff = 0.01
  )

  may_value <- result %>%
    filter(iso2 == "EU", date == as.Date("2025-05-01")) %>%
    pull(value)

  expect_equal(may_value, 19.84, tolerance = 0.01)
})

test_that("fill_eu_internal_gaps interpolates only after country-sum validation fails", {
  dates <- seq.Date(as.Date("2025-04-01"), as.Date("2025-06-01"), by = "month")
  co2 <- make_eu_gap_co2(
    eu_values = c(10, NA_real_, 30),
    country_values = list(
      DE = c(50, 500, 50),
      FR = c(50, 500, 50)
    ),
    dates = dates
  )

  result <- fill_eu_internal_gaps(
    co2,
    min_countries = 2,
    min_points = 2,
    max_rel_diff = 0.01
  )

  may_value <- result %>%
    filter(iso2 == "EU", date == as.Date("2025-05-01")) %>%
    pull(value)

  expect_equal(may_value, 19.84, tolerance = 0.01)
})

test_that("fill_eu_internal_gaps fills internal gaps created by forecast expansion", {
  co2 <- tibble(
    iso2 = "EU",
    date = as.Date(c("2025-04-01", "2025-06-01")),
    fuel = FUEL_OIL,
    sector = SECTOR_ELEC,
    unit = "t",
    value = c(10, 30)
  )

  forecasted <- project_until_now_forecast(
    co2,
    dts_month = seq.Date(as.Date("2025-04-01"), as.Date("2025-06-01"), by = "month")
  )

  expect_true(
    is.na(
      forecasted %>%
        filter(
          iso2 == "EU",
          date == as.Date("2025-05-01"),
          estimate == "central"
        ) %>%
        pull(value)
    )
  )

  result <- fill_eu_internal_gaps(forecasted)

  may_value <- result %>%
    filter(
      iso2 == "EU",
      date == as.Date("2025-05-01"),
      estimate == "central"
    ) %>%
    pull(value)

  expect_equal(may_value, 19.84, tolerance = 0.01)
  expect_no_error(validate_co2_no_protected_eu_internal_gaps(result))
})

test_that("project_until_now wires EU internal-gap fill around generic forecast", {
  orchestration_source <- paste(
    readLines(testthat::test_path("..", "..", "R", "model_project_orchestration.R")),
    collapse = "\n"
  )

  fill_positions <- gregexpr("fill_eu_internal_gaps", orchestration_source)[[1]]
  forecast_position <- regexpr("project_until_now_forecast", orchestration_source)[[1]]
  detotalise_position <- regexpr("detotalise_co2", orchestration_source)[[1]]

  expect_gte(length(fill_positions), 2)
  expect_lt(
    regexpr("project_until_now_coal_others", orchestration_source)[[1]],
    fill_positions[[1]]
  )
  expect_lt(
    fill_positions[[1]],
    forecast_position
  )
  expect_lt(
    forecast_position,
    fill_positions[[2]]
  )
  expect_lt(
    fill_positions[[2]],
    detotalise_position
  )
})

test_that("validate_co2 rejects protected EU central internal gaps", {
  co2 <- tibble(
    iso2 = "EU",
    date = as.Date(c("2025-04-01", "2025-05-01", "2025-06-01")),
    fuel = FUEL_GAS,
    sector = SECTOR_ELEC,
    estimate = "central",
    unit = "t",
    value = c(10, NA_real_, 30)
  )

  expect_error(
    validate_co2(co2, diagnostics_folder = NULL),
    "Protected EU central CO2 rows still contain internal gaps"
  )
})
