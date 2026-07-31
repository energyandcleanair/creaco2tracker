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

test_that("stabilise_eu_tail_estimates applies validated country-sum tail adjustments", {
  dates <- seq.Date(as.Date("2025-01-01"), as.Date("2025-08-01"), by = "month")
  country_rows <- bind_rows(lapply(c("DE", "FR"), function(iso2) {
    tibble(
      iso2 = iso2,
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(50, 6), 70, 80)
    )
  }))

  co2 <- bind_rows(
    tibble(
      iso2 = "EU",
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(100, 6), 130, 135)
    ),
    country_rows
  )

  result <- stabilise_eu_tail_estimates(
    co2,
    country_sum_min_countries = 2,
    country_sum_min_points = 6,
    tail_months = 2
  )

  expect_equal(
    result %>%
      filter(iso2 == "EU", date >= as.Date("2025-07-01")) %>%
      arrange(date) %>%
      pull(value),
    c(140, 160)
  )
})

test_that("country-sum submodel selects no tail adjustments when history disagrees", {
  dates <- seq.Date(as.Date("2025-01-01"), as.Date("2025-08-01"), by = "month")
  country_rows <- bind_rows(lapply(c("DE", "FR"), function(iso2) {
    tibble(
      iso2 = iso2,
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(40, 6), 70, 80)
    )
  }))

  co2 <- bind_rows(
    tibble(
      iso2 = "EU",
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(100, 6), 130, 135)
    ),
    country_rows
  )

  adjustments <- select_eu_tail_country_sum_adjustments(
    co2,
    min_countries = 2,
    min_points = 6,
    max_rel_diff = 0.05,
    tail_months = 2
  )

  expect_equal(nrow(adjustments), 0)
})

test_that("country-sum submodel selects no tail adjustments below country threshold", {
  dates <- seq.Date(as.Date("2025-01-01"), as.Date("2025-08-01"), by = "month")
  co2 <- bind_rows(
    tibble(
      iso2 = "EU",
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(100, 6), 130, 135)
    ),
    tibble(
      iso2 = "DE",
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = c(rep(100, 6), 140, 160)
    )
  )

  adjustments <- select_eu_tail_country_sum_adjustments(
    co2,
    min_countries = 2,
    min_points = 6,
    tail_months = 2
  )

  expect_equal(nrow(adjustments), 0)
})

make_eu_tail_seasonal_fixture <- function(tail_values, holdout_value = 130) {
  dates <- seq.Date(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "month")
  values <- rep(100, length(dates))
  values[dates == as.Date("2023-11-01")] <- 100
  values[dates == as.Date("2023-12-01")] <- 110
  values[dates == as.Date("2024-08-01")] <- 120
  values[dates == as.Date("2024-09-01")] <- 120
  values[dates == as.Date("2024-10-01")] <- holdout_value
  values[dates == as.Date("2024-11-01")] <- tail_values[[1]]
  values[dates == as.Date("2024-12-01")] <- tail_values[[2]]

  bind_rows(lapply(c("central", "lower", "upper"), function(estimate) {
    tibble(
      iso2 = "EU",
      date = dates,
      fuel = FUEL_OIL,
      sector = SECTOR_TRANSPORT_DOMESTIC,
      estimate = estimate,
      unit = "t",
      value = values
    )
  }))
}

expand_eu_tail_seasonal_fixture_daily <- function(co2) {
  bind_rows(lapply(seq_len(nrow(co2)), function(i) {
    row <- co2[i, ]
    dates <- seq.Date(
      row$date,
      lubridate::ceiling_date(row$date, "month") - lubridate::days(1),
      by = "day"
    )
    weights <- seq_along(dates)

    tibble(
      iso2 = row$iso2,
      date = dates,
      fuel = row$fuel,
      sector = row$sector,
      estimate = row$estimate,
      unit = row$unit,
      value = row$value * weights / sum(weights)
    )
  }))
}

test_that("stabilise_eu_tail_estimates applies selected seasonal-YoY tail adjustments", {
  co2 <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95))

  result <- stabilise_eu_tail_estimates(
    co2,
    tail_months = 2,
    seasonal_min_history_points = 12
  )

  expect_equal(
    result %>%
      filter(
        iso2 == "EU",
        fuel == FUEL_OIL,
        sector == SECTOR_TRANSPORT_DOMESTIC,
        estimate == "central",
        date %in% as.Date(c("2024-11-01", "2024-12-01"))
      ) %>%
      arrange(date) %>%
      pull(value),
    c(120, 132)
  )
})

test_that("daily seasonal-YoY adjustments scale the current shape without a first-day spike", {
  co2 <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95)) %>%
    expand_eu_tail_seasonal_fixture_daily()

  result <- stabilise_eu_tail_estimates(
    co2,
    tail_months = 2,
    seasonal_min_history_points = 12
  )

  before <- co2 %>%
    filter(
      estimate == "central",
      lubridate::floor_date(date, "month") == as.Date("2024-11-01")
    ) %>%
    arrange(date)
  after <- result %>%
    filter(
      estimate == "central",
      fuel == FUEL_OIL,
      lubridate::floor_date(date, "month") == as.Date("2024-11-01")
    ) %>%
    arrange(date)

  expect_equal(after$value / before$value, rep(after$value[[1]] / before$value[[1]], 30))
  expect_equal(sum(after$value), 120)
  expect_lt(after$value[[1]], sum(after$value))

  adjusted_sums <- result %>%
    filter(
      fuel == FUEL_OIL,
      date >= as.Date("2024-11-01")
    ) %>%
    mutate(month = lubridate::floor_date(date, "month")) %>%
    group_by(month, estimate) %>%
    summarise(value = sum(value), .groups = "drop")

  expect_equal(sort(unique(adjusted_sums$estimate)), c("central", "lower", "upper"))
  expect_equal(adjusted_sums$value, rep(c(120, 132), each = 3))

  daily_totals <- result %>%
    filter(estimate == "central", date >= as.Date("2024-11-01")) %>%
    select(date, fuel, value) %>%
    tidyr::pivot_wider(names_from = fuel, values_from = value)
  expect_equal(daily_totals$total, daily_totals$oil)
})

test_that("daily seasonal-YoY adjustments match partial-month coverage", {
  co2 <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95)) %>%
    expand_eu_tail_seasonal_fixture_daily() %>%
    filter(
      lubridate::floor_date(date, "month") != as.Date("2024-12-01") |
        lubridate::day(date) <= 10
    )

  adjustments <- select_eu_tail_seasonal_yoy_adjustments(
    co2,
    tail_months = 2,
    min_history_points = 12
  )

  expected_december <- co2 %>%
    filter(
      estimate == "central",
      lubridate::floor_date(date, "month") == as.Date("2023-12-01"),
      lubridate::day(date) <= 10
    ) %>%
    summarise(value = sum(value) * 1.2) %>%
    pull(value)
  actual_december <- adjustments %>%
    filter(
      estimate == "central",
      lubridate::floor_date(date, "month") == as.Date("2024-12-01")
    ) %>%
    summarise(value = sum(value)) %>%
    pull(value)

  expect_equal(nrow(adjustments %>% filter(date >= as.Date("2024-12-01"))), 30)
  expect_equal(actual_december, expected_december)
})

test_that("daily seasonal-YoY adjustments skip incomplete and zero profiles", {
  co2 <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95)) %>%
    expand_eu_tail_seasonal_fixture_daily() %>%
    filter(date != as.Date("2023-11-15"))

  missing_comparable <- select_eu_tail_seasonal_yoy_adjustments(
    co2,
    tail_months = 2,
    min_history_points = 12
  )

  expect_false(any(
    lubridate::floor_date(missing_comparable$date, "month") == as.Date("2024-11-01")
  ))
  expect_true(any(
    lubridate::floor_date(missing_comparable$date, "month") == as.Date("2024-12-01")
  ))

  zero_profile <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95)) %>%
    expand_eu_tail_seasonal_fixture_daily() %>%
    mutate(
      value = if_else(
        lubridate::floor_date(date, "month") == as.Date("2024-11-01"),
        0,
        value
      )
    )
  zero_adjustments <- select_eu_tail_seasonal_yoy_adjustments(
    zero_profile,
    tail_months = 2,
    min_history_points = 12
  )

  expect_false(any(
    lubridate::floor_date(zero_adjustments$date, "month") == as.Date("2024-11-01")
  ))
  expect_true(any(
    lubridate::floor_date(zero_adjustments$date, "month") == as.Date("2024-12-01")
  ))
})

test_that("daily seasonal-YoY adjustments skip leap days without a comparable date", {
  dates <- seq.Date(as.Date("2022-03-01"), as.Date("2024-02-01"), by = "month")
  values <- rep(100, length(dates))
  values[dates %in% as.Date(c("2023-10-01", "2023-11-01"))] <- 120
  values[dates == as.Date("2023-12-01")] <- 130
  values[dates %in% as.Date(c("2024-01-01", "2024-02-01"))] <- 95
  co2 <- bind_rows(lapply(c("central", "lower", "upper"), function(estimate) {
    tibble(
      iso2 = "EU",
      date = dates,
      fuel = FUEL_OIL,
      sector = SECTOR_TRANSPORT_DOMESTIC,
      estimate,
      unit = "t",
      value = values
    )
  })) %>%
    expand_eu_tail_seasonal_fixture_daily()

  adjustments <- select_eu_tail_seasonal_yoy_adjustments(
    co2,
    tail_months = 2,
    min_history_points = 12
  )

  expect_true(any(
    lubridate::floor_date(adjustments$date, "month") == as.Date("2024-01-01")
  ))
  expect_false(any(
    lubridate::floor_date(adjustments$date, "month") == as.Date("2024-02-01")
  ))
})

test_that("seasonal-YoY submodel selects no tail adjustments when backtest is worse", {
  co2 <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95), holdout_value = 100)

  adjustments <- select_eu_tail_seasonal_yoy_adjustments(
    co2,
    tail_months = 2,
    min_history_points = 12
  )

  expect_equal(nrow(adjustments), 0)
})

test_that("stabilise_eu_tail_estimates keeps selected total adjustments after recomputing totals", {
  component_rows <- make_eu_tail_seasonal_fixture(tail_values = c(95, 95))
  dates <- seq.Date(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "month")

  eu_total_rows <- tibble(
    iso2 = "EU",
    date = dates,
    fuel = FUEL_TOTAL,
    sector = SECTOR_ALL,
    estimate = "central",
    unit = "t",
    value = if_else(dates >= as.Date("2024-11-01"), 95, 100)
  )
  country_total_rows <- bind_rows(lapply(c("DE", "FR"), function(iso2) {
    tibble(
      iso2 = iso2,
      date = dates,
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      value = if_else(
        dates == as.Date("2024-11-01"),
        70,
        if_else(dates == as.Date("2024-12-01"), 80, 50)
      )
    )
  }))

  result <- stabilise_eu_tail_estimates(
    bind_rows(component_rows, eu_total_rows, country_total_rows),
    country_sum_min_countries = 2,
    country_sum_min_points = 6,
    tail_months = 2,
    seasonal_min_history_points = 12
  )

  expect_equal(
    result %>%
      filter(
        iso2 == "EU",
        fuel == FUEL_TOTAL,
        sector == SECTOR_ALL,
        estimate == "central",
        date %in% as.Date(c("2024-11-01", "2024-12-01"))
      ) %>%
      arrange(date) %>%
      pull(value),
    c(140, 160)
  )
})
