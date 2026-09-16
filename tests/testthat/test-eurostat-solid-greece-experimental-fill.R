library(testthat)
library(dplyr)

test_that("coal annual imputation backfills a fully missing Greek lignite year", {
  cons_monthly <- tibble::tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec, ~fuel, ~values,
    "EU", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 100,
    "DE", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 60,
    "PL", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 40
  )

  cons_yearly_monthly <- tibble::tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec, ~fuel, ~values,
    "GR", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 20,
    "GR", SECTOR_ALL, as.Date("2024-12-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 15
  )

  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) c("DE", "PL", "GR"),
    .package = "creaco2tracker"
  )

  out <- apply_coal_annual_imputation(cons_monthly, cons_yearly_monthly, tibble())

  gr_val <- out %>%
    filter(
      iso2 == "GR",
      sector == SECTOR_ALL,
      time == as.Date("2025-01-01"),
      siec == SIEC_BROWN_COAL
    ) %>%
    pull(values)
  expect_equal(gr_val, 20)

  eu_val <- out %>%
    filter(
      iso2 == "EU",
      sector == SECTOR_ALL,
      time == as.Date("2025-01-01"),
      siec == SIEC_BROWN_COAL
    ) %>%
    pull(values)
  expect_equal(eu_val, 120)
})


test_that("coal annual imputation keeps a partially reported series unchanged", {
  cons_monthly <- tibble::tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec, ~fuel, ~values,
    "GR", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 25,
    "EU", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 125,
    "DE", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 60,
    "PL", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 40
  )

  cons_yearly_monthly <- tibble::tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec, ~fuel, ~values,
    "GR", SECTOR_ALL, as.Date("2025-01-01"), "TJ", SIEC_BROWN_COAL, FUEL_COAL, 20
  )

  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) c("DE", "PL", "GR"),
    .package = "creaco2tracker"
  )

  out <- apply_coal_annual_imputation(cons_monthly, cons_yearly_monthly, tibble())

  gr_val <- out %>%
    filter(iso2 == "GR", time == as.Date("2025-01-01"), siec == SIEC_BROWN_COAL) %>%
    pull(values)
  expect_equal(gr_val, 25)
})


test_that("coal annual imputation follows a complete Ember-calibrated coal profile", {
  months <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
  cons_monthly <- tibble::tibble(
    iso2 = "EU", sector = SECTOR_ALL, time = months, unit = "TJ",
    siec = SIEC_BROWN_COAL, fuel = FUEL_COAL, values = 100
  )
  cons_yearly_monthly <- tibble::tibble(
    iso2 = "GR", sector = SECTOR_ALL, time = months, unit = "TJ",
    siec = SIEC_BROWN_COAL, fuel = FUEL_COAL, values = 10
  )
  pwr_generation <- tibble::tibble(
    iso2 = "GR", source = "Coal", date = months,
    value_mwh = c(2, rep(1, 11))
  )

  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) c("GR"),
    .package = "creaco2tracker"
  )

  out <- apply_coal_annual_imputation(
    cons_monthly, cons_yearly_monthly, pwr_generation
  )

  gr <- out %>% filter(iso2 == "GR") %>% arrange(time)
  expect_equal(sum(gr$values), 120)
  expect_equal(gr$values[[1]], 120 * 2 / 13)
  expect_equal(gr$values[[2]], 120 / 13)
})


test_that("coal annual imputation applies to Sweden as well as Greece", {
  months <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
  cons_yearly_monthly <- tibble::tibble(
    iso2 = "SE", sector = SECTOR_ALL, time = months, unit = "TJ",
    siec = SIEC_HARD_COAL, fuel = FUEL_COAL, values = 10
  )
  pwr_generation <- tibble::tibble(
    iso2 = "SE", source = "Coal", date = months, value_mwh = 1
  )

  out <- apply_coal_annual_imputation(tibble(), cons_yearly_monthly, pwr_generation)

  sweden <- out %>% filter(iso2 == "SE")
  expect_equal(nrow(sweden), 12)
  expect_equal(sum(sweden$values), 120)
})


test_that("coal annual bounds check flags a material annual mismatch", {
  months <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
  monthly <- tibble::tibble(
    iso2 = "SE", sector = SECTOR_ALL, time = months, unit = "TJ",
    siec = SIEC_HARD_COAL, fuel = FUEL_COAL, values = 11, source = "monthly"
  )
  annual <- tibble::tibble(
    iso2 = "SE", sector = SECTOR_ALL, time = as.Date("2025-01-01"), unit = "TJ",
    siec = SIEC_HARD_COAL, fuel = FUEL_COAL, values = 120
  )

  result <- check_coal_annual_bounds(monthly, annual, diagnostics_folder = NULL)

  expect_false(result$within_bounds[[1]])
  expect_equal(result$difference[[1]], 12)
})


test_that("process_solid_yearly keeps NA when all electricity components are missing", {
  yearly_raw <- tibble::tribble(
    ~iso2, ~time, ~siec, ~nrg_bal, ~unit, ~values,
    "GR", as.Date("2025-01-01"), SIEC_BROWN_COAL, "TI_EHG_MAPE_E", "TJ", NA_real_,
    "GR", as.Date("2025-01-01"), SIEC_BROWN_COAL, "TI_EHG_MAPCHP_E", "TJ", NA_real_
  )

  out <- process_solid_yearly(yearly_raw)

  expect_equal(nrow(out), 1)
  expect_true(is.na(out$values[[1]]))
  expect_equal(out$sector[[1]], SECTOR_ELEC)
})
