library(testthat)

test_that("sum_or_na keeps all-missing aggregates missing", {
  expect_true(is.na(sum_or_na(c(NA_real_, NA_real_))))
  expect_equal(sum_or_na(c(1, NA_real_, 2)), 3)
})

test_that("collect_oil preserves unavailable Eurostat oil rows", {
  monthly_stub <- tibble::tibble(
    siec = c(SIEC_OIL_PRODUCTS, SIEC_OIL_PRODUCTS),
    nrg_bal = "GID_OBS",
    time = as.Date(c("2026-02-01", "2026-03-01")),
    values = c(10, NA_real_),
    unit = EUROSTAT_UNIT_THOUSAND_TONNES
  )
  yearly_stub <- tibble::tibble(
    siec = SIEC_OIL_PRODUCTS,
    nrg_bal = "GID_OBS",
    time = as.Date("2025-01-01"),
    values = 120,
    unit = EUROSTAT_UNIT_THOUSAND_TONNES
  )

  local_mocked_bindings(
    get_eurostat_from_code = function(code, use_cache = FALSE, filters = NULL, ...) {
      if (identical(code, "nrg_cb_oil")) {
        return(yearly_stub)
      }
      if (identical(code, "nrg_cb_oilm")) {
        return(monthly_stub)
      }

      stop("unexpected code")
    },
    fill_oil_non_energy_use_yearly = function(x) x,
    fill_gid_obs_with_gid_cal = function(x) x,
    fill_oil_non_energy_use_monthly = function(yearly, monthly) monthly,
    add_oil_transport = function(monthly, yearly) monthly,
    add_iso2 = function(x) dplyr::mutate(x, iso2 = "EU"),
    .package = "creaco2tracker"
  )

  result <- collect_oil(use_cache = FALSE)

  expect_equal(result$monthly$time, as.Date(c("2026-02-01", "2026-03-01")))
  expect_equal(result$monthly$values, c(10, NA_real_))
})

test_that("process_oil keeps available all-sector components for incomplete latest month", {
  oil_input <- tibble::tribble(
    ~geo, ~siec, ~nrg_bal, ~time, ~values, ~unit,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_OBS", "2026-02-01", 1000, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_NE", "2026-02-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_OBS", "2026-02-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_NE", "2026-02-01", 10, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_OBS", "2026-02-01", 50, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_NE", "2026-02-01", 5, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIOGASOLINE, "GID_OBS", "2026-02-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIODIESEL, "GID_OBS", "2026-02-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_NE", "2026-03-01", 6, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_NE", "2026-03-01", 1, EUROSTAT_UNIT_THOUSAND_TONNES
  ) %>%
    dplyr::mutate(time = as.Date(time))

  local_mocked_bindings(
    add_iso2 = function(x, ...) dplyr::mutate(x, iso2 = "EU"),
    fill_gaps_in_time_series = function(data, ...) data,
    fill_eu_from_countries_sum = function(data, ...) data,
    fix_eu_when_important_countries_missing = function(data, ...) data,
    check_eurostat_oil_completeness = function(x) TRUE,
    .package = "creaco2tracker"
  )

  result <- process_oil(oil_input)

  expect_gt(
    nrow(result %>% dplyr::filter(time == as.Date("2026-02-01"), sector == SECTOR_ALL)),
    0
  )
  expect_gt(
    nrow(result %>% dplyr::filter(time == as.Date("2026-03-01"), sector == SECTOR_ALL)),
    0
  )
})

test_that("process_oil does not materialize structurally missing months before gap filling", {
  oil_input <- tibble::tribble(
    ~geo, ~siec, ~nrg_bal, ~time, ~values, ~unit,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_OBS", "2026-01-01", 1000, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_NE", "2026-01-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_OBS", "2026-01-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_NE", "2026-01-01", 10, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_OBS", "2026-01-01", 50, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_NE", "2026-01-01", 5, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIOGASOLINE, "GID_OBS", "2026-01-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIODIESEL, "GID_OBS", "2026-01-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_OBS", "2026-03-01", 1000, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_OIL_PRODUCTS, "GID_NE", "2026-03-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_OBS", "2026-03-01", 100, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_HEATING_GASOIL, "GID_NE", "2026-03-01", 10, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_OBS", "2026-03-01", 50, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_FUEL_OIL, "GID_NE", "2026-03-01", 5, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIOGASOLINE, "GID_OBS", "2026-03-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES,
    "EU27_2020", SIEC_BIODIESEL, "GID_OBS", "2026-03-01", 0, EUROSTAT_UNIT_THOUSAND_TONNES
  ) %>%
    dplyr::mutate(time = as.Date(time))

  gap_fill_inputs <- list()

  local_mocked_bindings(
    add_iso2 = function(x, ...) dplyr::mutate(x, iso2 = "EU"),
    fill_gaps_in_time_series = function(data, ...) {
      gap_fill_inputs[[length(gap_fill_inputs) + 1]] <<- data
      data
    },
    fill_eu_from_countries_sum = function(data, ...) data,
    fix_eu_when_important_countries_missing = function(data, ...) data,
    check_eurostat_oil_completeness = function(x) TRUE,
    .package = "creaco2tracker"
  )

  process_oil(oil_input)

  expect_length(gap_fill_inputs, 2)
  expect_false(any(gap_fill_inputs[[1]]$time == as.Date("2026-02-01")))
  expect_false(any(gap_fill_inputs[[2]]$time == as.Date("2026-02-01")))
})

test_that("validate_co2 rejects materially negative central component rows", {
  co2 <- tibble::tibble(
    iso2 = "EU",
    date = as.Date("2026-03-01"),
    unit = "t",
    estimate = "central",
    value = -1,
    fuel = FUEL_OIL,
    sector = SECTOR_OTHERS
  )

  expect_error(
    validate_co2(co2, diagnostics_folder = NULL),
    "Negative central CO2 component values"
  )
})

test_that("validate_co2 allows materially negative non-EU central component rows with warning", {
  co2 <- tibble::tibble(
    iso2 = "NL",
    date = as.Date("2026-03-01"),
    unit = "t",
    estimate = "central",
    value = -1,
    fuel = FUEL_OIL,
    sector = SECTOR_OTHERS
  )

  expect_warning(
    validate_co2(co2, diagnostics_folder = NULL),
    "Negative central CO2 component values"
  )
})
