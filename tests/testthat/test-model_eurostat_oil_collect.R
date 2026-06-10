library(testthat)

test_that("collect_oil queries yearly oil data in one batched request", {
  yearly_calls <- list()

  monthly_stub <- tibble::tibble(
    siec = "TOTAL",
    nrg_bal = "GID_OBS",
    time = as.Date("2024-01-01"),
    values = 1,
    unit = "TJ"
  )
  yearly_stub <- tibble::tibble(
    siec = "TOTAL",
    nrg_bal = "GID_OBS",
    time = as.Date("2023-01-01"),
    values = 1,
    unit = "TJ"
  )

  local_mocked_bindings(
    get_eurostat_from_code = function(code, use_cache = FALSE, filters = NULL, ...) {
      if (identical(code, "nrg_cb_oil")) {
        yearly_calls[[length(yearly_calls) + 1]] <<- filters$siec
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
    add_iso2 = function(x) dplyr::mutate(x, iso2 = "DE"),
    .package = "creaco2tracker"
  )

  out <- collect_oil(use_cache = FALSE)

  expect_equal(length(yearly_calls), 1)
  expect_equal(length(yearly_calls[[1]]), 11)
  expect_setequal(
    yearly_calls[[1]],
    c(
      SIEC_OIL_PRODUCTS,
      SIEC_CRUDE_OIL,
      SIEC_ROAD_DIESEL,
      SIEC_MOTOR_GASOLINE_XBIO,
      SIEC_HEATING_GASOIL,
      SIEC_FUEL_OIL,
      SIEC_GASOIL_DIESEL,
      SIEC_KEROSENE_XBIO,
      SIEC_AVIATION_GASOLINE,
      SIEC_BIOGASOLINE,
      SIEC_BIODIESEL
    )
  )
  expect_named(out, c("monthly", "yearly"))
})
