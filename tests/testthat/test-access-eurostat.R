library(testthat)

test_that("collect_oil applies source masking at fetch stage", {
  monthly_stub <- tibble::tibble(
    siec = c(SIEC_OIL_PRODUCTS, SIEC_OIL_PRODUCTS),
    nrg_bal = c("GID_OBS", "GID_OBS"),
    time = as.Date(c("2024-01-01", "2024-02-01")),
    values = c(1, 2),
    unit = c("TJ", "TJ")
  )
  yearly_stub <- tibble::tibble(
    siec = c(SIEC_OIL_PRODUCTS, SIEC_OIL_PRODUCTS),
    nrg_bal = c("GID_OBS", "GID_OBS"),
    time = as.Date(c("2023-01-01", "2024-01-01")),
    values = c(1, 2),
    unit = c("TJ", "TJ")
  )

  local_mocked_bindings(
    get_eurostat_from_code = function(code, use_cache = FALSE, filters = NULL, ...) {
      if (identical(code, "nrg_cb_oilm")) {
        return(monthly_stub)
      }
      if (identical(code, "nrg_cb_oil")) {
        return(yearly_stub)
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

  data_masking <- list(
    eurostat_oil_monthly = list(date_from = "2024-02-01"),
    eurostat_oil_yearly = list(date_from = "2024-01-01")
  )

  out <- collect_oil(use_cache = FALSE, data_masking = data_masking)

  expect_equal(sort(unique(as.character(out$monthly$time))), "2024-01-01")
  expect_equal(sort(unique(as.character(out$yearly$time))), "2023-01-01")
})
