library(testthat)

test_that("get_eurostat_from_code passes filters remotely with keepFlags disabled", {
  temp_cache <- tempfile("eurostat-client-test-")
  dir.create(temp_cache, recursive = TRUE)
  old_dir <- setwd(temp_cache)
  on.exit(
    {
      setwd(old_dir)
      unlink(temp_cache, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  withr::with_tempdir({
    local_mocked_bindings(
      get_eurostat = function(code, filters = NULL, keepFlags = FALSE, ...) {
        calls[[length(calls) + 1]] <<- list(
          code = code,
          filters = filters,
          keepFlags = keepFlags
        )

        tibble::tibble(
          nrg_bal = c("FC_E", "FC_E", "FC_E"),
          siec = c("TOTAL", "CF", "O4651XR5210B"),
          TIME_PERIOD = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
          values = c(1, 2, 3)
        )
      },
      .package = "eurostat"
    )

    out <- get_eurostat_from_code(
      code = "nrg_cb_oil",
      use_cache = FALSE,
      filters = list(siec = c("TOTAL", "CF"))
    )

    expect_equal(nrow(out), 3)
  })

  expect_length(list.files("cache", pattern = "^eurostat_nrg_cb_oil_.*\\.parquet$"), 0)
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$siec, c("TOTAL", "CF"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code keeps keepFlags disabled with no filters", {
  temp_cache <- tempfile("eurostat-client-test-")
  dir.create(temp_cache, recursive = TRUE)
  old_dir <- setwd(temp_cache)
  on.exit(
    {
      setwd(old_dir)
      unlink(temp_cache, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  withr::with_tempdir({
    local_mocked_bindings(
      get_eurostat = function(code, filters = NULL, keepFlags = FALSE, ...) {
        calls[[length(calls) + 1]] <<- list(
          code = code,
          filters = filters,
          keepFlags = keepFlags
        )

        tibble::tibble(
          siec = "TOTAL",
          TIME_PERIOD = as.Date("2024-01-01"),
          values = 1
        )
      },
      .package = "eurostat"
    )

    get_eurostat_from_code(
      code = "nrg_cb_oilm",
      use_cache = FALSE
    )
  })

  expect_length(list.files("cache", pattern = "^eurostat_nrg_cb_oilm_.*\\.parquet$"), 0)
  expect_equal(length(calls), 1)
  expect_false(calls[[1]]$keepFlags)
  expect_null(calls[[1]]$filters)
})

test_that("get_eurostat_from_code keeps server-side filtering for non-oil endpoints", {
  temp_cache <- tempfile("eurostat-client-test-")
  dir.create(temp_cache, recursive = TRUE)
  old_dir <- setwd(temp_cache)
  on.exit(
    {
      setwd(old_dir)
      unlink(temp_cache, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  withr::with_tempdir({
    local_mocked_bindings(
      get_eurostat = function(code, filters = NULL, keepFlags = FALSE, ...) {
        calls[[length(calls) + 1]] <<- list(
          code = code,
          filters = filters,
          keepFlags = keepFlags
        )

        tibble::tibble(
          siec = c("TOTAL", "CF", "O4651XR5210B"),
          nrg_bal = c("FC_E", "FC_E", "FC_E"),
          TIME_PERIOD = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
          values = c(1, 2, 3)
        )
      },
      .package = "eurostat"
    )

    out <- get_eurostat_from_code(
      code = "nrg_cb_gas",
      use_cache = FALSE,
      filters = list(nrg_bal = c("IC_OBS", "FC_NE"))
    )

    expect_equal(nrow(out), 3)
  })

  expect_length(list.files("cache", pattern = "^eurostat_nrg_cb_gas_.*\\.parquet$"), 0)
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$nrg_bal, c("IC_OBS", "FC_NE"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code adds geo filter from iso2s", {
  temp_cache <- tempfile("eurostat-client-test-")
  dir.create(temp_cache, recursive = TRUE)
  old_dir <- setwd(temp_cache)
  on.exit(
    {
      setwd(old_dir)
      unlink(temp_cache, recursive = TRUE)
    },
    add = TRUE
  )

  calls <- list()

  withr::with_tempdir({
    local_mocked_bindings(
      get_eurostat = function(code, filters = NULL, keepFlags = FALSE, ...) {
        calls[[length(calls) + 1]] <<- list(
          code = code,
          filters = filters,
          keepFlags = keepFlags
        )

        tibble::tibble(
          siec = c("TOTAL", "CF", "O4651XR5210B"),
          nrg_bal = c("FC_E", "FC_E", "FC_E"),
          TIME_PERIOD = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
          values = c(1, 2, 3)
        )
      },
      .package = "eurostat"
    )

    get_eurostat_from_code(
      code = "nrg_cb_oil",
      use_cache = FALSE,
      filters = list(siec = c("TOTAL")),
      iso2s = c("DE", "FR")
    )
  })

  expect_length(list.files("cache", pattern = "^eurostat_nrg_cb_oil_.*\\.parquet$"), 0)
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$siec, c("TOTAL"))
  expect_equal(calls[[1]]$filters$geo, c("DE", "FR"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code preserves *_code columns from raw dimensions", {
  temp_cache <- tempfile("eurostat-client-test-")
  dir.create(temp_cache, recursive = TRUE)
  old_dir <- setwd(temp_cache)
  on.exit(
    {
      setwd(old_dir)
      unlink(temp_cache, recursive = TRUE)
    },
    add = TRUE
  )

  withr::with_tempdir({
    local_mocked_bindings(
      get_eurostat = function(code, filters = NULL, keepFlags = FALSE, ...) {
        tibble::tibble(
          siec = "TOTAL",
          nrg_bal = "FC_E",
          TIME_PERIOD = as.Date("2024-01-01"),
          values = 1
        )
      },
      .package = "eurostat"
    )

    out <- get_eurostat_from_code(
      code = "nrg_cb_oilm",
      use_cache = FALSE
    )

    expect_true("siec" %in% names(out))
    expect_true("nrg_bal" %in% names(out))
    expect_equal(out$siec[[1]], "TOTAL")
    expect_equal(out$nrg_bal[[1]], "FC_E")
  })

  expect_length(list.files("cache", pattern = "^eurostat_nrg_cb_oilm_.*\\.parquet$"), 0)
})


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
