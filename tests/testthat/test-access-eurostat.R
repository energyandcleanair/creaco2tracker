library(testthat)

test_that("get_eurostat_from_code passes filters remotely with keepFlags disabled", {
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

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$siec, c("TOTAL", "CF"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code keeps keepFlags disabled with no filters", {
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

  expect_equal(length(calls), 1)
  expect_false(calls[[1]]$keepFlags)
  expect_null(calls[[1]]$filters)
})

test_that("get_eurostat_from_code keeps server-side filtering for non-oil endpoints", {
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

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$nrg_bal, c("IC_OBS", "FC_NE"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code adds geo filter from iso2s", {
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

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$filters$siec, c("TOTAL"))
  expect_equal(calls[[1]]$filters$geo, c("DE", "FR"))
  expect_false(calls[[1]]$keepFlags)
})

test_that("get_eurostat_from_code preserves *_code columns from raw dimensions", {
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
})
