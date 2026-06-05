library(testthat)

test_that("agsi.get_storage_change parses AGSI storage withdrawals", {
  old_key <- Sys.getenv("AGSI_API_KEY", unset = NA_character_)
  Sys.setenv(AGSI_API_KEY = "test-key")
  on.exit(
    {
      if (is.na(old_key)) {
        Sys.unsetenv("AGSI_API_KEY")
      } else {
        Sys.setenv(AGSI_API_KEY = old_key)
      }
    },
    add = TRUE
  )

  calls <- list()

  local_mocked_bindings(
    GET = function(url, ...) {
      calls[[length(calls) + 1]] <<- list(url = url, options = list(...))
      list(url = url)
    },
    content = function(response, type = "text", encoding = "UTF-8", ...) {
      paste0(
        '{"data":[',
        '{"code":"DE","gasDayStart":"2023-01-02","netWithdrawal":"11.3505"}',
        ']}'
      )
    },
    .package = "httr"
  )

  result <- suppressMessages(
    agsi.get_storage_change(
      date_from = "2023-01-01",
      date_to = "2023-01-03",
      iso2 = "DE"
    )
  )

  expect_length(calls, 1)
  expect_match(calls[[1]]$url, "country=DE")
  expect_match(calls[[1]]$url, "from=2023-01-01")
  expect_match(calls[[1]]$url, "to=2023-01-03")
  expect_equal(result$iso2, "DE")
  expect_equal(result$date, as.Date("2023-01-02"))
  expect_equal(result$value_gwh, 11.3505)
  expect_equal(result$value_m3, 1e6, tolerance = 1)
  expect_equal(result$type, "storage_drawdown")
})

test_that("agsi.get_storage_change returns no data when the API key is absent", {
  old_key <- Sys.getenv("AGSI_API_KEY", unset = NA_character_)
  Sys.unsetenv("AGSI_API_KEY")
  on.exit(
    {
      if (is.na(old_key)) {
        Sys.unsetenv("AGSI_API_KEY")
      } else {
        Sys.setenv(AGSI_API_KEY = old_key)
      }
    },
    add = TRUE
  )

  local_mocked_bindings(
    GET = function(...) {
      stop("AGSI should not be called without an API key")
    },
    .package = "httr"
  )

  expect_warning(
    result <- suppressMessages(
      agsi.get_storage_change(
        date_from = "2023-01-01",
        date_to = "2023-01-03",
        iso2 = "DE"
      )
    ),
    "AGSI_API_KEY not set"
  )
  expect_equal(nrow(result), 0)
})
