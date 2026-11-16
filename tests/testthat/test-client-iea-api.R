test_that("IEA clients cache source CSV and decode on cache hits", {
  cache <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = cache)
  calls <- list()
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    calls[[length(calls) + 1L]] <<- list(url = url, query = query)
    writeLines("year,value\n2023,2", path)
  })
  for (client in list(iea.get_balance, iea.get_conversion_factors)) {
    first <- client(2023, 2023, "DE", use_cache = TRUE)
    second <- client(2023, 2023, "DE", use_cache = TRUE)
    fresh <- client(2023, 2023, "DE", use_cache = FALSE)
    expect_identical(first, second)
    expect_identical(first, fresh)
    expect_equal(first$value, 2)
  }
  expect_length(calls, 4L)
  expect_equal(calls[[1]]$query$country, "DE")
  expect_equal(calls[[1]]$query$year_from, "2023")
  expect_length(list.files(cache, recursive = TRUE, pattern = "csv$"), 2L)
  expect_length(list.files(cache, recursive = TRUE, pattern = "parquet$"), 0L)
})

test_that("fixed-year IEA inputs remain cached across calendar days", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  day <- as.Date("2026-01-01")
  calls <- 0L
  local_mocked_bindings(Sys.Date = function() day, .package = "base")
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    calls <<- calls + 1L
    writeLines("year,value\n2023,2", path)
  })
  first <- iea.get_conversion_factors(2023, 2023, "DE")
  day <- day + 1
  second <- iea.get_conversion_factors(2023, 2023, "DE")
  expect_identical(first, second)
  expect_equal(calls, 1L)
})
