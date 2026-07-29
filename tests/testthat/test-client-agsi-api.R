library(testthat)

local_agsi_cache_dir <- function() {
  cache_dir <- tempfile("agsi-client-test-")
  dir.create(cache_dir, recursive = TRUE)
  withr::local_options(
    list(creaco2tracker.cache_dir = cache_dir),
    .local_envir = parent.frame()
  )
  cache_dir
}

write_agsi_bundle <- function(
  filepath,
  country_code = c("DE", "FR", "DE", "FR", "DE"),
  gas_day = as.Date(c(
    "2022-12-31",
    "2023-01-01",
    "2023-01-02",
    "2023-01-03",
    "2023-01-04"
  )),
  net_withdrawal = c(1, 2, 11.3505, 4, 5),
  unused_column = "not requested"
) {
  arrow::write_parquet(
    tibble(
      country_code = country_code,
      gas_day = gas_day,
      net_withdrawal = net_withdrawal,
      unused_column = unused_column
    ),
    filepath
  )
}

local_agsi_download_mock <- function(source_path, calls, fail = FALSE) {
  local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet, ...) {
      calls$count <- calls$count + 1L
      calls$url <- url

      if (fail) {
        stop("simulated download failure")
      }

      file.copy(source_path, destfile, overwrite = TRUE)
      0L
    },
    .package = "utils",
    .env = parent.frame()
  )
}

test_that("agsi.get_storage_change filters and maps the parquet bundle", {
  cache_dir <- local_agsi_cache_dir()
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls)

  result <- suppressMessages(
    agsi.get_storage_change(
      date_from = "2023-01-01",
      date_to = "2023-01-03",
      iso2 = c("DE", "FR"),
      use_cache = FALSE
    )
  ) %>%
    arrange(.data$date)

  expect_equal(calls$count, 1L)
  expect_match(calls$url, "country_daily\\.parquet$")
  expect_equal(result$iso2, c("FR", "DE", "FR"))
  expect_equal(
    result$date,
    as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  )
  expect_equal(result$value_gwh, c(2, 11.3505, 4))
  expect_equal(result$value_m3[[2]], 1e6, tolerance = 1)
  expect_equal(result$type, rep("storage_drawdown", 3))
  expect_false(file.exists(file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)))
})

test_that("AGSI current-period data must contain values from the last three days", {
  reference_date <- as.Date("2026-07-29")
  fresh_data <- tibble(
    date = reference_date - 3,
    value_gwh = 0
  )
  stale_data <- tibble(
    date = reference_date - 4,
    value_gwh = 1
  )

  expect_invisible(
    .agsi_check_recent_values(
      data = fresh_data,
      date_to = reference_date,
      reference_date = reference_date
    )
  )
  expect_error(
    .agsi_check_recent_values(
      data = stale_data,
      date_to = reference_date,
      reference_date = reference_date
    ),
    "no values since 2026-07-26"
  )
  expect_invisible(
    .agsi_check_recent_values(
      data = stale_data,
      date_to = as.Date("2023-01-03"),
      reference_date = reference_date
    )
  )
})

test_that("AGSI bundle filters remain lazy until collection", {
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)

  query <- .agsi_build_bundle_query(
    filepath = source_path,
    date_from = "2023-01-01",
    date_to = "2023-01-03",
    iso2 = c("DE", "FR")
  )
  query_description <- paste(capture.output(print(query)), collapse = "\n")

  expect_s3_class(query, "arrow_dplyr_query")
  expect_named(query, c("iso2", "date", "value_gwh"))
  expect_match(query_description, "Filter:")
  expect_match(query_description, "country_code")
  expect_match(query_description, "gas_day")
})

test_that("agsi.get_storage_change returns an empty typed result for no matches", {
  local_agsi_cache_dir()
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls)

  result <- suppressMessages(
    agsi.get_storage_change(
      date_from = "2023-01-01",
      date_to = "2023-01-03",
      iso2 = "XX",
      use_cache = FALSE
    )
  )

  expect_equal(nrow(result), 0)
  expect_named(result, c("iso2", "date", "value_gwh", "value_m3", "type"))
  expect_type(result$iso2, "character")
  expect_s3_class(result$date, "Date")
})

test_that("agsi.get_storage_change reuses today's cached bundle", {
  cache_dir <- local_agsi_cache_dir()
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls)

  first <- suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
  )
  second <- suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
  )

  expect_equal(calls$count, 1L)
  expect_equal(second, first)
  expect_true(file.exists(file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)))
})

test_that("agsi.get_storage_change refreshes a cache fetched before today", {
  cache_dir <- local_agsi_cache_dir()
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls)

  suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
  )
  cache_path <- file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)
  Sys.setFileTime(cache_path, Sys.time() - 2 * 24 * 60 * 60)
  suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
  )

  expect_equal(calls$count, 2L)
  expect_true(cache_file_modified_today(cache_path))
})

test_that("agsi.get_storage_change always downloads when caching is disabled", {
  cache_dir <- local_agsi_cache_dir()
  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls)

  suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE", use_cache = FALSE)
  )
  suppressMessages(
    agsi.get_storage_change("2023-01-01", "2023-01-03", "DE", use_cache = FALSE)
  )

  expect_equal(calls$count, 2L)
  expect_false(file.exists(file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)))
})

test_that("failed refresh retains the previous bundle and raises an error", {
  cache_dir <- local_agsi_cache_dir()
  cache_path <- file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)
  write_agsi_bundle(cache_path)
  Sys.setFileTime(cache_path, Sys.time() - 2 * 24 * 60 * 60)
  original_hash <- digest::digest(file = cache_path)

  source_path <- tempfile(fileext = ".parquet")
  write_agsi_bundle(source_path)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(source_path, calls, fail = TRUE)

  expect_error(
    suppressMessages(
      agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
    ),
    "simulated download failure"
  )

  expect_equal(calls$count, 1L)
  expect_equal(digest::digest(file = cache_path), original_hash)
})

test_that("invalid refresh retains the previous bundle and raises an error", {
  cache_dir <- local_agsi_cache_dir()
  cache_path <- file.path(cache_dir, AGSI_COUNTRY_DAILY_CACHE_FILENAME)
  write_agsi_bundle(cache_path)
  Sys.setFileTime(cache_path, Sys.time() - 2 * 24 * 60 * 60)
  original_hash <- digest::digest(file = cache_path)

  invalid_source <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tibble(unexpected = 1), invalid_source)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  local_agsi_download_mock(invalid_source, calls)

  expect_error(
    suppressMessages(
      agsi.get_storage_change("2023-01-01", "2023-01-03", "DE")
    ),
    "missing required columns"
  )

  expect_equal(digest::digest(file = cache_path), original_hash)
})
