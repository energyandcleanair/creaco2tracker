library(testthat)

test_that("cache_file_get_or_refresh populates and reuses a fresh cache", {
  cache_path <- tempfile(fileext = ".txt")
  populate_count <- 0L

  populate_fun <- function(filepath) {
    populate_count <<- populate_count + 1L
    writeLines(paste0("value-", populate_count), filepath)
  }

  first <- cache_file_get_or_refresh(
    filepath = cache_path,
    populate_fun = populate_fun,
    consume_fun = readLines
  )
  second <- cache_file_get_or_refresh(
    filepath = cache_path,
    populate_fun = populate_fun,
    consume_fun = readLines
  )

  expect_equal(first, "value-1")
  expect_equal(second, first)
  expect_equal(populate_count, 1L)
})

test_that("cache_file_get_or_refresh replaces a stale cache", {
  cache_path <- tempfile(fileext = ".txt")
  writeLines("old", cache_path)

  result <- cache_file_get_or_refresh(
    filepath = cache_path,
    populate_fun = function(filepath) writeLines("new", filepath),
    consume_fun = readLines,
    is_fresh_fun = function(filepath) FALSE
  )

  expect_equal(result, "new")
  expect_equal(readLines(cache_path), "new")
})

test_that("cache_file_get_or_refresh cleans up files when caching is disabled", {
  cache_path <- tempfile(fileext = ".txt")
  consumed_path <- NULL

  result <- cache_file_get_or_refresh(
    filepath = cache_path,
    populate_fun = function(filepath) writeLines("temporary", filepath),
    consume_fun = function(filepath) {
      consumed_path <<- filepath
      expect_true(file.exists(filepath))
      readLines(filepath)
    },
    use_cache = FALSE
  )

  expect_equal(result, "temporary")
  expect_false(file.exists(consumed_path))
  expect_false(file.exists(cache_path))
})

test_that("cache_file_get_or_refresh preserves old cache after validation failure", {
  cache_path <- tempfile(fileext = ".txt")
  writeLines("old", cache_path)

  expect_error(
    cache_file_get_or_refresh(
      filepath = cache_path,
      populate_fun = function(filepath) writeLines("invalid", filepath),
      consume_fun = readLines,
      is_fresh_fun = function(filepath) FALSE,
      validate_fun = function(filepath) stop("invalid cache")
    ),
    "invalid cache"
  )

  expect_equal(readLines(cache_path), "old")
})

test_that("cache_file_modified_today uses the local calendar date", {
  cache_path <- tempfile(fileext = ".txt")
  writeLines("cached", cache_path)
  local_date <- Sys.Date()
  Sys.setFileTime(cache_path, as.POSIXct(paste(local_date, "12:00:00")))

  expect_true(cache_file_modified_today(cache_path, current_date = local_date))
  expect_false(cache_file_modified_today(cache_path, current_date = local_date + 1))
  expect_false(cache_file_modified_today(tempfile(), current_date = local_date))
})
