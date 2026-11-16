test_that("raw cache preserves payloads, parses hits, and bypasses persistent cache", {
  cache <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = cache)
  fetches <- 0L
  decodes <- 0L
  payload <- "date,value,unused\n2023-01-01,1,original\n2023-01-03,NA,original\n"
  fetch <- function(path) {
    fetches <<- fetches + 1L
    writeBin(charToRaw(payload), path)
  }
  decode <- function(path) {
    decodes <<- decodes + 1L
    .raw_read_csv(path)
  }
  run <- function() .raw_input_get("test", "key", "csv", fetch, decode, "entsog")
  restore <- .pipeline_artifacts_start(file.path(cache, "a"), TRUE)
  on.exit(restore(), add = TRUE)
  first <- run()
  restore()
  restore <- .pipeline_artifacts_start(file.path(cache, "b"), TRUE)
  second <- run()
  expect_identical(first, second)
  expect_equal(fetches, 1L)
  expect_equal(decodes, 3L) # validation on miss, then consumption on both runs
  expect_equal(names(first), c("date", "value", "unused"))
  expect_equal(nrow(first), 2L)
  expect_true(is.na(first$value[[2]]))
  for (name in list.files(file.path(cache, "a"))) {
    expect_identical(
      readBin(file.path(cache, "a", name), "raw", n = 1e6),
      readBin(file.path(cache, "b", name), "raw", n = 1e6)
    )
  }
  stored <- list.files(file.path(cache, "raw"), full.names = TRUE)
  expect_identical(readBin(stored, "raw", n = 1e6), charToRaw(payload))
  .raw_input_get("test", "key", "csv", fetch, decode, "entsog", use_cache = FALSE)
  expect_equal(fetches, 2L)
  expect_length(list.files(file.path(cache, "raw")), 1L)
})

test_that("split CREA requests retain boundaries and source fields", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  queries <- list()
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    queries[[length(queries) + 1L]] <<- query
    writeLines("date,value,extra\n2023-01-01,1,source", path)
  })
  result <- .crea_raw_get("https://example.test/flows", "2022-01-01", "2023-01-02",
    split_by = "year", stage = "entsog", country = c("DE", "FR")
  )
  expect_length(queries, 2L)
  expect_equal(queries[[1]]$date_from, "2022-01-01")
  expect_equal(queries[[1]]$date_to, "2022-12-31")
  expect_equal(queries[[2]]$date_from, "2023-01-01")
  expect_equal(queries[[2]]$date_to, "2023-01-02")
  expect_equal(queries[[1]]$country, "DE,FR")
  expect_equal(result$extra, c("source", "source"))
})

test_that("native requests retry failures with exponential backoff", {
  attempts <- 0L
  delays <- numeric()
  local_mocked_bindings(GET = function(...) {
    attempts <<- attempts + 1L
    if (attempts < 3L) stop("transient")
    structure(list(status_code = 200L, url = "https://example.test"), class = "response")
  }, .package = "httr")
  local_mocked_bindings(Sys.sleep = function(time) delays <<- c(delays, time), .package = "base")
  .raw_http_download("https://example.test", list(), tempfile())
  expect_equal(attempts, 3L)
  expect_equal(delays, c(1, 2))
})

test_that("failed raw payload validation does not populate the cache", {
  cache <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = cache)
  expect_error(
    .raw_input_get(
      "invalid", "key", "csv",
      function(path) writeLines("bad", path), function(path) stop("invalid payload"), "entsog"
    ),
    "invalid payload"
  )
  expect_length(list.files(cache, recursive = TRUE), 0L)
})

test_that("a decoding failure retains the original input in the partial bundle", {
  root <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = file.path(root, "cache"))
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  expect_error(.raw_input_get("malformed", "key", "json",
    function(path) writeLines("invalid JSON", path),
    function(path) jsonlite::fromJSON(path), "ember"), "lexical error")
  path <- file.path(root, "bundle", "20_040_power_ember_malformed_000_raw.json")
  expect_equal(readLines(path), "invalid JSON")
  expect_length(list.files(file.path(root, "cache"), recursive = TRUE), 0L)
})

test_that("native requests rethrow after exhausting their retries", {
  calls <- 0L
  delays <- numeric()
  local_mocked_bindings(GET = function(...) {
    calls <<- calls + 1L
    stop("persistent source failure")
  }, .package = "httr")
  local_mocked_bindings(Sys.sleep = function(time) delays <<- c(delays, time),
    .package = "base")
  expect_error(.raw_http_download("https://example.test", list(), tempfile()),
    "persistent source failure")
  expect_equal(calls, 3L)
  expect_equal(delays, c(1, 2))
})
