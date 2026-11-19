test_that("Ember caches unmodified JSON and normalizes on each read", {
  cache <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = cache)
  calls <- list()
  payload <- '{"data":[{"date":"2023","series":"Gas","generation_twh":1.25,"extra":"kept"}]}'
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    calls[[length(calls) + 1L]] <<- list(url = url, query = query)
    writeLines(payload, path)
  })
  first <- ember.get_power_generation("yearly", "DE")
  second <- ember.get_power_generation("yearly", "DE")
  expect_identical(first, second)
  expect_length(calls, 1L)
  expect_equal(calls[[1]]$query$entity_code, "DEU")
  expect_match(calls[[1]]$url, "/electricity-generation/yearly/")
  expect_equal(first$value_mwh, 1.25e6)
  expect_equal(first$date, as.Date("2023-01-01"))
  expect_equal(readLines(list.files(cache, recursive = TRUE, full.names = TRUE)), payload)

  fresh <- ember.get_power_generation("yearly", "DE", use_cache = FALSE)
  expect_identical(first, fresh)
  expect_length(calls, 2L)
})

test_that("Ember capacity uses native input caching", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    expect_equal(query$entity, "EU")
    writeLines('{"data":[{"date":"2023-01-01","series":"Solar","capacity_gw":2.5}]}', path)
  })
  result <- ember.get_installed_capacity("monthly", "EU", use_cache = FALSE)
  expect_equal(result$value_mw, 2500)
  expect_equal(result$source, "Solar")
})

test_that("artifact failures are not swallowed by Ember's source fallback", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  local_mocked_bindings(.raw_input_get = function(...) {
    stop(structure(list(message = "snapshot failed", call = NULL),
      class = c("pipeline_artifact_error", "error", "condition")))
  })
  expect_error(ember.get_power_generation("monthly", "EU"), class = "pipeline_artifact_error")
})
