.eurostat_json_fixture <- paste0(
  '{"id":["geo","time"],"size":[1,2],"dimension":{',
  '"geo":{"category":{"index":{"DE":0},"label":{"DE":"Germany"}}},',
  '"time":{"category":{"index":{"2023-01":0,"2023-02":1},',
  '"label":{"2023-01":"2023-01","2023-02":"2023-02"}}}},',
  '"value":{"0":2},"status":{"1":"p"}}'
)

test_that("filtered Eurostat caches JSON before calendar conversion", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  calls <- list()
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    calls[[length(calls) + 1L]] <<- query
    writeLines(.eurostat_json_fixture, path)
  })
  result <- get_eurostat_from_code("nrg_cb_gas", "DE", filters = list(siec = c("A", "B")))
  again <- get_eurostat_from_code("nrg_cb_gas", "DE", filters = list(siec = c("A", "B")))
  expect_identical(result, again)
  expect_length(calls, 1L)
  expect_equal(unlist(calls[[1]][names(calls[[1]]) == "siec"]), c(siec = "A", siec = "B"))
  expect_equal(calls[[1]]$geo, "DE")
  expect_equal(result$time, as.Date(c("2023-01-01", "2023-02-01")))
  expect_equal(result$values, c(2, NA_real_))
})

test_that("bulk Eurostat caches compressed TSV and retains the established decoder", {
  withr::local_options(creaco2tracker.cache_dir = withr::local_tempdir())
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    connection <- gzfile(path, "wt")
    writeLines("freq,unit,geo\\TIME_PERIOD\t2023-01 \t2023-02 \nM,TJ,DE\t2 \t: p", connection)
    close(connection)
  })
  result <- get_eurostat_from_code("test_bulk")
  expect_equal(result$geo, c("DE", "DE"))
  expect_equal(result$values, c(2, NA_real_))
  expect_s3_class(result$time, "Date")
})
