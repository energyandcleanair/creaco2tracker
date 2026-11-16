test_that("validation inputs retain native files and honor the pipeline cache setting", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.csv")
  calls <- 0L
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    calls <<- calls + 1L
    writeLines(paste0("original,value\nreported,", calls), path)
  })
  restore <- .pipeline_artifacts_start(file.path(root, "a"), TRUE)
  on.exit(restore(), add = TRUE)
  first <- .validation_input("https://example.test/source.csv", path)
  second <- .validation_input("https://example.test/source.csv", path)
  expect_identical(first, second)
  expect_equal(calls, 1L)
  expect_equal(readLines(file.path(root, "a", "90_010_validation_inputs_000_raw_source.csv")),
    c("original,value", "reported,1"))
  expect_true(file.exists(file.path(root, "a",
    "90_010_validation_inputs_010_decoded_source.parquet")))
  restore()
  restore <- .pipeline_artifacts_start(file.path(root, "b"), FALSE)
  fresh <- .validation_input("https://example.test/source.csv", path)
  expect_equal(fresh$value, 2)
  expect_equal(.raw_read_csv(path)$value, 1)
})
