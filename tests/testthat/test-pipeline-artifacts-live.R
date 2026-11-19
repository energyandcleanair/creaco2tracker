# Opt-in: this fetches the complete get_co2 source inputs into a fresh raw cache.
# Run with RUN_PIPELINE_ARTIFACT_TESTS=TRUE and the source API credentials configured.
test_that("real get_co2 snapshots and results are identical with a shared raw cache", {
  skip_if_not(toupper(Sys.getenv("RUN_PIPELINE_ARTIFACT_TESTS", "FALSE")) == "TRUE",
    "Set RUN_PIPELINE_ARTIFACT_TESTS=TRUE to run the full shared-cache comparison")
  skip_if_client_envvar_missing("API_KEY")
  skip_if_client_envvar_missing("EMBER_KEY")
  root <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = file.path(root, "cache"))
  run <- function(name = NULL) get_co2(
    diagnostics_folder = NULL, iso2s = "EU", date_to = as.Date("2025-12-31"),
    min_year = 2020, use_cache = TRUE,
    artifacts_folder = if (is.null(name)) NULL else file.path(root, name)
  )
  first <- run("first")
  unexpected_requests <- 0L
  fail_request <- function(...) {
    unexpected_requests <<- unexpected_requests + 1L
    stop("Unexpected source request on warm run")
  }
  local_mocked_bindings(
    .raw_http_download = fail_request,
    .agsi_download_bundle = fail_request
  )
  second <- run("second")
  third <- run()
  expect_identical(first, second)
  expect_identical(first, third)
  expect_equal(unexpected_requests, 0L)
  files <- list.files(file.path(root, "first"))
  expect_identical(files, list.files(file.path(root, "second")))
  for (name in files) {
    a <- file.path(root, "first", name)
    b <- file.path(root, "second", name)
    if (grepl("\\.parquet$", name) && !grepl("_000_raw\\.", name)) {
      expect_identical(arrow::read_parquet(a), arrow::read_parquet(b), info = name)
    } else {
      expect_identical(digest::digest(file = a), digest::digest(file = b), info = name)
    }
  }
})
