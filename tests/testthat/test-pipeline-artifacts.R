test_that("snapshots preserve objects and expose tabular provenance without indexes", {
  root <- withr::local_tempdir()
  prior <- getOption("creaco2tracker.artifacts")
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  value <- tibble::tibble(date = as.Date("2023-01-01"), value = NA_real_)
  attr(value, "provenance") <- tibble::tibble(method = "reported")
  expect_identical(.pipeline_snapshot(value, "final"), value)
  files <- list.files(file.path(root, "bundle"))
  expect_equal(files, c("99_000_co2_provenance.parquet", "99_000_co2.parquet")[order(
    c("99_000_co2_provenance.parquet", "99_000_co2.parquet")
  )])
  expect_equal(
    arrow::read_parquet(file.path(root, "bundle", "99_000_co2.parquet"))$value,
    NA_real_
  )
  expect_error(.pipeline_artifacts_start(file.path(root, "bundle"), TRUE), "new or empty")
  restore()
  expect_identical(getOption("creaco2tracker.artifacts"), prior)
  expect_identical(.pipeline_snapshot(value, "final"), value)
})

test_that("failed snapshot writes propagate and retain earlier files", {
  root <- withr::local_tempdir()
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  .pipeline_snapshot(tibble::tibble(value = 1), "regions")
  expect_error(.pipeline_artifact_write("broken.parquet", function(path) stop("disk failure")),
    "disk failure",
    class = "pipeline_artifact_error"
  )
  expect_equal(list.files(file.path(root, "bundle")), "70_010_selected_regions.parquet")
})

test_that("reference tables are captured without changing their values", {
  expected <- get_ipcc_emission_factors()
  root <- withr::local_tempdir()
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  expect_identical(get_ipcc_emission_factors(), expected)
  expect_true(file.exists(file.path(
    root, "bundle",
    "50_020_reference_factors_ipcc_emission_factors.parquet"
  )))
})

test_that("get_co2 produces identical results and snapshots using a shared raw cache", {
  root <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = file.path(root, "cache"))
  input <- tidyr::crossing(
    iso2 = "DE", date = as.Date("2023-01-01"), fuel = c("gas", "oil", "coal"),
    sector = c("electricity", "others"), unit = "t", estimate = "central", value = 100
  )
  payload <- tempfile(fileext = ".csv")
  readr::write_csv(input, payload)
  on.exit(unlink(payload), add = TRUE)
  calls <- 0L
  fetch <- function(kind) {
    .crea_raw_get(paste0("https://example.test/", kind),
      date_to = "2023-01-01",
      use_cache = TRUE, stage = if (kind == "gas") "entsog" else "eurostat"
    )
  }
  local_mocked_bindings(
    .raw_http_download = function(url, query, path, ...) {
      calls <<- calls + 1L
      file.copy(payload, path)
    },
    get_gas_demand = function(...) fetch("gas"),
    get_power_generation = function(...) fetch("power"),
    get_eurostat_cons = function(...) fetch("consumption"),
    get_eurostat_indprod = function(...) fetch("industry"),
    coal_prepare_total_forecasts = function(x, ...) x,
    get_co2_from_eurostat_cons = function(eurostat_cons, ...) eurostat_cons,
    project_until_now = function(co2_unprojected, ...) co2_unprojected
  )
  run <- function(folder = NULL) {
    get_co2(
      iso2s = "DE", date_to = as.Date("2023-01-01"), downscale_daily = FALSE,
      diagnostics_folder = NULL, use_cache = TRUE, artifacts_folder = folder
    )
  }
  first <- run(file.path(root, "a"))
  first_calls <- calls
  second <- run(file.path(root, "b"))
  third <- run()
  expect_identical(first, second)
  expect_identical(first, third)
  expect_equal(first_calls, 4L)
  expect_equal(calls, first_calls)
  expect_null(getOption("creaco2tracker.artifacts"))
  files <- list.files(file.path(root, "a"))
  expect_identical(files, list.files(file.path(root, "b")))
  expect_true("99_000_co2.parquet" %in% files)
  expect_false(any(grepl("index|manifest|metadata", files)))
  for (name in files) {
    a <- file.path(root, "a", name)
    b <- file.path(root, "b", name)
    if (grepl("parquet$", name)) {
      expect_identical(arrow::read_parquet(a), arrow::read_parquet(b), info = name)
    } else {
      expect_identical(readBin(a, "raw", n = 1e6), readBin(b, "raw", n = 1e6), info = name)
    }
  }
  expect_identical(arrow::read_parquet(file.path(root, "a", "99_000_co2.parquet")), first)
  expect_error(run(file.path(root, "a")), "new or empty")
  expect_null(getOption("creaco2tracker.artifacts"))
})

test_that("ENTSOE snapshots distinguish source rows from completion and zero filling", {
  root <- withr::local_tempdir()
  withr::local_options(creaco2tracker.cache_dir = file.path(root, "cache"))
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  local_mocked_bindings(.raw_http_download = function(url, query, path, ...) {
    readr::write_csv(tibble::tibble(
      iso2 = "DE", region = "Europe", country = "Germany",
      date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-03")),
      source = c("Fossil Gas", "Solar", "Solar"), data_source = "entsoe",
      value_mwh = c(0, 10, 20), value_mw = c(0, 1, 2)
    ), path)
  })
  entsoe.get_power_generation("2023-01-01", "2023-01-03", "DE")
  files <- list.files(file.path(root, "bundle"), full.names = TRUE)
  decoded <- arrow::read_parquet(files[grepl("010_decoded", files)])
  completed <- arrow::read_parquet(files[grepl("20_030_power_entsoe_completed", files)])
  expect_equal(nrow(decoded), 3L)
  expect_false(any(decoded$source == "Fossil Gas" & decoded$date == as.Date("2023-01-03")))
  gas_tail <- completed[completed$source == "Fossil Gas" &
    completed$date == as.Date("2023-01-03"), ]
  expect_equal(gas_tail$value_mwh, 0)
})

test_that("source failures are not evaluated again while capturing masked inputs", {
  root <- withr::local_tempdir()
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), TRUE)
  on.exit(restore(), add = TRUE)
  calls <- 0L
  expect_error(apply_source_data_mask(
    {
      calls <- calls + 1L
      stop("fetch failed")
    },
    "entsog_flow_raw"
  ), "fetch failed")
  expect_equal(calls, 1L)
  expect_length(list.files(file.path(root, "bundle")), 0L)
})

test_that("get_co2 restores capture context after an execution failure", {
  root <- withr::local_tempdir()
  prior <- list(folder = NULL, use_cache = FALSE)
  withr::local_options(creaco2tracker.artifacts = prior)
  local_mocked_bindings(get_gas_demand = function(...) stop("source failed"))
  expect_error(get_co2(diagnostics_folder = NULL,
    artifacts_folder = file.path(root, "bundle")), "source failed")
  expect_identical(getOption("creaco2tracker.artifacts"), prior)
})

test_that("repeated input versions are retained without duplicating unchanged reads", {
  root <- withr::local_tempdir()
  restore <- .pipeline_artifacts_start(file.path(root, "bundle"), FALSE)
  on.exit(restore(), add = TRUE)
  input <- file.path(root, "source.csv.gz")
  writeBin(charToRaw("first payload"), input)
  .pipeline_input_file(input, "eurostat", "source.csv.gz")
  .pipeline_input_file(input, "eurostat", "source.csv.gz")
  writeBin(charToRaw("second payload"), input)
  .pipeline_input_file(input, "eurostat", "source.csv.gz")
  .pipeline_input_file(input, "eurostat", "source.csv.gz")
  files <- list.files(file.path(root, "bundle"))
  expect_length(files, 2L)
  expect_true("30_010_eurostat_source_002.csv.gz" %in% files)
  expect_identical(readBin(file.path(root, "bundle", "30_010_eurostat_source.csv.gz"),
    "raw", n = 100), charToRaw("first payload"))
  expect_identical(readBin(file.path(root, "bundle", "30_010_eurostat_source_002.csv.gz"),
    "raw", n = 100), charToRaw("second payload"))
})
