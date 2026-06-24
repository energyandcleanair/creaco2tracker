library(testthat)
library(dplyr)
library(readr)
library(tibble)

source(testthat::test_path("..", "..", "scripts", "compare_lib", "compare_get_co2_external.R"))

test_that("external source collector exposes the normalized schema and defaults", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )

  expect_equal(
    names(collector_env$empty_external()),
    c("source_id", "source", "period", "iso2", "date", "year", "value_mt", "unit")
  )
  expect_setequal(
    collector_env$resolve_sources("all")$source_id,
    c(
      "climate-watch",
      "unfccc",
      "pik",
      "global-carbon-budget-2025",
      "carbon-monitor",
      "primap-energy-and-industry",
      "primap-energy-and-industry-excl-mineral-industry"
    )
  )
  carbon_monitor <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor")
  expect_false(carbon_monitor$annual)
  expect_true(carbon_monitor$monthly)
})

test_that("external source collector skips unsupported Carbon Monitor annual rows", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )

  carbon_monitor <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor")
  result <- collector_env$collect_one_source_period(
    carbon_monitor,
    "annual",
    c("DE", "EU")
  )

  expect_equal(nrow(result$data), 0)
  expect_equal(result$status$status, "skipped")
})

test_that("external source collector maps Carbon Monitor uppercase countries", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )
  collector_env$download_carbonmonitor_raw <- function() {
    days <- seq(as.Date("2023-01-01"), as.Date("2023-01-31"), by = "day")
    tibble(
      country = rep(c("GERMANY", "EU27 & UK", "UNITED KINGDOM"), each = length(days)),
      date = rep(format(days, "%d/%m/%Y"), times = 3),
      sector = "Power",
      value = c(rep(1, length(days)), rep(10, length(days)), rep(2, length(days)))
    )
  }

  monthly <- collector_env$normalise_carbonmonitor_monthly(
    "carbon-monitor",
    "Carbon Monitor",
    c("DE", "EU")
  ) %>%
    arrange(iso2)

  expect_equal(monthly$iso2, c("DE", "EU"))
  expect_equal(monthly$value_mt, c(31, 248))
})

test_that("external comparison filters only complete periods", {
  input <- tibble(
    period = c("annual", "annual", "monthly", "monthly"),
    date = as.Date(c("2023-01-01", "2024-01-01", "2024-01-01", "2024-02-01")),
    year = c(2023L, 2024L, 2024L, 2024L),
    value_mt = c(1, 2, 3, 4)
  )

  filtered <- filter_complete_periods(input, as.Date("2024-02-15"))

  expect_equal(filtered$period, c("annual", "monthly"))
  expect_equal(filtered$date, as.Date(c("2023-01-01", "2024-01-01")))
})

test_that("external comparison reports raw and aviation-adjusted CREA totals", {
  raw <- tibble(
    iso2 = c("EU", "EU", "EU", "EU", "DE", "DE"),
    date = as.Date(c(
      "2023-01-01", "2023-01-01", "2023-02-01",
      "2023-02-01", "2023-01-01", "2023-01-01"
    )),
    fuel = c("total", "oil", "total", "oil", "total", "oil"),
    sector = c(
      "all",
      "transport_international_aviation",
      "all",
      "transport_international_aviation",
      "all",
      "transport_international_aviation"
    ),
    estimate = "central",
    value = c(100e6, 10e6, 120e6, 20e6, 50e6, 5e6)
  )

  totals <- normalise_crea_totals(raw, as.Date("2023-12-31"))
  eu_annual <- totals %>%
    filter(period == "annual", iso2 == "EU") %>%
    select(crea_variant, value_mt) %>%
    arrange(crea_variant)

  expect_equal(eu_annual$crea_variant, c("adjusted", "raw"))
  expect_equal(eu_annual$value_mt, c(190, 220))

  eu_january <- totals %>%
    filter(period == "monthly", iso2 == "EU", date == as.Date("2023-01-01")) %>%
    select(crea_variant, value_mt) %>%
    arrange(crea_variant)

  expect_equal(eu_january$value_mt, c(90, 100))
})

test_that("external comparison pairs external rows with both CREA variants", {
  crea <- tibble(
    crea_variant = c("raw", "adjusted"),
    period = "annual",
    iso2 = "EU",
    date = as.Date("2023-01-01"),
    year = 2023L,
    value_mt = c(220, 190),
    unit = "Mt"
  )
  external <- tibble(
    source_id = "example",
    source = "Example",
    period = "annual",
    iso2 = "EU",
    date = as.Date("2023-01-01"),
    year = 2023L,
    value_mt = 200,
    unit = "Mt"
  )

  pairs <- make_pairs(crea, external)

  expect_equal(sort(pairs$crea_variant), c("adjusted", "raw"))
  expect_false("fuel" %in% names(pairs))
  expect_false("sector" %in% names(pairs))
  expect_equal(
    pairs %>% arrange(crea_variant) %>% pull(diff_mt),
    c(-10, 20)
  )
})

test_that("external comparison report writes expected fixture artifacts", {
  comparison_dir <- tempfile("external-comparison-")
  dir.create(comparison_dir)

  raw_crea <- tibble(
    iso2 = c("EU", "EU", "DE", "DE", "EU", "EU", "DE", "DE"),
    date = as.Date(c(
      "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01",
      "2023-02-01", "2023-02-01", "2023-02-01", "2023-02-01"
    )),
    fuel = c("total", "oil", "total", "oil", "total", "oil", "total", "oil"),
    sector = rep(c("all", "transport_international_aviation"), 4),
    estimate = "central",
    value = c(100e6, 10e6, 50e6, 5e6, 120e6, 20e6, 60e6, 6e6)
  )
  external <- tibble(
    source_id = c("example", "example", "example", "carbon-monitor", "carbon-monitor"),
    source = c("Example", "Example", "Example", "Carbon Monitor", "Carbon Monitor"),
    period = c("annual", "annual", "annual", "monthly", "monthly"),
    iso2 = c("EU", "DE", "EU", "EU", "DE"),
    date = as.Date(c("2023-01-01", "2023-01-01", "2024-01-01", "2023-01-01", "2023-01-01")),
    year = c(2023L, 2023L, 2024L, 2023L, 2023L),
    value_mt = c(205, 105, 999, 95, 48),
    unit = "Mt"
  )
  status <- tibble(
    source_id = c("example", "carbon-monitor"),
    source = c("Example", "Carbon Monitor"),
    period = c("annual", "monthly"),
    status = "ok",
    message = "",
    rows = c(2L, 2L)
  )

  raw_path <- file.path(comparison_dir, "raw.csv")
  external_path <- file.path(comparison_dir, "external.csv")
  status_path <- file.path(comparison_dir, "status.csv")
  write_csv(raw_crea, raw_path)
  write_csv(external, external_path)
  write_csv(status, status_path)

  run_compare(list(
    comparison_dir = comparison_dir,
    raw_crea = raw_path,
    external_raw = external_path,
    source_status = status_path,
    target_label = "fixture",
    date_to = "2023-12-31"
  ))

  expect_true(file.exists(file.path(comparison_dir, "summary.md")))
  expect_true(file.exists(file.path(comparison_dir, "annual_pairs.csv")))
  expect_true(file.exists(file.path(comparison_dir, "plots", "annual_eu_timeseries.png")))
  expect_true(file.exists(file.path(
    comparison_dir,
    "plots",
    "annual_eu_timeseries_by_provider.png"
  )))
  expect_true(file.exists(file.path(
    comparison_dir,
    "plots",
    "annual_eu_timeseries_example.png"
  )))
  expect_true(file.exists(file.path(
    comparison_dir,
    "plots",
    "annual_country_timeseries_example.png"
  )))

  annual_pairs <- read_csv(file.path(comparison_dir, "annual_pairs.csv"), show_col_types = FALSE)
  expect_setequal(unique(annual_pairs$crea_variant), c("raw", "adjusted"))
  expect_equal(unique(annual_pairs$year), 2023L)
  expect_true("source_short" %in% names(annual_pairs))
  expect_false("fuel" %in% names(annual_pairs))
  expect_false("sector" %in% names(annual_pairs))
})
