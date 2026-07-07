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
      "iea-carbon-emissions",
      "carbon-monitor",
      "carbon-monitor-excl-bunkers",
      "primap-energy-and-industry",
      "primap-energy-and-industry-excl-mineral-industry"
    )
  )
  carbon_monitor <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor")
  expect_true(carbon_monitor$annual)
  expect_true(carbon_monitor$monthly)

  carbon_monitor_excl_bunkers <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor-excl-bunkers")
  expect_true(carbon_monitor_excl_bunkers$annual)
  expect_true(carbon_monitor_excl_bunkers$monthly)
})

test_that("external source collector derives IEA annual carbon emissions", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )
  collector_env$iea.get_balance <- function(year_from, year_to, iso2, use_cache) {
    expect_equal(year_from, 1990)
    expect_equal(year_to, 2024)
    expect_equal(iso2, c("DE", "FR"))
    expect_true(use_cache)

    tibble(
      iso2 = c("DE", "DE", "DE", "DE", "DE", "FR", "FR", "DE", "FR"),
      year = c(rep(2023L, 7), 2024L, 2024L),
      product_raw = c(
        "NATURAL_GAS", "NATURAL_GAS", "COAL", "OIL_TOTAL",
        "TOTAL", "NATURAL_GAS", "NATURAL_GAS", "NATURAL_GAS", "NATURAL_GAS"
      ),
      flow_raw = c("TFC", "NE_TOT", "MAINELEC", "TFC", "TFC", "TFC", "TFC", "TFC", "TFC"),
      unit = c("TJ", "TJ", "TJ", "TJ", "TJ", "TJ", "KTOE", "TJ", "TJ"),
      value = c(1e6, 1e5, -1e6, 1e6, 999, 2e6, 100, 1, 1)
    )
  }

  iea <- collector_env$source_catalog() %>%
    filter(source_id == "iea-carbon-emissions")

  annual <- collector_env$collect_one_source_period(
    iea,
    "annual",
    c("DE", "FR", "EU"),
    as.Date("2025-06-30")
  )

  expect_equal(annual$status$status, "ok")
  expect_equal(
    annual$data %>% arrange(iso2, year) %>% select(iso2, year, value_mt),
    tibble(
      iso2 = c("DE", "DE", "EU", "EU", "FR", "FR"),
      year = c(2023L, 2024L, 2023L, 2024L, 2023L, 2024L),
      value_mt = c(215.966, NA, 327.446, NA, 111.48, NA)
    )
  )
})

test_that("external source collector aggregates complete Carbon Monitor annual rows", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )
  collector_env$download_carbonmonitor_raw <- function() {
    days <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
    tibble(
      country = rep("GERMANY", each = length(days) * 3),
      date = rep(format(days, "%d/%m/%Y"), times = 3),
      sector = rep(c("Power", "International Aviation", "Shipping"), each = length(days)),
      value = c(rep(1, length(days)), rep(10, length(days)), rep(100, length(days)))
    )
  }

  carbon_monitor <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor")
  carbon_monitor_excl_bunkers <- collector_env$source_catalog() %>%
    filter(source_id == "carbon-monitor-excl-bunkers")

  annual_raw <- collector_env$collect_one_source_period(carbon_monitor, "annual", c("DE"))
  annual_excl_bunkers <- collector_env$collect_one_source_period(
    carbon_monitor_excl_bunkers,
    "annual",
    c("DE")
  )

  expect_equal(annual_raw$data$value_mt, 40515)
  expect_equal(annual_excl_bunkers$data$value_mt, 365)
  expect_equal(annual_raw$status$status, "ok")
  expect_equal(annual_excl_bunkers$status$status, "ok")
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

test_that("external source collector can exclude Carbon Monitor bunker sectors", {
  collector_env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "scripts", "compare_lib", "collect_external_co2_sources.R"),
    envir = collector_env
  )
  collector_env$download_carbonmonitor_raw <- function() {
    days <- seq(as.Date("2023-01-01"), as.Date("2023-01-31"), by = "day")
    tibble(
      country = rep("GERMANY", each = length(days) * 3),
      date = rep(format(days, "%d/%m/%Y"), times = 3),
      sector = rep(c("Power", "International Aviation", "Shipping"), each = length(days)),
      value = c(rep(1, length(days)), rep(10, length(days)), rep(100, length(days)))
    )
  }

  monthly_raw <- collector_env$normalise_carbonmonitor_monthly(
    "carbon-monitor",
    "Carbon Monitor",
    c("DE")
  )
  monthly_excl_bunkers <- collector_env$normalise_carbonmonitor_monthly(
    "carbon-monitor-excl-bunkers",
    "Carbon Monitor (excl. aviation and shipping)",
    c("DE"),
    exclude_bunkers = TRUE
  )

  expect_equal(monthly_raw$value_mt, 3441)
  expect_equal(monthly_excl_bunkers$value_mt, 31)
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

test_that("external comparison trend agreement uses raw annual values", {
  annual_pairs <- tibble(
    source_id = rep("example", 8),
    source_short = rep("Example", 8),
    crea_variant = rep(c("raw", "adjusted"), each = 4),
    iso2 = rep("EU", 8),
    date = rep(as.Date(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")), 2),
    year = rep(2021:2024, 2),
    external_value_mt = rep(c(100, 110, 108, 120), 2),
    crea_value_mt = c(90, 95, 94, 96, 90, 80, 85, 70),
    has_external = TRUE,
    has_crea = TRUE
  )

  agreement <- trend_agreement(annual_pairs) %>%
    filter(trend_compared)

  expect_equal(agreement$trend_agrees, c(TRUE, TRUE, TRUE))
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
    source_id = c("example", "carbon-monitor", "example"),
    source = c("Example", "Carbon Monitor", "Example"),
    period = c("annual", "monthly", "monthly"),
    status = c("ok", "ok", "skipped"),
    message = c("", "", "Monthly comparison is only supported for Carbon Monitor in v1."),
    rows = c(2L, 2L, 0L)
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
  summary_text <- readLines(file.path(comparison_dir, "summary.md"))
  expect_true(any(summary_text == "## EU annual raw totals"))
  expect_lt(
    match("## EU annual raw totals", summary_text),
    match("## EU annual adjusted totals", summary_text)
  )
  expect_true(any(summary_text == "<summary>Detailed comparison</summary>"))
  expect_false(any(grepl(
    "Monthly comparison is only supported for Carbon Monitor in v1.",
    summary_text,
    fixed = TRUE
  )))

  annual_pairs <- read_csv(file.path(comparison_dir, "annual_pairs.csv"), show_col_types = FALSE)
  expect_setequal(unique(annual_pairs$crea_variant), c("raw", "adjusted"))
  expect_equal(unique(annual_pairs$year), 2023L)
  expect_true("source_short" %in% names(annual_pairs))
  expect_false("fuel" %in% names(annual_pairs))
  expect_false("sector" %in% names(annual_pairs))
})
