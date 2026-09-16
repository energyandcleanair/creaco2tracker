library(testthat)
library(dplyr)

coal_rows <- function(iso2, siec, nrg_bal, dates, values, unit = "THS_T") {
  tibble(
    freq = "M", nrg_bal = nrg_bal, siec = siec, unit = unit,
    geo = iso2, iso2 = iso2, time = as.Date(dates), values = values
  )
}

test_that("coal gap filling interpolates internal gaps without changing observations", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-01"), by = "month")
  monthly <- coal_rows("SE", SIEC_HARD_COAL, "GID_CAL", dates, c(10, 20, NA, NA, 50, 60))

  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )
  result <- fill_raw_coal_monthly(monthly, tibble())
  series <- result %>% arrange(time)
  provenance <- attr(result, "coal_gap_provenance") %>% arrange(time)

  expect_equal(series$values, c(10, 20, 30, 40, 50, 60))
  expect_equal(provenance$method, c("reported", "reported", "interpolation",
      "interpolation", "reported", "reported"))
  expect_equal(series$values[c(1, 2, 5, 6)], monthly$values[c(1, 2, 5, 6)])
})

test_that("coal gap filling uses reported previous-year months for short tails", {
  prior_dates <- seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "month")
  current_dates <- seq(as.Date("2024-01-01"), as.Date("2024-04-01"), by = "month")
  gid <- bind_rows(
    coal_rows("SE", SIEC_HARD_COAL, "GID_CAL", prior_dates, 1:6),
    coal_rows("SE", SIEC_HARD_COAL, "GID_CAL", current_dates, 11:14)
  )
  cutoff <- coal_rows("SE", SIEC_OIL_SHALE, "GID_CAL", as.Date("2024-06-01"), 1)

  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )
  result <- fill_raw_coal_monthly(bind_rows(gid, cutoff), tibble())
  tail <- result %>%
    filter(siec == SIEC_HARD_COAL, time >= as.Date("2024-05-01")) %>%
    arrange(time)
  provenance <- attr(result, "coal_gap_provenance") %>%
    filter(siec == SIEC_HARD_COAL, time >= as.Date("2024-05-01"))

  expect_equal(tail$values, c(5, 6))
  expect_true(all(provenance$method == "previous_year"))
})

test_that("internal interpolation is enabled only for the validated shared system", {
  dates <- c(
    seq(as.Date("2023-01-01"), as.Date("2023-03-01"), by = "month"),
    seq(as.Date("2024-01-01"), as.Date("2024-03-01"), by = "month")
  )
  brown <- coal_rows(
    "GR", SIEC_BROWN_COAL, "GID_CAL", dates,
    c(100, 20, 300, 10, NA, 30)
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_monthly(brown, tibble())
  provenance <- attr(result, "coal_gap_provenance") %>%
    filter(time == as.Date("2024-02-01"))

  expect_equal(provenance$filled_value, 20)
  expect_equal(provenance$method, "previous_year")
  expect_equal(provenance$interpolation_reason, "validation_policy_not_enabled")
  expect_true(.coal_monthly_interpolation_enabled(SIEC_HARD_COAL, "GID_CAL", "internal"))
  expect_false(.coal_monthly_interpolation_enabled(SIEC_BROWN_COAL, "GID_CAL", "internal"))
})

test_that("gaps longer than six months and unavailable briquettes stay unresolved", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-09-01"), by = "month")
  monthly <- coal_rows(
    "SE", SIEC_HARD_COAL, "GID_CAL", dates,
    c(10, rep(NA_real_, 7), 90)
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )
  result <- fill_raw_coal_monthly(monthly, tibble())

  expect_true(all(is.na(result$values[2:8])))
  expect_false(any(result$siec == SIEC_BROWN_COAL_BRIQUETTES))
  exclusions <- attr(result, "coal_gap_exclusions")
  expect_true(any(exclusions$siec == SIEC_BROWN_COAL_BRIQUETTES))
})

test_that("gap fills are never reused as previous-year observations", {
  dates <- seq(as.Date("2022-12-01"), as.Date("2023-12-01"), by = "month")
  values <- c(10, NA, 30:40)
  gid <- coal_rows("SE", SIEC_HARD_COAL, "GID_CAL", dates, values)
  cutoff <- coal_rows("SE", SIEC_OIL_SHALE, "GID_CAL", as.Date("2024-01-01"), 1)
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_monthly(bind_rows(gid, cutoff), tibble())
  provenance <- attr(result, "coal_gap_provenance") %>%
    filter(siec == SIEC_HARD_COAL, time %in% as.Date(c("2023-01-01", "2024-01-01"))) %>%
    arrange(time)

  expect_equal(provenance$method, c("interpolation", "unresolved"))
  expect_equal(provenance$filled_value, c(20, NA))
})

test_that("accounting uses reported supply inputs and only prior annual years", {
  history_dates <- seq(as.Date("2021-01-01"), as.Date("2023-12-01"), by = "month")
  supply <- tidyr::crossing(
    time = history_dates,
    nrg_bal = COAL_MONTHLY_SUPPLY_BALANCES
  ) %>%
    mutate(
      freq = "M", siec = SIEC_HARD_COAL, unit = "THS_T", geo = "SE", iso2 = "SE",
      values = case_when(nrg_bal == "IPRD" ~ 10, nrg_bal == "IMP" ~ 2,
        nrg_bal == "EXP" ~ 1, TRUE ~ -1)
    )
  current_supply <- tidyr::crossing(
    time = as.Date("2024-01-01"), nrg_bal = COAL_MONTHLY_SUPPLY_BALANCES
  ) %>%
    mutate(
      freq = "M", siec = SIEC_HARD_COAL, unit = "THS_T", geo = "SE", iso2 = "SE",
      values = case_when(nrg_bal == "IPRD" ~ 20, nrg_bal == "IMP" ~ 4,
        nrg_bal == "EXP" ~ 2, TRUE ~ -2)
    )
  gid <- coal_rows(
    "SE", SIEC_HARD_COAL, "GID_CAL",
    c(as.Date("2023-12-01"), as.Date("2024-01-01")), c(10, NA)
  )
  annual <- tibble(
    freq = "A", nrg_bal = "IC_CAL", siec = SIEC_HARD_COAL, unit = "THS_T",
    geo = "SE", iso2 = "SE", time = as.Date(paste0(2021:2024, "-01-01")),
    values = c(120, 120, 120, 999)
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )
  result <- fill_raw_coal_monthly(bind_rows(supply, current_supply, gid), annual)
  filled <- result %>% filter(nrg_bal == "GID_CAL", time == as.Date("2024-01-01"))
  method <- attr(result, "coal_gap_provenance") %>%
    filter(nrg_bal == "GID_CAL", time == as.Date("2024-01-01")) %>%
    pull(method)

  expect_equal(filled$values, 20)
  expect_equal(method, "accounting")

  current_supply$values[current_supply$nrg_bal == "IMP"] <- NA
  unavailable <- fill_raw_coal_monthly(bind_rows(supply, current_supply, gid), annual)
  unavailable_method <- attr(unavailable, "coal_gap_provenance") %>%
    filter(nrg_bal == "GID_CAL", time == as.Date("2024-01-01")) %>%
    pull(method)
  expect_equal(unavailable_method, "unresolved")
})

test_that("solid sector splitting preserves usable electricity and requires dependencies", {
  input <- tibble::tribble(
    ~iso2, ~time, ~unit, ~siec, ~fuel, ~sector, ~values,
    "SE", as.Date("2024-01-01"), "THS_T", SIEC_HARD_COAL, FUEL_COAL, SECTOR_ALL, 10,
    "SE", as.Date("2024-02-01"), "THS_T", SIEC_HARD_COAL, FUEL_COAL, SECTOR_ELEC, 4,
    "SE", as.Date("2024-03-01"), "THS_T", SIEC_HARD_COAL, FUEL_COAL, SECTOR_ALL, 10,
    "SE", as.Date("2024-03-01"), "THS_T", SIEC_HARD_COAL, FUEL_COAL, SECTOR_ELEC, 4
  )
  result <- eurostat_split_solid_elec_others(input)

  expect_equal(nrow(filter(result, time == as.Date("2024-01-01"))), 0)
  expect_equal(
    filter(result, time == as.Date("2024-02-01"), sector == SECTOR_ELEC)$values,
    4
  )
  expect_equal(
    filter(result, time == as.Date("2024-03-01"), sector == SECTOR_OTHERS)$values,
    6
  )
})

test_that("strict dependency splitting does not remove coke or peat", {
  input <- tibble::tribble(
    ~iso2, ~time, ~unit, ~siec, ~fuel, ~sector, ~values,
    "DE", as.Date("2024-01-01"), "THS_T", SIEC_COKE_OVEN_COKE,
    FUEL_COKE, SECTOR_ALL, 10,
    "FI", as.Date("2024-01-01"), "THS_T", SIEC_PEAT,
    FUEL_PEAT, SECTOR_ALL, 5
  )

  result <- eurostat_split_solid_elec_others(input)

  expect_equal(nrow(result), 4)
  expect_equal(sum(result$values), 15)
  expect_equal(
    filter(result, sector == SECTOR_OTHERS) %>% arrange(siec) %>% pull(values),
    c(10, 5)
  )
})

test_that("missing coking input blocks only the affected total", {
  input <- tibble::tribble(
    ~iso2, ~time, ~siec, ~nrg_bal, ~unit, ~values,
    "SE", as.Date("2024-01-01"), SIEC_HARD_COAL, "GID_CAL", "THS_T", 10,
    "SE", as.Date("2024-01-01"), SIEC_HARD_COAL, "TI_EHG_MAP", "THS_T", 4,
    "SE", as.Date("2024-01-01"), SIEC_HARD_COAL, "TI_CO", "THS_T", NA_real_,
    "SE", as.Date("2024-01-01"), SIEC_BROWN_COAL, "GID_CAL", "THS_T", 20,
    "SE", as.Date("2024-01-01"), SIEC_BROWN_COAL, "TI_EHG_MAP", "THS_T", 15
  )
  local_mocked_bindings(
    fill_eu_from_countries_sum = function(data, ...) data,
    .package = "creaco2tracker"
  )

  result <- process_solid_monthly(input, pwr_generation = tibble()) %>%
    eurostat_split_solid_elec_others()

  expect_equal(
    filter(result, siec == SIEC_HARD_COAL, sector == SECTOR_ELEC)$values,
    4
  )
  expect_equal(nrow(filter(result, siec == SIEC_HARD_COAL, sector == SECTOR_OTHERS)), 0)
  expect_equal(
    filter(result, siec == SIEC_BROWN_COAL, sector == SECTOR_OTHERS)$values,
    5
  )
})

test_that("collect_solid is offline-testable with vendored source fixtures", {
  fixture <- function(name) {
    readr::read_csv(test_path("fixtures", name), show_col_types = FALSE) %>%
      mutate(time = as.Date(time))
  }
  local_mocked_bindings(
    get_eurostat_from_code = function(code, ...) {
      if (code == "nrg_cb_sffm") fixture("eurostat_coal_monthly.csv") else
        fixture("eurostat_coal_annual.csv")
    },
    add_iso2 = function(data, country_col = "geo") {
      if (!"iso2" %in% names(data)) data$iso2 <- data[[country_col]]
      data$iso2[data$iso2 == "EL"] <- "GR"
      data
    },
    apply_source_data_mask = function(data, ...) data,
    get_eu_iso2s = function(include_eu = FALSE) c("SE", "GR", "EE"),
    .package = "creaco2tracker"
  )

  result <- collect_solid(use_cache = FALSE)
  sweden <- result$monthly %>%
    filter(iso2 == "SE", siec == SIEC_HARD_COAL, nrg_bal == "GID_CAL") %>%
    arrange(time)

  expect_equal(sweden$values, c(10, 20, 30, 40, 50, 60))
  expect_false(any(result$monthly$siec == SIEC_BROWN_COAL_BRIQUETTES))
  completeness <- attr(result$monthly, "coal_gap_completeness")
  expect_true(all(completeness$expected_countries == 3))
  expect_true(any(!completeness$complete))
})

test_that("coal diagnostics are written separately from production output", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-03-01"), by = "month")
  monthly <- coal_rows("SE", SIEC_HARD_COAL, "GID_CAL", dates, c(10, NA, 30))
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "SE",
    .package = "creaco2tracker"
  )
  filled <- fill_raw_coal_monthly(monthly, tibble())
  folder <- file.path(withr::local_tempdir(), "coal")

  write_coal_gap_diagnostics(filled, folder)

  expect_true(all(file.exists(file.path(folder, c(
    "coal_gap_provenance.csv", "coal_gap_reconciliation.csv",
    "coal_gap_completeness.csv", "coal_gap_exclusions.csv"
  )))))
})

test_that("annual reconciliation handles zero denominators explicitly", {
  months <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  monthly <- tibble(
    iso2 = "SE", sector = SECTOR_OTHERS, time = months, unit = "THS_T",
    siec = SIEC_HARD_COAL, fuel = FUEL_COAL, values = 1, source = "monthly"
  )
  annual <- tibble(
    iso2 = "SE", sector = SECTOR_OTHERS, time = as.Date("2024-01-01"),
    unit = "THS_T", siec = SIEC_HARD_COAL, fuel = FUEL_COAL, values = 0
  )
  result <- check_coal_annual_bounds(monthly, annual, diagnostics_folder = NULL)

  expect_true(result$zero_denominator)
  expect_true(is.na(result$relative_difference))
  expect_false(result$within_bounds)
})

test_that("process_solid_yearly keeps NA when all electricity components are missing", {
  yearly_raw <- tibble::tribble(
    ~iso2, ~time, ~siec, ~nrg_bal, ~unit, ~values,
    "GR", as.Date("2025-01-01"), SIEC_BROWN_COAL, "TI_EHG_MAPE_E", "TJ", NA_real_,
    "GR", as.Date("2025-01-01"), SIEC_BROWN_COAL, "TI_EHG_MAPCHP_E", "TJ", NA_real_
  )
  out <- process_solid_yearly(yearly_raw)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$values[[1]]))
})
