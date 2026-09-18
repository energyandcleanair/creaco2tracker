coke_rows <- function(dates, values, balance = "GID_CAL") {
  tibble::tibble(iso2 = "SE", time = as.Date(dates), nrg_bal = balance,
    values = values, siec = SIEC_COKE_OVEN_COKE, unit = "THS_T")
}

sweden_coke_fixture <- function(absent = FALSE) {
  dates <- seq(as.Date("2014-01-01"), as.Date("2026-06-01"), by = "month")
  coke <- c(106,91,113,106,116,99,76,99,108,119,121,128,126,129,148,139,133,73,76,
    71,124,130,89,103,103,105,113,102,109,102,111,100,94,96,97,132,
    103.486,114.468,117.004,135.351,116.717,99.375,108.412,91.91,123.175,
    108.907,122.371,114.305,117,88,116,103,120,98,102,123,66,112,71,132,130,130,
    159,99,135,95,134,137,109,93,80,120,119,98,120,107,119,93,55,72,112,108,108,
    104,109,107,108,115,112,110,101,62,113,119,105,114,77,112,113,130,100,104,152,
    76,82,128,88,96,105,101,98,110,106,114,113,92,83,91,103,89,122,111,114,116,115,
    95,103,60,95,80,105,108,125,90,94,98,112,105,88,102,85,92,110,120,
    NA,NA,NA,NA,NA,117)
  activity <- c(83.5,83.8,90.7,86.1,84.7,90.7,65,71.4,84.5,85.7,82.3,69.2,87,87.5,
    101.3,103.8,105.5,100.9,83.1,88,107.7,113.2,102.8,87.3,106.6,106.5,113.6,
    109.7,108.9,117,88.5,92.5,115.9,116.9,115.2,96.9,
    111.5,101.7,119.2,110.1,116.7,111.5,85.8,92.1,116.1,116.5,117,
    98.1,106.9,111.4,118.1,115.2,118.6,112.6,91.6,92.7,106.4,116.7,111.1,97.6,
    110.6,107.1,114.4,111.5,115.3,109,88.2,93.3,102.3,105.6,96.7,83.8,106.5,
    105.2,113.4,98.9,94.2,98.2,67.1,78.5,101.9,106.3,104.9,95.8,102.6,107.1,
    123.1,109.6,107.5,106.5,82.6,77.4,98.6,98.2,98.6,84.4,91.7,94.2,109.9,99.5,
    108.7,106.6,76.3,81,100.9,100.3,99.3,93,99.5,96.5,104.7,104.9,97.7,93.2,72,
    79.3,91.2,95.9,84.9,87.6,88.2,82.9,106,87.6,89.1,90.4,71,68.7,91.9,89,88.6,
    76.6,83,86.8,96.2,88.2,87.9,91.7,64.2,70.7,84.7,84.8,80.4,78.4,80.6,81.2,
    92.9,86.2,84.6,88.8)
  monthly <- coke_rows(dates, coke)
  if (absent) monthly <- filter(monthly, !(time >= as.Date("2026-01-01") &
    time <= as.Date("2026-05-01")))
  industry <- tibble::tibble(iso2 = "SE", time = dates, nace_r2 = "C24",
    unit = "I21", s_adj = "CA", values = activity)
  years <- 2022:2024
  annual <- tidyr::crossing(time = as.Date(sprintf("%04d-01-01", years)),
    nrg_bal = c("TI_EHG_MAPE_E", "TI_EHG_MAPCHP_E")) %>%
    mutate(iso2 = "SE", siec = SIEC_COKE_OVEN_COKE, unit = "THS_T", values = 0)
  list(monthly = monthly, annual = annual, industry = industry)
}

test_that("Sweden absent and explicit coke gaps use the same validated estimate", {
  explicit <- sweden_coke_fixture(FALSE)
  absent <- sweden_coke_fixture(TRUE)
  resolved_explicit <- .resolve_coke_consumption(
    explicit$monthly, explicit$annual, explicit$industry)
  resolved_absent <- .resolve_coke_consumption(
    absent$monthly, absent$annual, absent$industry)
  dates <- seq(as.Date("2026-01-01"), by = "month", length.out = 5)
  e <- filter(resolved_explicit$diagnostics, time %in% dates)
  a <- filter(resolved_absent$diagnostics, time %in% dates)

  expect_equal(e$resolved_value, a$resolved_value)
  expect_true(all(e$method == "seasonal"))
  expect_equal(e$resolved_value, c(117, 101, 102, 108, 111), tolerance = 1)
  expect_true(all(e$electricity_method == "three_annual_zero_or_negligible_pairs"))
  expect_equal(filter(resolved_explicit$monthly, time == as.Date("2026-06-01"),
    nrg_bal == "GID_CAL")$values, 117)
})

test_that("coke estimates split with evidenced zero power and are stable", {
  fixture <- sweden_coke_fixture(TRUE)
  resolved <- .resolve_coke_consumption(fixture$monthly, fixture$annual, fixture$industry)
  again <- .resolve_coke_consumption(resolved$monthly, fixture$annual, fixture$industry)
  expect_equal(again$diagnostics, resolved$diagnostics)

  split <- suppressWarnings(process_solid_monthly(resolved$monthly, tibble())) %>%
    eurostat_split_solid_elec_others() %>%
    filter(time >= as.Date("2026-01-01"), time <= as.Date("2026-05-01"))
  expect_true(all(filter(split, sector == SECTOR_ELEC)$values == 0))
  expect_equal(filter(split, sector == SECTOR_OTHERS)$values,
    filter(resolved$diagnostics, time >= as.Date("2026-01-01"),
      time <= as.Date("2026-05-01"))$resolved_value)
})

test_that("later reporting years preserve historical coke gap reconstruction", {
  gap_dates <- seq(as.Date("2026-01-01"), as.Date("2026-05-01"), by = "month")
  later_dates <- seq(as.Date("2026-07-01"), as.Date("2027-06-01"), by = "month")
  for (absent in c(FALSE, TRUE)) {
    fixture <- sweden_coke_fixture(absent)
    baseline <- .resolve_coke_consumption(fixture$monthly, fixture$annual, fixture$industry)
    extended_monthly <- bind_rows(fixture$monthly, coke_rows(later_dates, 120))
    later_industry <- fixture$industry[rep(1, length(later_dates)), ] %>%
      mutate(time = later_dates, values = 90)
    extended <- .resolve_coke_consumption(extended_monthly, fixture$annual,
      bind_rows(fixture$industry, later_industry))
    before <- filter(baseline$diagnostics, time %in% gap_dates)
    after <- filter(extended$diagnostics, time %in% gap_dates)
    expect_true(all(before$eligible_gap & before$status == "estimated"))
    expect_equal(after, before)
    expect_equal(filter(extended$validation, target_start == min(gap_dates)),
      filter(baseline$validation, target_start == min(gap_dates)))
    expect_equal(filter(extended$monthly, time %in% gap_dates) %>% as.data.frame(),
      filter(baseline$monthly, time %in% gap_dates) %>% as.data.frame(), ignore_attr = TRUE)
    reported <- filter(extended$diagnostics, is.finite(original_value))
    expect_equal(reported$resolved_value, reported$original_value)
  }
})

test_that("long or unbounded coke gaps remain unresolved", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 14)
  monthly <- coke_rows(dates, c(rep(100, 3), rep(NA_real_, 7), rep(100, 4)))
  result <- .resolve_coke_consumption(monthly, monthly[0, ], tibble())
  expect_true(all(is.na(filter(result$diagnostics, eligible_gap == FALSE,
    method == "unresolved")$resolved_value)))
})

test_that("unbounded and duplicate-blocked coke gaps remain ineligible", {
  fixture <- sweden_coke_fixture()
  gap_dates <- seq(as.Date("2026-01-01"), as.Date("2026-05-01"), by = "month")
  trailing <- filter(fixture$monthly, time < as.Date("2026-06-01"))
  leading <- filter(fixture$monthly, time >= min(gap_dates))
  duplicate <- bind_rows(fixture$monthly,
    filter(fixture$monthly, time == as.Date("2026-06-01")))
  for (monthly in list(trailing, leading, duplicate)) {
    result <- .resolve_coke_consumption(monthly, fixture$annual, fixture$industry)
    gap <- filter(result$diagnostics, time %in% gap_dates)
    expect_equal(nrow(gap), 5L)
    expect_false(any(gap$eligible_gap))
    expect_true(all(is.na(gap$resolved_value)))
    expect_true(all(gap$status == "unresolved"))
  }
})

test_that("eligible coke gaps require enough historical validation evidence", {
  fixture <- sweden_coke_fixture()
  monthly <- filter(fixture$monthly, time >= as.Date("2025-01-01"))
  result <- .resolve_coke_consumption(monthly, fixture$annual, fixture$industry)
  gap <- filter(result$diagnostics, time >= as.Date("2026-01-01"),
    time <= as.Date("2026-05-01"))
  expect_true(all(gap$eligible_gap))
  expect_true(all(is.na(gap$resolved_value)))
  expect_true(all(gap$evidence == "fewer_than_three_matched_holdouts"))
})

test_that("coke totals without power evidence remain explicitly unallocated", {
  x <- tibble::tibble(iso2 = "SE", time = as.Date("2026-01-01"), unit = "THS_T",
    siec = SIEC_COKE_OVEN_COKE, fuel = FUEL_COKE, sector = SECTOR_ALL, values = 100)
  result <- eurostat_split_solid_elec_others(x)
  expect_equal(result$sector, SECTOR_UNKNOWN)
  expect_equal(result$values, 100)
})

test_that("negligible annual coke power use supports explicit zero allocation", {
  annual <- tidyr::crossing(time = as.Date(sprintf("%04d-01-01", 2022:2024)),
    nrg_bal = c("TI_EHG_MAPE_E", "TI_EHG_MAPCHP_E")) %>%
    mutate(iso2 = "EU", siec = SIEC_COKE_OVEN_COKE, unit = "THS_T",
      values = if_else(nrg_bal == "TI_EHG_MAPCHP_E", c(.004, .005, 0, .004, .005, 0), 0))
  expect_true(.coke_zero_power_evidence(annual, "EU", 2026))
})

test_that("complete monthly coke allocation replaces the annual unknown total", {
  x <- tibble::tibble(iso2 = "EU", time = as.Date("2025-01-01"), unit = "THS_T",
    siec = SIEC_COKE_OVEN_COKE, fuel = FUEL_COKE,
    sector = c(SECTOR_ELEC, SECTOR_OTHERS, SECTOR_UNKNOWN),
    values = c(0, 100, 101), source = c("monthly", "monthly", "yearly"))
  result <- resolve_coke_unallocated_totals(x)
  expect_setequal(result$sector, c(SECTOR_ELEC, SECTOR_OTHERS))
  expect_equal(sum(result$values), 100)
})
