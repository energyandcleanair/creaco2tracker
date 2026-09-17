library(testthat)
library(dplyr)

coal_rows <- function(iso2, siec, nrg_bal, dates, values, unit = "THS_T") {
  tibble(
    freq = "M", nrg_bal = nrg_bal, siec = siec, unit = unit,
    geo = iso2, iso2 = iso2, time = as.Date(dates), values = values
  )
}

coal_annual_rows <- function(iso2, siec, nrg_bal, years, values, unit = "THS_T") {
  tibble(
    freq = "A", nrg_bal = nrg_bal, siec = siec, unit = unit,
    geo = iso2, iso2 = iso2, time = as.Date(paste0(years, "-01-01")),
    values = values
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

  january <- filter(result, time == as.Date("2024-01-01"))
  expect_equal(nrow(january), 1)
  expect_equal(january$sector, SECTOR_UNKNOWN)
  expect_equal(january$values, 10)
  expect_equal(
    filter(result, time == as.Date("2024-02-01"), sector == SECTOR_ELEC)$values,
    4
  )
  expect_true(is.na(
    filter(result, time == as.Date("2024-02-01"), sector == SECTOR_OTHERS)$values
  ))
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
  expect_true(is.na(
    filter(result, siec == SIEC_HARD_COAL, sector == SECTOR_OTHERS)$values
  ))
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
  expect_equal(nrow(out), 2)
  expect_true(all(is.na(out$values)))
})

test_that("annual-backed rules recover a missing lignite year and its power split", {
  history_dates <- seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month")
  target_dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
  monthly <- bind_rows(
    coal_rows("GR", SIEC_BROWN_COAL, "GID_CAL", history_dates, rep(100, 36)),
    coal_rows("GR", SIEC_BROWN_COAL, "GID_CAL", target_dates, rep(NA_real_, 12)),
    coal_rows("GR", SIEC_BROWN_COAL, "TI_EHG_MAP", history_dates, rep(0, 36)),
    coal_rows("GR", SIEC_BROWN_COAL, "TI_EHG_MAP", target_dates, rep(0, 12))
  )
  annual <- bind_rows(
    coal_annual_rows("GR", SIEC_BROWN_COAL, "IC_CAL", 2022:2025, rep(1200, 4)),
    coal_annual_rows("GR", SIEC_BROWN_COAL, "TI_E", 2022:2025, rep(1188, 4)),
    coal_annual_rows(
      "GR", SIEC_BROWN_COAL, "TI_EHG_MAPE_E", 2022:2024, rep(600, 3)
    ),
    coal_annual_rows(
      "GR", SIEC_BROWN_COAL, "TI_EHG_MAPCHP_E", 2022:2024, rep(588, 3)
    )
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_annual_backed(monthly, annual)
  target <- result %>% filter(lubridate::year(time) == 2025)
  provenance <- attr(result, "coal_annual_provenance") %>%
    filter(lubridate::year(time) == 2025)

  expect_equal(sum(filter(target, nrg_bal == "GID_CAL")$values), 1200)
  expect_equal(sum(filter(target, nrg_bal == "TI_EHG_MAP")$values), 1188)
  expect_true(all(
    filter(provenance, nrg_bal == "TI_EHG_MAP")$component_status ==
      "reported_inconsistent"
  ))
  expect_true(all(
    filter(provenance, nrg_bal == "TI_EHG_MAP")$annual_method ==
      "transformation_identity"
  ))
})

test_that("failed annual coking policy leaves missing values explicit", {
  dates <- seq(as.Date("2022-01-01"), as.Date("2025-12-01"), by = "month")
  monthly <- bind_rows(
    coal_rows("FR", SIEC_HARD_COAL, "GID_CAL", dates, rep(100, 48)),
    coal_rows("FR", SIEC_HARD_COAL, "TI_EHG_MAP", dates, rep(10, 48)),
    coal_rows(
      "FR", SIEC_HARD_COAL, "TI_CO", dates,
      c(rep(0, 36), rep(NA_real_, 12))
    )
  )
  annual <- bind_rows(
    coal_annual_rows("FR", SIEC_HARD_COAL, "IC_CAL", 2022:2025, rep(1200, 4)),
    coal_annual_rows("FR", SIEC_HARD_COAL, "TI_CO_E", 2022:2024, rep(0, 3))
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "FR",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_annual_backed(monthly, annual)
  coking <- result %>%
    filter(nrg_bal == "TI_CO", lubridate::year(time) == 2025)
  provenance <- attr(result, "coal_annual_provenance")

  expect_true(all(is.na(coking$values)))
  expect_equal(nrow(provenance), 0)
  expect_false(.coal_annual_policy_enabled(SIEC_HARD_COAL, "TI_CO", 12))
  expect_false(.coal_annual_policy_enabled(
    SIEC_BROWN_COAL_BRIQUETTES, "GID_CAL", 6
  ))
  expect_false(.coal_annual_policy_enabled(SIEC_OIL_SHALE, "GID_CAL", 12))
  expect_true(.coal_annual_policy_enabled(SIEC_OIL_SHALE, "GID_CAL", 6))
})

test_that("reported zero transformation bounds briquette power use at zero", {
  dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
  monthly <- bind_rows(
    coal_rows(
      "AT", SIEC_BROWN_COAL_BRIQUETTES, "GID_CAL", dates, rep(10, 12)
    ),
    coal_rows(
      "AT", SIEC_BROWN_COAL_BRIQUETTES, "TI_EHG_MAP", dates,
      rep(NA_real_, 12)
    )
  )
  annual <- bind_rows(
    coal_annual_rows(
      "AT", SIEC_BROWN_COAL_BRIQUETTES, "IC_CAL", 2025, 120
    ),
    coal_annual_rows(
      "AT", SIEC_BROWN_COAL_BRIQUETTES, "TI_E", 2025, 0
    )
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "AT",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_annual_backed(monthly, annual)
  power <- result %>% filter(nrg_bal == "TI_EHG_MAP")
  provenance <- attr(result, "coal_annual_provenance") %>%
    filter(nrg_bal == "TI_EHG_MAP")

  expect_equal(power$values, rep(0, 12))
  expect_true(all(
    provenance$annual_method == "reported_zero_transformation_bound"
  ))
})

test_that("complete monthly coal sectors replace an annual unallocated total", {
  input <- tibble(
    iso2 = "DE",
    time = as.Date("2025-01-01"),
    unit = "THS_T",
    siec = SIEC_BROWN_COAL_BRIQUETTES,
    fuel = FUEL_COAL,
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS),
    values = c(100, 20, 70),
    source = c("yearly", "monthly", "monthly")
  )

  result <- resolve_coal_unallocated_totals(input)
  diagnostics <- attr(result, "coal_unallocated_sector")

  expect_false(SECTOR_UNKNOWN %in% result$sector)
  expect_equal(sum(result$values), 90)
  expect_equal(diagnostics$status, "monthly_split_complete")
})

test_that("a monthly unallocated coal total replaces annual sector fallbacks", {
  input <- tibble(
    iso2 = "EU",
    time = as.Date("2024-01-01"),
    unit = "THS_T",
    siec = SIEC_HARD_COAL,
    fuel = FUEL_COAL,
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS),
    values = c(100, 40, 30),
    source = c("monthly", "yearly", "yearly")
  )

  result <- resolve_coal_unallocated_totals(input)
  diagnostics <- attr(result, "coal_unallocated_sector")

  expect_equal(nrow(result), 1)
  expect_equal(result$sector, SECTOR_UNKNOWN)
  expect_equal(result$values, 100)
  expect_equal(diagnostics$status, "monthly_total_unallocated")
})

test_that("coal unallocated reconciliation preserves non-coal rows", {
  input <- tibble(
    iso2 = "DE",
    time = as.Date("2025-01-01"),
    unit = "TJ_GCV",
    siec = SIEC_NATURAL_GAS,
    fuel = FUEL_GAS,
    sector = c(SECTOR_ALL, SECTOR_ELEC),
    values = c(100, 20),
    source = "monthly"
  )

  result <- resolve_coal_unallocated_totals(input)
  attr(result, "coal_unallocated_sector") <- NULL

  expect_equal(result, input)
})

test_that("annual coal total preserves only the uncovered monthly residual", {
  input <- tibble(
    iso2 = "DE",
    time = as.Date("2025-01-01"),
    unit = "THS_T",
    siec = SIEC_BROWN_COAL_BRIQUETTES,
    fuel = FUEL_COAL,
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS),
    values = c(100, 20, NA_real_),
    source = c("yearly", "monthly", "monthly")
  )

  result <- resolve_coal_unallocated_totals(input)
  diagnostics <- attr(result, "coal_unallocated_sector")

  expect_equal(result$values[result$sector == SECTOR_UNKNOWN], 80)
  expect_false(SECTOR_OTHERS %in% result$sector)
  expect_equal(sum(result$values), 100)
  expect_equal(diagnostics$status, "annual_residual_unallocated")
})

test_that("an inconsistent annual coal residual remains explicitly unresolved", {
  input <- tibble(
    iso2 = "DE",
    time = as.Date("2025-01-01"),
    unit = "THS_T",
    siec = SIEC_BROWN_COAL_BRIQUETTES,
    fuel = FUEL_COAL,
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS),
    values = c(10, 20, NA_real_),
    source = c("yearly", "monthly", "monthly")
  )

  result <- resolve_coal_unallocated_totals(input)
  diagnostics <- attr(result, "coal_unallocated_sector")

  expect_true(is.na(result$values[result$sector == SECTOR_UNKNOWN]))
  expect_equal(diagnostics$status, "negative_residual")
})

test_that("yearly coal fallback is explicit and unresolved values stay missing", {
  monthly <- tibble::tribble(
    ~iso2, ~sector, ~time, ~unit, ~siec, ~fuel, ~values,
    "SE", SECTOR_ELEC, as.Date("2025-01-01"), "THS_T", SIEC_HARD_COAL,
    FUEL_COAL, NA_real_,
    "SE", SECTOR_OTHERS, as.Date("2025-01-01"), "THS_T", SIEC_HARD_COAL,
    FUEL_COAL, NA_real_
  )
  yearly <- monthly %>%
    filter(sector == SECTOR_ELEC) %>%
    mutate(values = 5)

  combined <- combine_monthly_yearly_with_cutoff(yearly, monthly)
  diagnostic <- coal_downstream_completeness(monthly, combined)

  expect_equal(filter(combined, sector == SECTOR_ELEC)$values, 5)
  expect_equal(
    filter(diagnostic, sector == SECTOR_ELEC)$status,
    "yearly_fallback"
  )
  expect_true(is.na(filter(combined, sector == SECTOR_OTHERS)$values))
  expect_equal(filter(diagnostic, sector == SECTOR_OTHERS)$status, "unresolved")
})

test_that("verified EU omission repairs are guarded against duplication", {
  candidates <- tibble(
    contributor_iso2 = "GR", siec = SIEC_BROWN_COAL, nrg_bal = "GID_CAL",
    unit = "THS_T", time = as.Date("2025-01-01"), contribution = 10,
    eu_value = 100, reported_country_sum = 100, source_difference = 0,
    tolerance = 0.001, verified = TRUE
  ) %>%
    bind_rows(., .)
  converted <- tibble::tribble(
    ~iso2, ~siec, ~time, ~unit, ~fuel, ~sector, ~value_co2_tonne,
    "GR", SIEC_BROWN_COAL, as.Date("2025-01-01"), "THS_T", FUEL_COAL,
    SECTOR_ELEC, 20,
    "EU", SIEC_BROWN_COAL, as.Date("2025-01-01"), "THS_T", FUEL_COAL,
    SECTOR_ELEC, 100
  )

  once <- apply_verified_coal_eu_repairs(
    converted %>% group_by(siec, iso2),
    candidates
  )
  twice <- apply_verified_coal_eu_repairs(once, candidates)

  expect_equal(filter(once, iso2 == "EU")$value_co2_tonne, 120)
  expect_equal(filter(twice, iso2 == "EU")$value_co2_tonne, 120)
})

test_that("missing coal components retain visible partial aggregates with diagnostics", {
  components <- tidyr::crossing(
    iso2 = "SE", date = as.Date("2025-01-01"), unit = "t",
    sector = SECTOR_ALL, estimate = c("central", "lower", "upper")
  ) %>%
    tidyr::crossing(fuel = c(FUEL_COAL, FUEL_GAS)) %>%
    mutate(value = if_else(fuel == FUEL_COAL, NA_real_, 10))

  totals <- add_total_co2(components) %>%
    filter(fuel == "total")
  recombined <- tibble(
    iso2 = "SE", date = as.Date("2025-01-01"), unit = "t",
    sector = SECTOR_ALL, estimate = "central",
    fuel = c(FUEL_COAL, FUEL_PEAT), value = c(NA_real_, 5)
  ) %>%
    recombine_fuels()

  expect_equal(totals$value[totals$estimate == "central"], 10)
  expect_false(attr(totals, "total_component_completeness")$central_complete)
  expect_false(any(c(
    "central_component_count", "central_available_components", "central_complete"
  ) %in% names(totals)))
  expect_equal(recombined$value, 5)
  expect_false(attr(recombined, "fuel_completeness")$complete)
})
