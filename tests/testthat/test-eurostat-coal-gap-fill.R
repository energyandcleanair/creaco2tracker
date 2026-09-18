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

greece_current_year_coal_fixture <- function(
  explicit_future = FALSE,
  missing_consumption_month = NA_integer_,
  extend_to_2027 = FALSE
) {
  history_years <- 2023:2025
  current_dates <- seq(as.Date("2026-01-01"), as.Date("2026-06-01"), by = "month")
  current_consumption <- c(100, 110, 120, 130, 140, 150)
  if (!is.na(missing_consumption_month)) {
    current_consumption[missing_consumption_month] <- NA_real_
  }
  monthly <- bind_rows(
    coal_rows("GR", SIEC_BROWN_COAL, "GID_CAL", current_dates, current_consumption),
    coal_rows("GR", SIEC_BROWN_COAL, "TI_EHG_MAP", current_dates, rep(0, 6))
  )
  if (explicit_future) {
    future_dates <- seq(as.Date("2026-07-01"), as.Date("2026-12-01"), by = "month")
    monthly <- bind_rows(
      monthly,
      coal_rows("GR", SIEC_BROWN_COAL, "GID_CAL", future_dates, rep(NA_real_, 6)),
      coal_rows("GR", SIEC_BROWN_COAL, "TI_EHG_MAP", future_dates, rep(NA_real_, 6))
    )
  }
  if (extend_to_2027) {
    later_dates <- seq(as.Date("2027-01-01"), as.Date("2027-06-01"), by = "month")
    monthly <- bind_rows(
      monthly,
      coal_rows("GR", SIEC_BROWN_COAL, "GID_CAL", later_dates, rep(80, 6)),
      coal_rows("GR", SIEC_BROWN_COAL, "TI_EHG_MAP", later_dates, rep(70, 6))
    )
  }
  annual <- bind_rows(
    coal_annual_rows("GR", SIEC_BROWN_COAL, "IC_CAL", history_years, rep(1200, 3)),
    coal_annual_rows("GR", SIEC_BROWN_COAL, "TI_E", history_years, rep(1080, 3)),
    coal_annual_rows(
      "GR", SIEC_BROWN_COAL, "TI_EHG_MAPE_E", history_years, rep(540, 3)
    ),
    coal_annual_rows(
      "GR", SIEC_BROWN_COAL, "TI_EHG_MAPCHP_E", history_years, rep(540, 3)
    )
  )
  list(monthly = monthly, annual = annual, expected = current_consumption * 0.9)
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

test_that("strict dependency splitting keeps unallocated coke and split peat", {
  input <- tibble::tribble(
    ~iso2, ~time, ~unit, ~siec, ~fuel, ~sector, ~values,
    "DE", as.Date("2024-01-01"), "THS_T", SIEC_COKE_OVEN_COKE,
    FUEL_COKE, SECTOR_ALL, 10,
    "FI", as.Date("2024-01-01"), "THS_T", SIEC_PEAT,
    FUEL_PEAT, SECTOR_ALL, 5
  )

  result <- eurostat_split_solid_elec_others(input)

  expect_equal(nrow(result), 3)
  expect_equal(sum(result$values), 15)
  expect_equal(filter(result, siec == SIEC_COKE_OVEN_COKE)$sector, SECTOR_UNKNOWN)
  expect_equal(filter(result, sector == SECTOR_OTHERS) %>% pull(values), 5)
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

test_that("current-year Greek lignite power is repaired through the country cutoff", {
  fixture <- greece_current_year_coal_fixture()
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_annual_backed(fixture$monthly, fixture$annual)
  current_power <- result %>%
    filter(nrg_bal == "TI_EHG_MAP", lubridate::year(time) == 2026) %>%
    arrange(time)
  provenance <- attr(result, "coal_annual_provenance") %>%
    filter(nrg_bal == "TI_EHG_MAP", lubridate::year(time) == 2026) %>%
    arrange(time)
  split <- result %>%
    filter(
      lubridate::year(time) == 2026,
      nrg_bal %in% c("GID_CAL", "TI_EHG_MAP")
    ) %>%
    mutate(
      fuel = FUEL_COAL,
      sector = if_else(nrg_bal == "TI_EHG_MAP", SECTOR_ELEC, SECTOR_ALL)
    ) %>%
    select(iso2, time, unit, siec, fuel, sector, values) %>%
    eurostat_split_solid_elec_others()

  expect_equal(current_power$time, seq(as.Date("2026-01-01"), by = "month", length.out = 6))
  expect_equal(current_power$values, fixture$expected)
  expect_equal(provenance$original_value, rep(0, 6))
  expect_equal(provenance$component_status, rep("reported_inconsistent", 6))
  expect_equal(provenance$annual_method, rep("previous_year_share_unconstrained", 6))
  expect_equal(provenance$annual_input_years, rep("2023,2024,2025", 6))
  expect_equal(provenance$profile_method, rep("same_month_reported_consumption", 6))
  expect_true(all(current_power$time <= as.Date("2026-06-01")))
  expect_equal(
    split %>% group_by(time) %>% summarise(value = sum(values), .groups = "drop") %>%
      pull(value),
    c(100, 110, 120, 130, 140, 150)
  )
  expect_equal(
    split %>% filter(sector == SECTOR_OTHERS) %>% arrange(time) %>% pull(values),
    c(100, 110, 120, 130, 140, 150) - fixture$expected
  )
})

test_that("current-year coal repair is independent of future row coverage", {
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )
  variants <- list(
    absent = greece_current_year_coal_fixture(),
    explicit = greece_current_year_coal_fixture(explicit_future = TRUE),
    extended = greece_current_year_coal_fixture(extend_to_2027 = TRUE)
  )
  repaired <- lapply(variants, function(fixture) {
    result <- fill_raw_coal_annual_backed(fixture$monthly, fixture$annual)
    list(
      values = result %>%
        filter(nrg_bal == "TI_EHG_MAP", time >= as.Date("2026-01-01"),
          time <= as.Date("2026-06-01")) %>%
        arrange(time) %>% pull(values),
      provenance = attr(result, "coal_annual_provenance") %>%
        filter(nrg_bal == "TI_EHG_MAP", time >= as.Date("2026-01-01"),
          time <= as.Date("2026-06-01")) %>%
        arrange(time) %>%
        select(time, original_value, filled_value, component_status, annual_method,
          annual_input_years, profile_method, profile_input_years)
    )
  })

  expect_equal(repaired$explicit, repaired$absent)
  expect_equal(repaired$extended, repaired$absent)
})

test_that("current-year coal repair skips only months without consumption", {
  fixture <- greece_current_year_coal_fixture(missing_consumption_month = 3)
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )

  result <- fill_raw_coal_annual_backed(fixture$monthly, fixture$annual)
  power <- result %>%
    filter(nrg_bal == "TI_EHG_MAP", lubridate::year(time) == 2026) %>%
    arrange(time)

  expect_equal(power$values, c(fixture$expected[1:2], 0, fixture$expected[4:6]))
  expect_equal(
    attr(result, "coal_annual_provenance") %>%
      filter(nrg_bal == "TI_EHG_MAP", lubridate::year(time) == 2026) %>%
      pull(time),
    power$time[-3]
  )
  repeated <- fill_raw_coal_annual_backed(result, fixture$annual)
  expect_equal(
    repeated %>% filter(nrg_bal == "TI_EHG_MAP", lubridate::year(time) == 2026) %>%
      arrange(time) %>% pull(values),
    power$values
  )
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

test_that("a monthly coal total retains scaled annual sector fallbacks", {
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

  expect_equal(nrow(result), 2)
  expect_equal(result$sector, c(SECTOR_ELEC, SECTOR_OTHERS))
  expect_equal(result$values, 100 * c(40, 30) / 70)
  expect_equal(diagnostics$status, "annual_split_scaled_to_monthly_total")
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

  expect_equal(result$values[result$sector == SECTOR_OTHERS], 80)
  expect_false(SECTOR_UNKNOWN %in% result$sector)
  expect_equal(sum(result$values), 100)
  expect_equal(diagnostics$status, "annual_residual_allocated")
})

test_that("an inconsistent annual coal split is bounded to its valid total", {
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

  expect_equal(result$values[result$sector == SECTOR_ELEC], 10)
  expect_equal(result$values[result$sector == SECTOR_OTHERS], 0)
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

test_that("EU power corrections require an exact match to the reported country sum", {
  original <- tibble::tibble(
    iso2 = c("GR", "EU"), siec = SIEC_BROWN_COAL, nrg_bal = "TI_EHG_MAP",
    unit = "THS_T", time = as.Date("2026-01-01"), values = 0
  )
  provenance <- tibble::tibble(
    iso2 = "GR", siec = SIEC_BROWN_COAL, nrg_bal = "TI_EHG_MAP",
    unit = "THS_T", time = as.Date("2026-01-01"), original_value = 0,
    filled_value = 10, component_status = "reported_inconsistent"
  )
  local_mocked_bindings(
    get_eu_iso2s = function(include_eu = FALSE) "GR",
    .package = "creaco2tracker"
  )

  verified <- .coal_detect_eu_omissions(original, original, provenance)
  mismatched <- .coal_detect_eu_omissions(
    original %>% mutate(values = if_else(iso2 == "EU", 1, values)),
    original,
    provenance
  )
  absent <- .coal_detect_eu_omissions(
    original %>% filter(iso2 != "EU"),
    original,
    provenance
  )

  expect_true(verified$verified)
  expect_equal(verified$contribution, 10)
  expect_equal(verified$repair_sector, SECTOR_ELEC)
  expect_false(mismatched$verified)
  expect_false(absent$verified)
})

test_that("verified EU power corrections preserve total by reducing others", {
  candidates <- tibble::tibble(
    contributor_iso2 = "GR", siec = SIEC_BROWN_COAL, nrg_bal = "TI_EHG_MAP",
    unit = "THS_T", time = as.Date("2026-01-01"), contribution = 10,
    repair_sector = SECTOR_ELEC, eu_value = 100, reported_country_sum = 100,
    source_difference = 0, tolerance = 0.001, verified = TRUE
  )
  converted <- tidyr::crossing(
    iso2 = c("GR", "EU"),
    sector = c(SECTOR_ELEC, SECTOR_OTHERS)
  ) %>%
    mutate(
      siec = SIEC_BROWN_COAL, time = as.Date("2026-01-01"), unit = "THS_T",
      fuel = FUEL_COAL,
      value_co2_tonne = case_when(
        iso2 == "GR" & sector == SECTOR_ELEC ~ 20,
        iso2 == "GR" ~ 5,
        iso2 == "EU" & sector == SECTOR_ELEC ~ 100,
        TRUE ~ 40
      )
    )

  result <- apply_verified_coal_eu_repairs(converted, candidates)
  repeated <- apply_verified_coal_eu_repairs(result, candidates)

  expect_equal(
    result %>% filter(iso2 == "EU", sector == SECTOR_ELEC) %>% pull(value_co2_tonne),
    120
  )
  expect_equal(
    result %>% filter(iso2 == "EU", sector == SECTOR_OTHERS) %>% pull(value_co2_tonne),
    20
  )
  expect_equal(sum(filter(result, iso2 == "EU")$value_co2_tonne), 140)
  expect_equal(repeated$value_co2_tonne, result$value_co2_tonne)
})

test_that("EU power corrections require both sector rows", {
  candidates <- tibble::tibble(
    contributor_iso2 = "GR", siec = SIEC_BROWN_COAL, nrg_bal = "TI_EHG_MAP",
    unit = "THS_T", time = as.Date("2026-01-01"), contribution = 10,
    repair_sector = SECTOR_ELEC, eu_value = 100, reported_country_sum = 100,
    source_difference = 0, tolerance = 0.001, verified = TRUE
  )
  converted <- tibble::tribble(
    ~iso2, ~sector, ~siec, ~time, ~unit, ~fuel, ~value_co2_tonne,
    "GR", SECTOR_ELEC, SIEC_BROWN_COAL, as.Date("2026-01-01"), "THS_T", FUEL_COAL, 20,
    "EU", SECTOR_ELEC, SIEC_BROWN_COAL, as.Date("2026-01-01"), "THS_T", FUEL_COAL, 100
  )

  result <- apply_verified_coal_eu_repairs(converted, candidates)

  expect_equal(filter(result, iso2 == "EU")$value_co2_tonne, 100)
  expect_false(attr(result, "coal_eu_emissions_repairs")$applied)
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

test_that("monthly coal totals reject incomplete or invalid annual splits", {
  x <- tibble(iso2 = "DE", time = as.Date("2025-01-01"), unit = "THS_T",
    siec = SIEC_BROWN_COAL_BRIQUETTES, fuel = FUEL_COAL,
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS),
    values = c(100, 40, NA_real_), source = c("monthly", "yearly", "yearly"))
  for (invalid in c(NA_real_, -1, Inf)) {
    x$values[3] <- invalid
    result <- resolve_coal_unallocated_totals(x)
    expect_equal(result$sector, SECTOR_UNKNOWN)
    expect_equal(result$values, 100)
  }
})
