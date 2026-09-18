coal_energy_fixture <- function() {
  readr::read_csv(test_path("fixtures", "coal-energy", "annual.csv"),
    show_col_types = FALSE) %>% mutate(iso2 = if_else(geo == "EL", "GR", geo))
}

test_that("annual-only energy is retained independently of inland consumption", {
  raw <- coal_energy_fixture() %>% filter(
    time == as.Date("2025-01-01"), siec == SIEC_BROWN_COAL_BRIQUETTES
  )
  annual <- process_solid_yearly(raw) %>% eurostat_split_solid_elec_others()
  result <- coal_allocate_annual(annual, annual[0, ])
  totals <- result %>% group_by(iso2) %>% summarise(value = sum(values), .groups = "drop")
  expect_equal(totals$value[totals$iso2 == "LU"], 3.745)
  expect_equal(totals$value[totals$iso2 == "PL"], 0.938)
  expect_equal(totals$value[totals$iso2 == "SK"], 0)
  expect_true(all(filter(result, iso2 == "BG")$sector == SECTOR_UNKNOWN))
  expect_true(all(is.finite(totals$value)))
  expect_equal(nrow(filter(result, iso2 == "BG")), 12)
  expect_true(all(attr(result, "coal_allocation")$allocation_method == "equal_months"))
})

test_that("annual energy requires coking only when transformation can apply", {
  x <- tibble(
    iso2 = "SE", siec = SIEC_HARD_COAL, unit = "THS_T",
    time = as.Date("2025-01-01"), nrg_bal = c("FC_E", "TI_E"), values = c(10, 0)
  )
  expect_equal(coal_annual_energy(x)$energy, 10)
  x$values[2] <- 5
  expect_true(is.na(coal_annual_energy(x)$energy))
  x <- bind_rows(x, mutate(x[1, ], nrg_bal = "TI_CO_E", values = 0))
  expect_equal(coal_annual_energy(x)$energy, 15)
  expect_true(is.na(coal_annual_energy(bind_rows(x, x[1, ]))$energy))
  x$values[x$nrg_bal == "TI_E"] <- -1
  expect_true(is.na(coal_annual_energy(x)$energy))
})

test_that("annual allocation preserves months and never borrows future profiles", {
  annual <- tibble(iso2 = "FR", siec = SIEC_BROWN_COAL, fuel = FUEL_COAL,
    sector = SECTOR_OTHERS, unit = "THS_T", time = as.Date("2025-01-01"), values = 120)
  monthly <- annual[rep(1, 13), ] %>% mutate(
    time = c(as.Date("2025-01-01"), seq(as.Date("2026-01-01"), by = "month", length.out = 12)),
    values = c(10, 1:12)
  )
  result <- coal_allocate_annual(annual, monthly)
  expect_equal(result$values, rep(10, 12))
  monthly$values[1] <- 121
  result <- coal_allocate_annual(annual, monthly)
  expect_equal(result$values[1], 121)
  expect_true(all(is.na(result$values[-1])))
  annual$values <- 0
  monthly <- annual[rep(1, 13), ] %>% mutate(
    time = c(seq(as.Date("2024-01-01"), by = "month", length.out = 12), annual$time),
    values = c(10, rep(0, 12))
  )
  expect_equal(coal_allocate_annual(annual, monthly)$values, rep(0, 12))
})

test_that("historical annual backfill retains the observed seasonal climatology", {
  annual <- tibble(iso2 = "FR", siec = SIEC_BROWN_COAL, fuel = FUEL_COAL,
    sector = SECTOR_OTHERS, unit = "THS_T", time = as.Date("2010-01-01"), values = 120)
  monthly <- annual[rep(1, 12), ] %>% mutate(
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    values = c(22, rep(98 / 11, 11))
  )

  result <- coal_allocate_annual(annual, monthly)

  expect_equal(result$values, monthly$values)
  expect_true(all(attr(result, "coal_allocation")$allocation_method ==
    "historical_reported_climatology"))
})

test_that("an explicit annual zero rejects a contradictory complete monthly series", {
  annual <- tibble(iso2 = "EE", siec = SIEC_BROWN_COAL, fuel = FUEL_COAL,
    sector = SECTOR_OTHERS, unit = "THS_T", time = as.Date("2016-01-01"), values = 0)
  monthly <- annual[rep(1, 12), ] %>% mutate(
    time = seq(as.Date("2016-01-01"), by = "month", length.out = 12),
    values = 1:12
  )

  result <- coal_allocate_annual(annual, monthly)

  expect_equal(result$values, rep(0, 12))
  expect_true(all(attr(result, "coal_allocation")$allocation_method ==
    "reported_zero_annual_bound"))
})

test_that("fuel total forecasts do not train on a changing unallocated residual", {
  x <- tibble(iso2 = "DE", siec = SIEC_BROWN_COAL_BRIQUETTES, fuel = FUEL_COAL,
    unit = "THS_T", sector = SECTOR_UNKNOWN,
    time = seq(as.Date("2025-01-01"), by = "month", length.out = 12), values = 10)
  known <- x[1, ] %>% mutate(time = as.Date("2026-01-01"), sector = SECTOR_ELEC, values = 4)
  result <- coal_prepare_total_forecasts(bind_rows(x, known), as.Date("2027-01-01"))
  jan <- result %>% filter(time == as.Date("2026-01-01"))
  expect_equal(sum(jan$values), 10)
  expect_equal(jan$values[jan$sector == SECTOR_OTHERS], 6)
  expect_true(all(is.na(filter(result, time == as.Date("2027-01-01"))$values)))
  known$values <- 11
  result <- coal_prepare_total_forecasts(bind_rows(x, known), as.Date("2026-01-01"))
  expect_equal(filter(result, time == as.Date("2026-01-01"),
    sector == SECTOR_ELEC)$values, 11)
  expect_equal(filter(result, time == as.Date("2026-01-01"),
    sector == SECTOR_OTHERS)$values, 0)
  interrupted <- x
  interrupted$values[6] <- NA_real_
  result <- coal_prepare_total_forecasts(interrupted, as.Date("2026-06-01"))
  expect_true(is.na(filter(result, time == as.Date("2025-06-01"))$values))
})

test_that("forecast conflicts raise totals while preserving either known sector", {
  history <- tibble(iso2 = "DE", siec = SIEC_BROWN_COAL_BRIQUETTES, fuel = FUEL_COAL,
    unit = "THS_T", sector = SECTOR_UNKNOWN,
    time = seq(as.Date("2023-01-01"), as.Date("2025-12-01"), by = "month"), values = 100)
  diagnostics_folder <- tempfile("coal-forecasts-")
  on.exit(unlink(diagnostics_folder, recursive = TRUE), add = TRUE)
  for (method in c("previous_year", "three_year_average")) {
    for (sector in c(SECTOR_ELEC, SECTOR_OTHERS)) {
      for (value in c(90, 100, 110)) {
        known <- history[1, ] %>% mutate(time = as.Date("2026-01-01"),
          sector = .env$sector, values = value)
        result <- coal_prepare_total_forecasts(bind_rows(history, known),
          as.Date("2027-01-01"), diagnostics_folder, forecast_method = method)
        current <- filter(result, time == known$time)
        expect_equal(current$values[current$sector == sector], value)
        expect_equal(current$values[current$sector != sector], max(100 - value, 0))
        expect_equal(sum(current$values), max(100, value))
        diagnostics <- attr(result, "coal_total_forecasts") %>% filter(time == known$time)
        expect_equal(diagnostics$conflict, value > 100)
        expect_equal(diagnostics$forecast_total, 100)
        expect_equal(diagnostics$forecast_residual, 100 - value)
        expect_equal(diagnostics$total, max(100, value))
        expect_equal(diagnostics$residual, max(100 - value, 0))
        expect_equal(diagnostics$method, paste0(method, "_total",
          if (value > 100) "_raised_to_known_sectors" else ""))
        # Neither ordinary nor revised forecasts become next year's history.
        expect_true(all(is.na(filter(result, time == as.Date("2027-01-01"))$values)))
        exported <- readr::read_csv(file.path(diagnostics_folder, "coal_total_forecasts.csv"),
          show_col_types = FALSE) %>% filter(time == known$time)
        expect_equal(exported, diagnostics, ignore_attr = TRUE)
      }
    }
  }
})

test_that("annual-only forecasting includes an established zero power component", {
  annual <- tibble(iso2 = "LU", siec = SIEC_BROWN_COAL_BRIQUETTES, fuel = FUEL_COAL,
    unit = "THS_T", sector = c(SECTOR_ELEC, SECTOR_OTHERS),
    time = as.Date("2025-01-01"), values = c(0, 12))
  monthly <- annual[rep(1, 12), ] %>%
    mutate(time = seq(as.Date("2025-01-01"), by = "month", length.out = 12))
  attr(monthly, "coal_reported_monthly") <- monthly[0, ]
  allocated <- coal_allocate_annual(annual, monthly)
  expect_true(any(attr(allocated, "coal_allocation")$allocation_method ==
    "preserved_monthly_estimate"))
  result <- coal_prepare_total_forecasts(allocated, as.Date("2026-06-01"))
  h1 <- result %>% filter(lubridate::year(time) == 2026)
  expect_equal(nrow(h1), 12)
  expect_true(all(h1$values[h1$sector == SECTOR_ELEC] == 0))
  expect_equal(sum(h1$values), 6)
  expect_true("LU" %in% attr(result, "coal_separate_projection")$iso2)
  annual$values[2] <- NA_real_
  unresolved <- coal_prepare_total_forecasts(
    coal_allocate_annual(annual, monthly), as.Date("2026-06-01")
  ) %>% filter(lubridate::year(time) == 2026)
  expect_equal(nrow(unresolved), 6)
  expect_true(all(is.na(unresolved$values)))
})

test_that("separate fuel totals survive an inconsistent derived sector split", {
  x <- tibble(iso2 = "IT", siec = SIEC_HARD_COAL, fuel = FUEL_COAL, unit = "THS_T",
    time = as.Date(c("2020-01-01", "2021-01-01", "2021-01-01")),
    sector = c(SECTOR_UNKNOWN, SECTOR_ELEC, SECTOR_OTHERS), values = c(80, 100, -20))
  result <- coal_prepare_total_forecasts(x, as.Date("2021-01-01"))
  current <- result %>% filter(time == as.Date("2021-01-01"))
  expect_equal(current$values[current$sector == SECTOR_ELEC], 80)
  expect_equal(current$values[current$sector == SECTOR_OTHERS], 0)
  provenance <- attr(result, "coal_total_forecasts") %>% filter(time == current$time)
  expect_equal(provenance$method, "conflicting_split_bounded_to_total")
  expect_true(provenance$conflict)
  expect_equal(provenance$residual, -20)

  x$values[3] <- -120
  unresolved <- coal_prepare_total_forecasts(x, as.Date("2021-01-01")) %>%
    filter(time == as.Date("2021-01-01"))
  expect_equal(unresolved$values[unresolved$sector == SECTOR_ELEC], 100)
  expect_true(anyNA(unresolved$values))
  expect_false(any(unresolved$values < 0, na.rm = TRUE))
})

test_that("unallocated missing forecasts retain a visible incomplete country total", {
  x <- tibble(iso2 = "BG", date = as.Date("2026-01-01"), fuel = FUEL_COAL,
    sector = c(SECTOR_ELEC, SECTOR_UNKNOWN), value = c(10, NA_real_),
    unit = "t", estimate = "central")
  result <- add_total_co2(detotalise_co2(x))
  expect_equal(filter(result, fuel == "total", estimate == "central")$value, 10)
  expect_false(attr(result, "total_component_completeness")$central_complete)
  unknown <- x %>% filter(sector == SECTOR_UNKNOWN) %>% select(-estimate)
  projected <- project_until_now(unknown, NULL, NULL, NULL, date_to = unknown$date)
  expect_equal(nrow(projected), 3)
  expect_true(all(is.na(projected$value)))
})

test_that("an annual anchor is not compressed into the available half-year", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  m <- tibble(iso2 = "GR", siec = SIEC_BROWN_COAL, unit = "THS_T",
    nrg_bal = "GID_CAL", time = dates, values = 10)
  reported <- m %>% rename(reported_value = values)
  a <- m[1, ] %>% mutate(time = as.Date("2025-01-01"), nrg_bal = "IC_CAL", values = 120)
  result <- .coal_annual_fill_series(m, a, reported, "GR", SIEC_BROWN_COAL,
    "THS_T", 2025, "GID_CAL", as.Date("2025-06-01"))
  expect_equal(nrow(result), 6)
  expect_equal(sum(result$filled_value), 60)
})

test_that("empty annual reconstruction tasks and absent monthly coverage are valid", {
  expect_false(.coal_annual_policy_enabled(character(), character(), integer()))
  m <- tibble(iso2 = "EE", siec = SIEC_OIL_SHALE, unit = "THS_T",
    nrg_bal = "GID_CAL", time = as.Date("2025-01-01"), values = NA_real_)
  a <- mutate(m, nrg_bal = "IC_CAL", values = 120)
  expect_equal(fill_raw_coal_annual_backed(m, a), m)
})

test_that("masked monthly history can leave no eligible raw reconstruction year", {
  monthly <- tibble(iso2 = "GR", siec = SIEC_HARD_COAL, unit = "THS_T",
    nrg_bal = "GID_CAL", time = as.Date("2020-01-01"), values = NA_real_)
  seed <- mutate(monthly, siec = SIEC_BROWN_COAL, values = 0)
  annual <- mutate(monthly, time = as.Date("2019-01-01"), nrg_bal = "IC_CAL", values = 12)
  input <- bind_rows(monthly, seed)
  result <- fill_raw_coal_annual_backed(input, annual)
  expect_equal(result$values, input$values)
  coverage <- tibble(iso2 = "GR", country_start = as.Date("2020-01-01"),
    country_cutoff = as.Date("2020-01-01"))
  expect_equal(.coal_complete_required_rows(input, annual, coverage), input)
})

test_that("an unsupported projection proxy leaves its input unchanged", {
  x <- tibble(iso2 = "PL", fuel = FUEL_COAL, sector = SECTOR_OTHERS,
    unit = "t", date = as.Date("2025-01-01"), value = 10)
  proxy <- tibble(iso2 = "PL", fuel = FUEL_GAS, sector = SECTOR_ALL,
    date = as.Date("2025-01-01"), value_proxy = 1)
  expect_equal(project_until_now_lm(x, proxy, x$date), x)
})

test_that("separate fuel totals rejoin emissions exactly once after projection", {
  identity_projection <- function(co2, ...) co2
  local_mocked_bindings(
    add_ncv_iea = function(x, ...) mutate(x,
      ncv_kjkg = if_else(siec == SIEC_BROWN_COAL_BRIQUETTES, 2000, 1000)),
    add_emission_factor = function(x) mutate(x, co2_factor_t_per_TJ = 2),
    project_until_now_elec = identity_projection,
    project_until_now_gas = identity_projection,
    project_eu_from_countries = identity_projection,
    project_until_now_coal_others = identity_projection,
    fill_eu_internal_gaps = identity_projection,
    project_until_now_forecast = function(co2, ...) {
      tidyr::crossing(co2, estimate = c("central", "lower", "upper"))
    }
  )
  x <- tibble(iso2 = "PL", time = as.Date("2025-01-01"), unit = "THS_T",
    siec = c(SIEC_HARD_COAL, SIEC_BROWN_COAL_BRIQUETTES), fuel = FUEL_COAL,
    sector = SECTOR_OTHERS, values = c(100, 5))
  attr(x, "coal_separate_projection") <- x[2, c("iso2", "siec", "unit", "fuel")]
  converted <- get_co2_from_eurostat_cons(x, diagnostics_folder = NULL)
  expect_equal(converted$value, 200)
  expect_equal(attr(converted, "coal_separate_projection")$value, 20)
  result <- project_until_now(converted, NULL, NULL, NULL, date_to = x$time[1])
  expect_equal(filter(result, estimate == "central")$value, 220)
  expect_equal(nrow(result), 3)
})

test_that("EU coal rebuilds require all members and preserve reported aggregates", {
  x <- tibble(iso2 = get_eu_iso2s(), time = as.Date("2025-01-01"),
    siec = SIEC_HARD_COAL, nrg_bal = "GID_CAL", sector = SECTOR_ALL,
    unit = "THS_T", values = 1)
  result <- coal_fill_complete_eu(x)
  expect_equal(filter(result, iso2 == "EU")$values, 27)
  incomplete <- x[-1, ]
  expect_false("EU" %in% coal_fill_complete_eu(incomplete)$iso2)
  reported <- bind_rows(incomplete, mutate(x[1, ], iso2 = "EU", values = 30))
  expect_equal(filter(coal_fill_complete_eu(reported), iso2 == "EU")$values, 30)
  duplicate <- bind_rows(x, x[1, ])
  expect_false("EU" %in% coal_fill_complete_eu(duplicate)$iso2)
})

test_that("EU tail totals cannot use partial country coverage", {
  dates <- seq(as.Date("2025-01-01"), by = "month", length.out = 8)
  countries <- tidyr::crossing(iso2 = get_eu_iso2s(), date = dates,
    fuel = c(FUEL_COAL, FUEL_TOTAL)) %>% mutate(sector = SECTOR_ALL,
      unit = "t", estimate = "central", value = 1)
  eu <- countries %>% filter(iso2 == "DE") %>% mutate(iso2 = "EU", value = 27)
  missing <- countries %>% mutate(value = if_else(iso2 == "SE" &
    date >= as.Date("2025-07-01"), NA_real_, value))
  result <- select_eu_tail_country_sum_adjustments(bind_rows(eu, missing), tail_months = 2)
  expect_equal(nrow(result), 0)
  expect_true(all(!filter(attr(result, "country_coverage"), is_tail_month)$complete_members))
  absent <- missing %>% filter(!is.na(value))
  expect_equal(nrow(select_eu_tail_country_sum_adjustments(
    bind_rows(eu, absent), tail_months = 2)), 0)
  complete <- select_eu_tail_country_sum_adjustments(
    bind_rows(eu, countries), tail_months = 2)
  expect_equal(nrow(complete), 4)
  expect_equal(complete$value, rep(27, 4))
  duplicate <- bind_rows(countries, filter(countries, iso2 == "SE"))
  expect_equal(nrow(select_eu_tail_country_sum_adjustments(
    bind_rows(eu, duplicate), tail_months = 2)), 0)
})

test_that("known coal totals reuse recent splits only within the same fuel and country", {
  x <- tibble(iso2 = "DE", siec = SIEC_BROWN_COAL_BRIQUETTES, fuel = FUEL_COAL,
    unit = "THS_T", time = as.Date(c("2024-01-01", "2024-01-01", "2025-01-01")),
    sector = c(SECTOR_ELEC, SECTOR_OTHERS, SECTOR_UNKNOWN), values = c(2, 8, 20))
  result <- coal_prepare_total_forecasts(x, as.Date("2025-01-01"))
  current <- filter(result, time == as.Date("2025-01-01"))
  expect_equal(current$values[current$sector == SECTOR_ELEC], 4)
  expect_equal(current$values[current$sector == SECTOR_OTHERS], 16)
  expect_true("recent_historical_split" %in% attr(result, "coal_total_forecasts")$method)

  for (field in c("iso2", "siec", "time")) {
    separate <- x
    separate[[field]][1:2] <- switch(field, iso2 = "FR", siec = SIEC_HARD_COAL,
      time = as.Date("2026-01-01"))
    current <- coal_prepare_total_forecasts(separate, as.Date("2025-01-01")) %>%
      filter(time == as.Date("2025-01-01"))
    expect_equal(current$sector, SECTOR_UNKNOWN)
    expect_equal(current$values, 20)
  }
})

test_that("negative coal splits are bounded even without unallocated history", {
  x <- tibble(iso2 = "IT", siec = SIEC_HARD_COAL, fuel = FUEL_COAL, unit = "THS_T",
    time = as.Date("2025-01-01"), sector = c(SECTOR_ELEC, SECTOR_OTHERS),
    values = c(100, -20))
  result <- coal_prepare_total_forecasts(x, x$time[1])
  expect_equal(result$values[result$sector == SECTOR_ELEC], 80)
  expect_equal(result$values[result$sector == SECTOR_OTHERS], 0)
})
