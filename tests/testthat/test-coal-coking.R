coking_rows <- function(iso2, dates, balance, values, siec = SIEC_HARD_COAL) {
  tibble::tibble(iso2 = iso2, time = as.Date(dates), nrg_bal = balance,
    values = values, siec = siec, unit = "THS_T")
}

test_that("absent coking and explicit missing coking never become zero deductions", {
  x <- bind_rows(
    coking_rows("SE", "2026-03-01", "GID_CAL", 9006),
    coking_rows("SE", "2026-03-01", "TI_EHG_MAP", 3873)
  )
  absent <- process_solid_monthly(x, tibble()) %>% eurostat_split_solid_elec_others()
  explicit <- process_solid_monthly(bind_rows(x,
    coking_rows("SE", "2026-03-01", "TI_CO", NA_real_)), tibble()) %>%
    eurostat_split_solid_elec_others()
  expect_equal(absent, explicit, ignore_attr = TRUE)
  expect_equal(filter(absent, iso2 == "SE", sector == SECTOR_ELEC)$values, 3873)
  expect_true(is.na(filter(absent, iso2 == "SE", sector == SECTOR_OTHERS)$values))
})

test_that("annual residuals preserve observations and reconcile all twelve months", {
  dates <- seq(as.Date("2025-01-01"), by = "month", length.out = 12)
  x <- bind_rows(coking_rows("DE", dates, "GID_CAL", 100),
    coking_rows("DE", dates[1:2], "TI_CO", c(5, 15)),
    coking_rows("DE", dates, "IPRD", 1:12, SIEC_COKE_OVEN_COKE))
  annual <- bind_rows(coking_rows("DE", dates[1], "TI_CO_E", 120),
    coking_rows("DE", dates[1], "TI_E", 1000))
  result <- .resolve_coal_coking(x, annual)
  d <- filter(result$diagnostics, iso2 == "DE", frequency == "monthly") %>% arrange(time)
  expect_equal(d$resolved_value[1:2], c(5, 15))
  expect_equal(sum(d$resolved_value), 120)
  expect_equal(d$resolved_value[3:12], 100 * (3:12) / sum(3:12))
  expect_true(all(d$method[3:12] == "annual_residual"))
})

test_that("steel activity resolves France and evidenced EU omissions exactly once", {
  history <- seq(as.Date("2019-01-01"), by = "month", length.out = 12)
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  local_mocked_bindings(get_eu_iso2s = function(include_eu = FALSE) c("FR", "DE"),
    .package = "creaco2tracker")
  x <- bind_rows(coking_rows("FR", history, "TI_CO", 20),
    coking_rows("FR", c(history, dates), "GID_CAL", 100),
    coking_rows("FR", dates, "TI_EHG_MAP", 10),
    coking_rows("DE", dates, "TI_CO", 40),
    coking_rows("EU", dates[1:3], "TI_CO", 40),
    coking_rows("EU", dates, "GID_CAL", 200),
    coking_rows("EU", dates, "TI_EHG_MAP", 30))
  annual <- bind_rows(coking_rows("FR", dates[1], "TI_CO_E", 0),
    coking_rows("FR", dates[1], "TI_E", 800),
    coking_rows("FR", dates[1], "FC_E", 100),
    coking_rows("DE", dates[1], "TI_CO_E", 480),
    coking_rows("EU", dates[1], "TI_CO_E", 480))
  activity <- tibble(iso2 = "FR", time = c(history, dates), nace_r2 = "C241",
    s_adj = "NSA", unit = "I21", values = c(rep(100, 12), rep(50, 12)))
  activity <- bind_rows(activity, activity %>% filter(time %in% history) %>%
    mutate(unit = "I15", values = values * 2))
  result <- .resolve_coal_coking(x, annual, activity)
  fr <- filter(result$diagnostics, iso2 == "FR", frequency == "monthly", time %in% dates)
  expect_equal(fr$resolved_value, rep(10, 12))
  expect_equal(filter(result$diagnostics, iso2 == "EU", frequency == "monthly")$resolved_value,
    rep(50, 12))
  expect_equal(filter(result$diagnostics, iso2 == "EU", frequency == "annual")$resolved_value, 600)
  expect_equal(filter(result$diagnostics, iso2 == "FR", frequency == "annual")$resolved_value, 120)
  again <- .resolve_coal_coking(result$monthly, result$yearly, activity)
  expect_equal(again$diagnostics, result$diagnostics)
  split <- process_solid_monthly(result$monthly, tibble()) %>% eurostat_split_solid_elec_others()
  expect_equal(filter(split, iso2 == "EU", sector == SECTOR_OTHERS)$values, rep(124, 12))
  energy <- coal_annual_energy(result$yearly) %>% filter(iso2 == "FR")
  expect_equal(energy$energy, 900 - .92 * 120)
})

test_that("genuine zeros, duplicate rows and incompatible units remain distinct", {
  dates <- as.Date("2025-01-01")
  x <- bind_rows(coking_rows("SE", dates, "GID_CAL", 100),
    coking_rows("SE", dates, "TI_CO", 0),
    coking_rows("DE", dates, "GID_CAL", 100),
    coking_rows("DE", dates, "TI_CO", c(10, 10)),
    coking_rows("AT", dates, "GID_CAL", 100),
    mutate(coking_rows("AT", dates, "TI_CO", 10), unit = "TJ"))
  r <- .resolve_coal_coking(x, x[0, ])
  d <- filter(r$diagnostics, frequency == "monthly")
  expect_equal(filter(d, iso2 == "SE")$resolved_value, 0)
  expect_true(is.na(filter(d, iso2 == "DE")$resolved_value))
  expect_true(is.na(filter(d, iso2 == "AT")$resolved_value))
  expect_true(filter(d, iso2 == "DE")$duplicate)
  expect_equal(filter(r$monthly, iso2 == "AT", unit == "TJ")$values, 10)
  expect_false(.coking_conflict("FR", as.Date("2026-01-01"), 0))
  expect_false(.coking_conflict("FR", as.Date("2024-01-01"), 0, "monthly"))
})

test_that("annual-only coking has a non-recursive fallback and transformation bound", {
  a <- bind_rows(coking_rows("DE", "2023-01-01", "TI_CO_E", 100),
    coking_rows("DE", as.Date(c("2024-01-01", "2025-01-01")), "TI_E", c(80, 200)))
  r <- .resolve_coal_coking(a[0, ], a)
  d <- filter(r$diagnostics, iso2 == "DE", frequency == "annual") %>% arrange(time)
  expect_equal(d$resolved_value, c(100, 80, NA_real_))
  expect_true(d$bounded[2])
})

test_that("candidate selection uses matched holdouts and ranks six-month error first", {
  candidates <- tibble(time = as.Date("2025-01-01"), previous_year = 10,
    coke_activity = 20, steel_activity = 30)
  scores <- tidyr::crossing(method = c("previous_year", "coke_activity", "steel_activity"),
    start = as.Date(c("2022-01-01", "2023-01-01", "2024-01-01"))) %>%
    mutate(horizon = "short", total_error = if_else(method == "steel_activity", 1, 2),
      monthly_error = if_else(method == "steel_activity", 4, 1))
  expect_equal(.coking_choose(candidates, scores, "short")$method, "steel_activity")
  expect_equal(.coking_choose(candidates, scores[0, ], "long")$method, "coke_activity")
  history <- tibble(time = seq(as.Date("2019-01-01"), by = "month", length.out = 24),
    value = c(rep(10, 12), rep(9999, 12)), duplicate = FALSE, conflict = FALSE)
  act <- tibble(time = history$time, proxy = "coke_activity", value = 100)
  p <- .coking_candidates(history, history$time[13:24], act, as.Date("2020-01-01"))
  expect_equal(p$coke_activity, rep(10, 12))
  expect_equal(p$previous_year, rep(10, 12))
})

test_that("conversion and total forecasts use the coking-adjusted quantity once", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  x <- bind_rows(coking_rows("DE", dates, "GID_CAL", 100),
    coking_rows("DE", dates, "TI_CO", 20))
  resolved <- .resolve_coal_coking(x, x[0, ])
  cons <- process_solid_monthly(resolved$monthly, tibble()) %>%
    eurostat_split_solid_elec_others()
  expect_equal(cons$values, rep(81.6, 12))
  attr(cons, "coal_coking_provenance") <- resolved$diagnostics
  projected <- coal_prepare_total_forecasts(cons, as.Date("2025-06-01"))
  expect_equal(filter(projected, time > max(dates))$values, rep(81.6, 6))
  expect_equal(attr(projected, "coal_coking_provenance"), resolved$diagnostics)
  coke <- cons %>% mutate(siec = SIEC_COKE_OVEN_COKE, fuel = FUEL_COKE,
    sector = SECTOR_OTHERS, values = 10)
  emissions <- get_co2_from_eurostat_cons(bind_rows(cons, coke), ncv_source = "ipcc",
    diagnostics_folder = NULL)
  factor <- filter(get_ipcc_emission_factors(), siec == SIEC_COKE_OVEN_COKE)$co2_factor_t_per_TJ
  expect_equal(sum(emissions$value), 12 * (81.6 * 26.7 * 92.8 + 10 * 28.2 * factor))
})

test_that("an absent month for two members can be rebuilt without treating it as zero", {
  local_mocked_bindings(get_eu_iso2s = function(include_eu = FALSE) c("DE", "SE"),
    .package = "creaco2tracker")
  dates <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01",
    "2026-01-01", "2026-03-01"))
  x <- bind_rows(coking_rows("DE", dates, "TI_CO", 20),
    coking_rows("SE", dates, "TI_CO", 30))
  r <- .resolve_coal_coking(x, x[0, ])
  eu <- filter(r$diagnostics, iso2 == "EU", time == as.Date("2026-02-01"))
  expect_equal(eu$resolved_value, 50)
  expect_equal(eu$method, "resolved_member_sum")
})

test_that("a zero annual transformation total cannot erase observed coking", {
  dates <- seq(as.Date("2025-01-01"), by = "month", length.out = 12)
  monthly <- coking_rows("SK", dates, "TI_CO", 10)
  annual <- bind_rows(coking_rows("SK", dates[1], "TI_E", 0),
    coking_rows("SK", dates[1], "FC_E", 100))
  r <- .resolve_coal_coking(monthly, annual)
  d <- filter(r$diagnostics, iso2 == "SK", frequency == "annual")
  expect_equal(d$resolved_value, 120)
  expect_equal(d$constraint, "annual_transformation_conflict")
  expect_true(is.na(coal_annual_energy(r$yearly)$energy))
})

test_that("annual-only histories can use current activity without monthly calibration", {
  dates <- seq(as.Date("2022-01-01"), by = "month", length.out = 36)
  a <- bind_rows(coking_rows("DE", as.Date(c("2022-01-01", "2023-01-01")), "TI_CO_E", 120),
    coking_rows("DE", "2024-01-01", "TI_E", 500))
  industry <- tibble(iso2 = "DE", time = dates, nace_r2 = "C241", s_adj = "NSA",
    unit = "I21", values = c(rep(100, 24), rep(200, 12)))
  r <- .resolve_coal_coking(a[0, ], a, industry)
  expect_equal(filter(r$diagnostics, iso2 == "DE", time == as.Date("2024-01-01"))$resolved_value,
    240)
})

test_that("proxy coverage is only required for the missing months", {
  history <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  current <- as.Date(c("2026-01-01", "2026-02-01"))
  x <- bind_rows(coking_rows("DE", history, "TI_CO", 10),
    coking_rows("DE", current, "GID_CAL", 100),
    coking_rows("DE", current[1], "TI_CO", 20),
    coking_rows("DE", c(history, current[2]), "IPRD", 5, SIEC_COKE_OVEN_COKE))
  r <- .resolve_coal_coking(x, x[0, ])
  expect_equal(filter(r$diagnostics, iso2 == "DE", time %in% current)$resolved_value,
    c(20, 10))
})

test_that("reported accounting conflicts are flagged without changing observations", {
  x <- bind_rows(coking_rows("DE", "2026-01-01", "GID_CAL", 100),
    coking_rows("DE", "2026-01-01", "TI_CO", 120))
  d <- .resolve_coal_coking(x, x[0, ])$diagnostics %>% filter(iso2 == "DE")
  expect_equal(d$resolved_value, 120)
  expect_equal(d$constraint, "reported_accounting_conflict")
})

test_that("partial proxy coverage retains usable estimates and explicit unresolved months", {
  history <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  current <- as.Date(c("2026-01-01", "2026-02-01"))
  x <- bind_rows(coking_rows("DE", history, "TI_CO", 10),
    coking_rows("DE", current, "GID_CAL", 100),
    coking_rows("DE", c(history, current[2]), "IPRD", 5, SIEC_COKE_OVEN_COKE))
  r <- .resolve_coal_coking(x, x[0, ])
  d <- filter(r$diagnostics, iso2 == "DE", time %in% current)
  expect_equal(d$resolved_value, c(NA_real_, 10))
  expect_equal(d$method, c("unresolved", "coke_activity"))
})

test_that("cached EU March-May 2026 inputs resolve even with absent coking rows", {
  # Eurostat cache snapshot 17 September 2026: all 27 members, reported
  # coking plus France's pre-break history and its C241 I21 activity series.
  f <- read.csv(test_path("fixtures", "coking-eu-20260917.csv"))
  f$time <- as.Date(f$time)
  x <- f %>% filter(kind == "coking") %>%
    transmute(iso2, time, nrg_bal = "TI_CO", values = value,
      siec = SIEC_HARD_COAL, unit = "THS_T")
  industry <- f %>% filter(kind == "steel") %>%
    transmute(iso2, time, nace_r2 = "C241", s_adj = "NSA", unit = "I21", values = value)
  dates <- as.Date(c("2026-03-01", "2026-04-01", "2026-05-01"))
  explicit <- .resolve_coal_coking(x, x[0, ], industry)
  absent <- .resolve_coal_coking(filter(x, !(iso2 == "EU" & time %in% dates)), x[0, ], industry)
  eu <- filter(explicit$diagnostics, iso2 == "EU", time %in% dates)
  expect_equal(eu, filter(absent$diagnostics, iso2 == "EU", time %in% dates))
  members <- explicit$diagnostics %>% filter(iso2 %in% get_eu_iso2s(), time %in% dates) %>%
    group_by(time) %>% summarise(total = sum(resolved_value), .groups = "drop")
  expect_true(all(is.finite(eu$resolved_value)))
  expect_equal(eu$resolved_value, members$total)
  expect_equal(eu$deduction, .92 * members$total)
  expect_equal(eu$method, rep("resolved_member_sum", 3))
})
