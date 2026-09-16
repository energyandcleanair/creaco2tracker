test_that("raw coal gap methods preserve reported observations and fill simple internal gaps", {
  dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
  x <- tibble(iso2 = "GR", siec = "C0200", nrg_bal = "GID_CAL", unit = "THS_T",
    time = dates, value = 100 + month(dates), reported = TRUE, source_flag = "")
  masked <- creaco2tracker:::.coal_gap_mask(x, as.Date("2024-05-01"), 3, "internal")
  prediction <- creaco2tracker:::.coal_gap_predict(masked, tibble(), as.Date("2024-05-01"), 3,
    "internal", "interpolation")
  expect_equal(prediction$values, c(105, 106, 107))
  expect_equal(masked$value[masked$time == as.Date("2024-04-01")], 104)
  expect_true(all(is.na(masked$value[masked$masked])))
})

test_that("seasonal methods do not use data after a forecast gap", {
  dates <- seq(as.Date("2017-01-01"), as.Date("2024-12-01"), by = "month")
  x <- tibble(iso2 = "SE", siec = "C0100", nrg_bal = "GID_CAL", unit = "THS_T",
    time = dates, value = 10 + month(dates), reported = TRUE, source_flag = "")
  masked <- creaco2tracker:::.coal_gap_mask(x, as.Date("2024-07-01"), 1, "forecast")
  prediction <- creaco2tracker:::.coal_gap_predict(masked, tibble(), as.Date("2024-07-01"),
    6, "forecast", "forward_ets")
  expect_false(is.null(prediction))
  expect_true(all(prediction$values >= 0))
})

test_that("accounting needs reported supply inputs and reconciled annual history", {
  dates <- seq(as.Date("2021-01-01"), as.Date("2024-12-01"), by = "month")
  supply <- tidyr::crossing(time = dates, nrg_bal = c("IPRD", "IMP", "EXP", "STK_CHG")) %>%
    mutate(iso2 = "EE", siec = "S2000", unit = "THS_T", reported = TRUE,
      value = case_when(nrg_bal == "IPRD" ~ 10, nrg_bal == "IMP" ~ 2, nrg_bal == "EXP" ~ 1, TRUE ~ -1))
  target <- supply %>% filter(nrg_bal == "IPRD") %>% transmute(iso2, siec, unit, time,
    nrg_bal = "GID_CAL", value = 10, reported = TRUE, source_flag = "")
  attr(target, "supply_inputs") <- supply
  annual <- tibble(iso2 = "EE", siec = "S2000", unit = "THS_T", nrg_bal = "IC_CAL",
    year = 2021:2023, annual_value = 120)
  masked <- creaco2tracker:::.coal_gap_mask(target, as.Date("2024-01-01"), 1, "internal")
  attr(masked, "supply_inputs") <- supply
  fit <- creaco2tracker:::.coal_gap_predict(masked, annual, as.Date("2024-01-01"), 1,
    "internal", "accounting")
  expect_equal(fit$values, 10)
  attr(masked, "supply_inputs")$reported[1] <- FALSE
  expect_null(creaco2tracker:::.coal_gap_predict(masked, annual, as.Date("2024-01-01"), 1,
    "internal", "accounting"))
})

test_that("reconciliation emits warnings and handles annual zero", {
  x <- tibble(iso2 = "SE", siec = "C0100", nrg_bal = "GID_CAL", unit = "THS_T",
    time = seq(as.Date("2024-01-01"), by = "month", length.out = 12), value = 10,
    reported = TRUE)
  annual <- tibble(iso2 = c("SE", "EE"), siec = c("C0100", "S2000"),
    nrg_bal = c("IC_CAL", "IC_CAL"), unit = "THS_T", year = 2024, annual_value = c(100, 0))
  out <- creaco2tracker:::.coal_gap_reconcile(x, annual)
  expect_true(out$warning[1])
  expect_false(out$zero_denominator[1])
})

test_that("fallback leaves unresolved cells explicit", {
  x <- tibble(iso2 = "GR", siec = "C0200", nrg_bal = "GID_CAL", unit = "THS_T",
    time = as.Date("2024-01-01"), value = NA_real_, reported = FALSE, source_flag = "")
  out <- creaco2tracker:::.coal_gap_apply_sequence(x, tibble(), as.Date("2024-01-01"), 1,
    "forecast", c("forward_ets", "previous_year"))
  expect_equal(out$method, "unresolved")
  expect_true(is.na(out$values))
})

test_that("short ETS history is ineligible and signed balances are not clipped", {
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  short <- tibble(iso2 = "SE", siec = "C0330", nrg_bal = "GID_CAL", unit = "THS_T",
    time = dates, value = rep(0, 12), reported = TRUE, source_flag = "")
  masked <- creaco2tracker:::.coal_gap_mask(short, as.Date("2024-12-01"), 1, "forecast")
  expect_null(creaco2tracker:::.coal_gap_predict(masked, tibble(), as.Date("2024-12-01"), 1,
    "forecast", "forward_ets"))
  stocks <- short %>% mutate(nrg_bal = "STK_CHG", value = -1)
  expect_false("STK_CHG" %in% creaco2tracker:::.coal_gap_nonnegative_balances)
  expect_equal(stocks$value[1], -1)
})
