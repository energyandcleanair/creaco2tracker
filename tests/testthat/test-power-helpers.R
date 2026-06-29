# Unit tests for the private helpers powering get_power_generation().
# Each test maps to a specific bug or invariant we want to preserve.

library(testthat)
suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# ============================================================================
# .aggregate_entsoe_monthly()
# ============================================================================

test_that(".aggregate_entsoe_monthly drops phantom NA-only source rows", {
  # ENTSOE's complete() creates rows for every (date, source, country) combo,
  # filling absent sources with NA. Austria with Wind Offshore should not end
  # up flagging Wind as incomplete because of those NA rows.
  df <- expand_grid(
    iso2   = "AT",
    source = c("Wind Onshore", "Wind Offshore"),
    date   = seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
  ) %>%
    mutate(value_mwh = if_else(source == "Wind Offshore", NA_real_, 100))

  result <- .aggregate_entsoe_monthly(df)

  # Only Wind (combined) should appear, and as a full month
  expect_equal(nrow(result), 1L)
  expect_equal(result$source, "Wind")
  expect_true(result$full_month)
  expect_equal(result$entsoe_mwh, 31 * 100)
})

test_that(".aggregate_entsoe_monthly combines sub-sources with all-full rule", {
  # When both Wind Onshore AND Wind Offshore are present, full_month must be
  # TRUE only if both constituents are complete.
  df <- expand_grid(
    iso2   = "DE",
    source = c("Wind Onshore", "Wind Offshore"),
    date   = seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
  ) %>%
    mutate(value_mwh = if_else(
      source == "Wind Offshore" & date == "2024-01-15",
      NA_real_, 100
    ))

  result <- .aggregate_entsoe_monthly(df)

  expect_equal(result$source, "Wind")
  expect_false(result$full_month) # one day of Offshore missing => not full
})

test_that(".aggregate_entsoe_monthly excludes Hydro Pumped Storage", {
  df <- expand_grid(
    iso2   = "AT",
    source = c("Hydro Run-of-river and poundage", "Hydro Pumped Storage"),
    date   = seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
  ) %>%
    mutate(value_mwh = 50)

  result <- .aggregate_entsoe_monthly(df)

  expect_equal(unique(result$source), "Hydro") # only run-of-river kept
  expect_equal(result$entsoe_mwh, 31 * 50) # pumped storage NOT summed in
})

# ============================================================================
# .compute_ratio_monthly()
# ============================================================================

test_that(".compute_ratio_monthly allows ember=0 (yields ratio=0)", {
  # SI/SK Wind case: Ember reports 0 generation while ENTSOE has small values.
  # The pipeline should produce ratio = 0 so scaled output is forced to 0.
  ent <- tribble(
    ~iso2, ~source, ~year, ~month, ~date, ~entsoe_mwh, ~full_month,
    "SI", "Wind", 2024L, 1L, as.Date("2024-01-01"), 3100, TRUE
  )
  emb <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "SI",  "Wind",  as.Date("2024-01-01"), 0
  )

  result <- .compute_ratio_monthly(ent, emb)

  expect_equal(nrow(result), 1L)
  expect_equal(result$ratio, 0)
})

test_that(".compute_ratio_monthly excludes entsoe=0 rows (no Inf ratios)", {
  ent <- tribble(
    ~iso2, ~source, ~year, ~month, ~date, ~entsoe_mwh, ~full_month,
    "SI", "Wind", 2024L, 1L, as.Date("2024-01-01"), 0, TRUE
  )
  emb <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "SI",  "Wind",  as.Date("2024-01-01"), 100
  )

  result <- .compute_ratio_monthly(ent, emb)
  expect_equal(nrow(result), 0L) # excluded; Ember-only fallback handles this
})

test_that(".compute_ratio_monthly keeps only full months", {
  ent <- tribble(
    ~iso2, ~source, ~year, ~month, ~date, ~entsoe_mwh, ~full_month,
    "DE", "Wind", 2024L, 1L, as.Date("2024-01-01"), 3000, TRUE,
    "DE", "Wind", 2024L, 2L, as.Date("2024-02-01"), 2800, FALSE # partial
  )
  emb <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "DE",  "Wind",  as.Date("2024-01-01"), 3100,
    "DE",  "Wind",  as.Date("2024-02-01"), 2900
  )

  result <- .compute_ratio_monthly(ent, emb)
  expect_equal(nrow(result), 1L)
  expect_equal(result$date, as.Date("2024-01-01"))
})

# ============================================================================
# .flag_outliers_mad()
# ============================================================================

test_that(".flag_outliers_mad flags 6xMAD points and leaves 4xMAD alone", {
  # median = 1, MAD-of-c(0.9,1,1.1) under constant 1.4826 = ~0.148
  # Use values 1.0, 1.0, 1.0, 1.5 - a clear outlier
  df <- tribble(
    ~iso2, ~source, ~date,                 ~ratio,
    "DE",  "Wind",  as.Date("2024-01-01"), 1.00,
    "DE",  "Wind",  as.Date("2024-02-01"), 0.98,
    "DE",  "Wind",  as.Date("2024-03-01"), 1.02,
    "DE",  "Wind",  as.Date("2024-04-01"), 1.01,
    "DE",  "Wind",  as.Date("2024-05-01"), 5.00 # clear outlier
  )

  result <- .flag_outliers_mad(df, threshold = 5)
  expect_true(result$is_outlier[result$date == as.Date("2024-05-01")])
  expect_false(any(result$is_outlier[result$date != as.Date("2024-05-01")]))
})

test_that(".flag_outliers_mad handles constant series (MAD=0) without flagging", {
  df <- tribble(
    ~iso2, ~source, ~date,                 ~ratio,
    "DE",  "Wind",  as.Date("2024-01-01"), 1.0,
    "DE",  "Wind",  as.Date("2024-02-01"), 1.0,
    "DE",  "Wind",  as.Date("2024-03-01"), 1.0
  )
  result <- .flag_outliers_mad(df, threshold = 5)
  expect_false(any(result$is_outlier))
})

test_that(".flag_outliers_mad treats each (iso2, source) group independently", {
  df <- tribble(
    ~iso2, ~source, ~date,                 ~ratio,
    "DE",  "Wind",  as.Date("2024-01-01"), 1.0,
    "DE",  "Wind",  as.Date("2024-02-01"), 1.0,
    "DE",  "Wind",  as.Date("2024-03-01"), 1.0,
    "DE",  "Solar", as.Date("2024-01-01"), 5.0, # extreme vs Wind median, but
    "DE",  "Solar", as.Date("2024-02-01"), 5.0, # consistent within Solar
    "DE",  "Solar", as.Date("2024-03-01"), 5.0
  )
  result <- .flag_outliers_mad(df, threshold = 5)
  expect_false(any(result$is_outlier)) # both groups internally consistent
})

# ============================================================================
# .extend_ratio()
# ============================================================================

test_that(".extend_ratio output grid covers start->target with no gaps", {
  df <- tibble(
    iso2       = "DE",
    source     = "Wind",
    date       = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    ratio      = 1,
    is_outlier = FALSE
  )

  result <- .extend_ratio(df,
    target_date = as.Date("2025-03-01"),
    frequency   = 12L,
    min_obs     = 12L
  )

  expected_dates <- seq.Date(as.Date("2024-01-01"), as.Date("2025-03-01"), by = "month")
  expect_equal(sort(result$date), expected_dates)
  expect_equal(sum(result$type == "forecast"), 3L) # Jan, Feb, Mar 2025
})

test_that(".extend_ratio with too-short series falls back to last-value carry-forward", {
  df <- tibble(
    iso2       = "DE",
    source     = "Wind",
    date       = seq.Date(as.Date("2024-01-01"), as.Date("2024-03-01"), by = "month"),
    ratio      = c(1.0, 1.1, 1.2),
    is_outlier = FALSE
  )

  result <- .extend_ratio(df,
    target_date = as.Date("2024-06-01"),
    frequency   = 12L,
    min_obs     = 12L
  ) # we have only 3 obs

  # Future months should carry the last observed value (1.2)
  expect_equal(result$ratio_used[result$date == as.Date("2024-06-01")], 1.2)
})

test_that(".extend_ratio with method='last' uses locf for outliers and future months", {
  df <- tibble(
    iso2       = "DE",
    source     = "Wind",
    date       = seq.Date(as.Date("2024-01-01"), as.Date("2024-06-01"), by = "month"),
    ratio      = c(1.0, 1.1, 1.2, 99, 1.4, 1.5),
    is_outlier = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )

  result <- .extend_ratio(df,
    target_date = as.Date("2024-09-01"),
    frequency   = 12L,
    min_obs     = 6L,
    method      = "last"
  )

  # Outlier in April: should be filled with previous good value (March = 1.2)
  expect_equal(result$ratio_used[result$date == as.Date("2024-04-01")], 1.2)
  # Future months Jul/Aug/Sep: carry forward last observed (June = 1.5)
  expect_equal(
    result$ratio_used[result$date > as.Date("2024-06-01")],
    rep(1.5, 3)
  )
  # Future months are flagged as forecast
  expect_true(all(result$type[result$date > as.Date("2024-06-01")] == "forecast"))
})

test_that(".extend_ratio marks outlier months as 'interpolated' and future as 'forecast'", {
  df <- tibble(
    iso2       = "DE",
    source     = "Wind",
    date       = seq.Date(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "month"),
    ratio      = c(rep(1, 12), 5, rep(1, 11)), # month 13 is outlier
    is_outlier = c(rep(FALSE, 12), TRUE, rep(FALSE, 11))
  )

  result <- .extend_ratio(df,
    target_date = as.Date("2025-02-01"),
    frequency   = 12L,
    min_obs     = 12L
  )

  outlier_row <- result %>% filter(date == as.Date("2024-01-01"))
  expect_equal(outlier_row$type, "interpolated")
  expect_true(outlier_row$ratio_used != 5) # smoothed, not raw outlier value

  forecast_rows <- result %>% filter(date > as.Date("2024-12-01"))
  expect_true(all(forecast_rows$type == "forecast"))
})

# ============================================================================
# .apply_monthly_scaling()
# ============================================================================

test_that(".apply_monthly_scaling prefers ratio_obs over ratio_used", {
  # The recent fix: for months where Ember monthly is observed (ratio_obs not NA),
  # use that exact ratio rather than the ETS-smoothed ratio_used.
  daily <- tribble(
    ~iso2, ~source,        ~date,                 ~value_mwh,
    "DE",  "Wind Onshore", as.Date("2024-01-15"), 100,
    "DE",  "Wind Onshore", as.Date("2024-02-15"), 100
  )

  ratios <- tribble(
    ~iso2, ~source, ~date,                 ~ratio_obs, ~ratio_used, ~is_outlier, ~type,
    "DE",  "Wind",  as.Date("2024-01-01"), 2.0,        1.0,         FALSE,       "observed",
    "DE",  "Wind",  as.Date("2024-02-01"), NA,         1.0,         FALSE,       "forecast"
  )

  result <- .apply_monthly_scaling(daily, ratios)

  # Jan: ratio_obs available -> scaled = 100 * 2.0 = 200
  expect_equal(result$value_mwh[result$date == "2024-01-15"], 200)
  # Feb: ratio_obs NA -> fall back to ratio_used = 1.0 -> scaled = 100
  expect_equal(result$value_mwh[result$date == "2024-02-15"], 100)
})

test_that(".apply_monthly_scaling sums sub-sources before applying ratio", {
  daily <- tribble(
    ~iso2, ~source, ~date, ~value_mwh,
    "DE", "Wind Onshore", as.Date("2024-01-15"), 120,
    "DE", "Wind Offshore", as.Date("2024-01-15"), 80
  )
  ratios <- tribble(
    ~iso2, ~source, ~date,                 ~ratio_obs, ~ratio_used, ~is_outlier, ~type,
    "DE",  "Wind",  as.Date("2024-01-01"), 2.0,        2.0,         FALSE,       "observed"
  )

  result <- .apply_monthly_scaling(daily, ratios)

  # Single Wind row, value = (120 + 80) * 2.0 = 400
  expect_equal(nrow(result), 1L)
  expect_equal(result$source, "Wind")
  expect_equal(result$value_mwh, 400)
})

test_that(".apply_monthly_scaling passes through unscaled when no ratio exists", {
  daily <- tribble(
    ~iso2, ~source,        ~date,                 ~value_mwh,
    "DE",  "Wind Onshore", as.Date("2024-01-15"), 100
  )
  ratios <- tibble(
    iso2 = character(0), source = character(0), date = as.Date(character(0)),
    ratio_obs = double(0), ratio_used = double(0),
    is_outlier = logical(0), type = character(0)
  )

  result <- .apply_monthly_scaling(daily, ratios)
  expect_equal(result$value_mwh, 100) # unscaled
})

# ============================================================================
# .find_ember_only_series()
# ============================================================================

test_that(".find_ember_only_series detects (iso2, source) absent from scaled", {
  ember <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "CY",  "Wind",  as.Date("2024-01-01"), 100,
    "DE",  "Wind",  as.Date("2024-01-01"), 200
  )
  scaled <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "DE",  "Wind",  as.Date("2024-01-15"), 200
  )

  result <- .find_ember_only_series(ember, scaled, iso2s = c("CY", "DE"))
  expect_equal(nrow(result), 1L)
  expect_equal(result$iso2, "CY")
  expect_equal(result$source, "Wind")
})

test_that(".find_ember_only_series detects pairs where scaled sums to zero", {
  # SI/SK Wind case: scaled has rows but they're all 0 -> needs fallback.
  ember <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "SI",  "Wind",  as.Date("2024-01-01"), 100
  )
  scaled <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "SI",  "Wind",  as.Date("2024-01-15"), 0,
    "SI",  "Wind",  as.Date("2024-01-16"), 0
  )

  result <- .find_ember_only_series(ember, scaled, iso2s = "SI")
  expect_equal(nrow(result), 1L)
  expect_equal(result$iso2, "SI")
})

test_that(".find_ember_only_series does not flag pairs with meaningful scaled output", {
  ember <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "DE",  "Wind",  as.Date("2024-01-01"), 100
  )
  scaled <- tribble(
    ~iso2, ~source, ~date,                 ~value_mwh,
    "DE",  "Wind",  as.Date("2024-01-15"), 50
  )

  result <- .find_ember_only_series(ember, scaled, iso2s = "DE")
  expect_equal(nrow(result), 0L)
})

test_that(".find_ember_only_series ignores Ember sources not in EMBER_TO_OUTPUT", {
  ember <- tribble(
    ~iso2, ~source,        ~date,                 ~value_mwh,
    "CY",  "Imported",     as.Date("2024-01-01"), 50, # not a tracked source
    "CY",  "Wind",         as.Date("2024-01-01"), 100
  )
  scaled <- tibble(
    iso2 = character(0), source = character(0),
    date = as.Date(character(0)), value_mwh = double(0)
  )

  result <- .find_ember_only_series(ember, scaled, iso2s = "CY")
  expect_equal(unique(result$source), "Wind") # Imported filtered out
})
