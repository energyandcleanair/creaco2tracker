# One-off generator for power-generation pipeline test fixtures.
#
# Run from the package root:
#   Rscript tests/testthat/fixtures/build-power-fixtures.R
#
# Produces three CSVs in tests/testthat/fixtures/:
#   power_entsoe_daily.csv   - ENTSOE-shaped daily generation
#   power_ember_monthly.csv  - Ember-shaped monthly generation
#   power_ember_yearly.csv   - Ember-shaped yearly generation
#
# Designed to exercise every code path of get_power_generation():
#   DE: normal scaling, one MAD outlier (2024-06 Solar), small non-1 yearly correction
#   CY: Ember-only fallback (no ENTSOE rows at all)
#   SI: ENTSOE=0/Ember>0 case (Wind) + ENTSOE>0/Ember=0 case (Gas)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

OUT_DIR  <- "tests/testthat/fixtures"
DATE_FROM <- as.Date("2023-01-01")
DATE_TO   <- as.Date("2024-12-31")
DATES_D   <- seq.Date(DATE_FROM, DATE_TO, by = "day")
DATES_M   <- seq.Date(DATE_FROM, DATE_TO, by = "month")
DATES_Y   <- as.Date(c("2023-01-01", "2024-01-01"))

# ---- DE: well-behaved scaling, one outlier month, yearly correction ~1.04 ----
de_entsoe_daily <- expand_grid(
  iso2   = "DE",
  source = c("Wind Onshore", "Wind Offshore", "Solar", "Fossil Gas"),
  date   = DATES_D
) %>%
  mutate(
    value_mwh = case_when(
      source == "Wind Onshore"  ~ 120,
      source == "Wind Offshore" ~  80,
      source == "Solar"         ~ 100,
      source == "Fossil Gas"    ~ 200
    )
  )

# Ember monthly = entsoe monthly sum (so ratio ~1) except DE Solar 2024-06 (5× outlier)
de_ember_monthly <- expand_grid(
  iso2   = "DE",
  source = c("Wind", "Solar", "Gas"),
  date   = DATES_M
) %>%
  mutate(
    dim = as.integer(days_in_month(date)),
    value_mwh = case_when(
      source == "Wind"  ~ 200 * dim,  # 120 + 80 = 200/day
      source == "Solar" ~ 100 * dim,
      source == "Gas"   ~ 200 * dim
    ),
    value_mwh = if_else(
      source == "Solar" & date == as.Date("2024-06-01"),
      value_mwh * 5,                 # planted outlier
      value_mwh
    )
  ) %>%
  select(iso2, source, date, value_mwh)

# Yearly = (sum of monthly) × 1.04 so the yearly correction is a visible 4% nudge
de_ember_yearly <- de_ember_monthly %>%
  mutate(year = year(date)) %>%
  group_by(iso2, source, year) %>%
  summarise(value_mwh = sum(value_mwh) * 1.04, .groups = "drop") %>%
  mutate(date = ymd(paste(year, "01", "01", sep = "-"))) %>%
  select(iso2, source, date, value_mwh)

# ---- CY: Ember-only (no ENTSOE rows) ----
cy_entsoe_daily <- tibble()

cy_ember_monthly <- expand_grid(
  iso2   = "CY",
  source = c("Wind", "Solar", "Gas"),
  date   = DATES_M
) %>%
  mutate(
    dim = as.integer(days_in_month(date)),
    value_mwh = case_when(
      source == "Wind"  ~  50 * dim,
      source == "Solar" ~  80 * dim,
      source == "Gas"   ~ 150 * dim
    )
  ) %>%
  select(iso2, source, date, value_mwh)

cy_ember_yearly <- cy_ember_monthly %>%
  mutate(year = year(date)) %>%
  group_by(iso2, source, year) %>%
  summarise(value_mwh = sum(value_mwh), .groups = "drop") %>%
  mutate(date = ymd(paste(year, "01", "01", sep = "-"))) %>%
  select(iso2, source, date, value_mwh)

# ---- SI: edge cases (ENTSOE=0/Ember>0 wind, ENTSOE>0/Ember=0 gas) ----
si_entsoe_daily <- expand_grid(
  iso2   = "SI",
  source = c("Wind Onshore", "Solar", "Fossil Gas"),
  date   = DATES_D
) %>%
  mutate(
    value_mwh = case_when(
      source == "Wind Onshore" ~   0,   # ENTSOE reports no wind
      source == "Solar"        ~  60,
      source == "Fossil Gas"   ~  40    # ENTSOE has gas, Ember will report 0
    )
  )

si_ember_monthly <- expand_grid(
  iso2   = "SI",
  source = c("Wind", "Solar", "Gas"),
  date   = DATES_M
) %>%
  mutate(
    dim = as.integer(days_in_month(date)),
    value_mwh = case_when(
      source == "Wind"  ~ 60 * dim,    # Ember reports real wind
      source == "Solar" ~ 60 * dim,    # matches ENTSOE
      source == "Gas"   ~ 0            # Ember reports zero gas
    )
  ) %>%
  select(iso2, source, date, value_mwh)

si_ember_yearly <- si_ember_monthly %>%
  mutate(year = year(date)) %>%
  group_by(iso2, source, year) %>%
  summarise(value_mwh = sum(value_mwh), .groups = "drop") %>%
  mutate(date = ymd(paste(year, "01", "01", sep = "-"))) %>%
  select(iso2, source, date, value_mwh)

# ---- Assemble and write ----
entsoe_daily <- bind_rows(de_entsoe_daily, si_entsoe_daily) %>%
  mutate(
    region      = "Europe",
    country     = case_when(iso2 == "DE" ~ "Germany", iso2 == "SI" ~ "Slovenia"),
    value_mw    = value_mwh / 24,
    data_source = "entsoe"
  ) %>%
  select(iso2, region, country, source, date, value_mwh, value_mw, data_source) %>%
  arrange(iso2, source, date)

ember_monthly <- bind_rows(de_ember_monthly, cy_ember_monthly, si_ember_monthly) %>%
  arrange(iso2, source, date)

ember_yearly  <- bind_rows(de_ember_yearly,  cy_ember_yearly,  si_ember_yearly) %>%
  arrange(iso2, source, date)

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write_csv(entsoe_daily,  file.path(OUT_DIR, "power_entsoe_daily.csv"))
write_csv(ember_monthly, file.path(OUT_DIR, "power_ember_monthly.csv"))
write_csv(ember_yearly,  file.path(OUT_DIR, "power_ember_yearly.csv"))

cat(sprintf("Wrote ENTSOE daily : %d rows\n", nrow(entsoe_daily)))
cat(sprintf("Wrote Ember monthly: %d rows\n", nrow(ember_monthly)))
cat(sprintf("Wrote Ember yearly : %d rows\n", nrow(ember_yearly)))
