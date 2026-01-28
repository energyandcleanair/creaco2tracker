# Tests for get_weather_corrected_hydro with surrogate data

library(testthat)
library(dplyr)
library(lubridate)

test_that("get_weather_corrected_hydro with multiple countries, varying capacities, and exact calculations", {
  # Test with 3 countries, 3 years, varying capacities
  # DE: capacity increases (800 -> 1000 -> 1200 MW), CF varies (0.3 -> 0.4 -> 0.5)
  # FR: capacity constant (1000 MW), CF constant (0.4) - should have ratio = 1.0
  # IT: capacity decreases (1200 -> 1000 -> 800 MW), CF varies (0.5 -> 0.4 -> 0.3)

  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")

  # Generation data: value_mwh = CF * capacity_mw * 24
  pwr_generation <- bind_rows(
    # DE: varying capacity and CF
    tibble(
      date = dates,
      iso2 = "DE",
      source = "Hydro",
      value_mwh = case_when(
        year(date) == 2020 ~ 0.3 * 800 * 24,   # CF=0.3, cap=800MW -> 5760 MWh
        year(date) == 2021 ~ 0.4 * 1000 * 24,  # CF=0.4, cap=1000MW -> 9600 MWh
        year(date) == 2022 ~ 0.5 * 1200 * 24   # CF=0.5, cap=1200MW -> 14400 MWh
      )
    ),
    # FR: constant capacity and CF
    tibble(
      date = dates,
      iso2 = "FR",
      source = "Hydro",
      value_mwh = 0.4 * 1000 * 24  # CF=0.4, cap=1000MW -> 9600 MWh (constant)
    ),
    # IT: varying capacity and CF
    tibble(
      date = dates,
      iso2 = "IT",
      source = "Hydro",
      value_mwh = case_when(
        year(date) == 2020 ~ 0.5 * 1200 * 24,  # CF=0.5, cap=1200MW -> 14400 MWh
        year(date) == 2021 ~ 0.4 * 1000 * 24,  # CF=0.4, cap=1000MW -> 9600 MWh
        year(date) == 2022 ~ 0.3 * 800 * 24    # CF=0.3, cap=800MW -> 5760 MWh
      )
    )
  )

  # Capacity data with varying capacities
  mock_capacity_data <- bind_rows(
    tibble(
      date = dates,
      iso2 = "DE",
      source = "Hydro",
      value_mw = case_when(
        year(date) == 2020 ~ 800,
        year(date) == 2021 ~ 1000,
        year(date) == 2022 ~ 1200
      )
    ),
    tibble(
      date = dates,
      iso2 = "FR",
      source = "Hydro",
      value_mw = 1000  # Constant
    ),
    tibble(
      date = dates,
      iso2 = "IT",
      source = "Hydro",
      value_mw = case_when(
        year(date) == 2020 ~ 1200,
        year(date) == 2021 ~ 1000,
        year(date) == 2022 ~ 800
      )
    )
  )

  # Use local_mocked_bindings for reliable mocking in testthat 3.0+
  local_mocked_bindings(
    entsoe.get_installed_capacity = function(...) mock_capacity_data,
    .package = "creaco2tracker"
  )

  # Run function
  result <- suppressMessages(
    get_weather_corrected_hydro(
      iso2s = c("DE", "FR", "IT"),
      pwr_generation = pwr_generation,
      date_from = "2020-01-01",
      date_to = "2022-12-31",
      use_cache = FALSE
    )
  )

  # Calculate expected values for each country
  # DE: CFs = 0.3, 0.4, 0.5 -> avg CF = 0.4
  #     Ratios: 0.4/0.3 = 1.333, 0.4/0.4 = 1.0, 0.4/0.5 = 0.8
  # FR: CFs = 0.4, 0.4, 0.4 -> avg CF = 0.4
  #     Ratios: 0.4/0.4 = 1.0, 0.4/0.4 = 1.0, 0.4/0.4 = 1.0
  # IT: CFs = 0.5, 0.4, 0.3 -> avg CF = 0.4
  #     Ratios: 0.4/0.5 = 0.8, 0.4/0.4 = 1.0, 0.4/0.3 = 1.333

  result_summary <- result %>%
    filter(iso2 != "EU") %>%  # Exclude EU total for now
    mutate(year = year(date)) %>%
    group_by(iso2, year) %>%
    summarise(
      actual_mean = mean(value_mwh_actual),
      corrected_mean = mean(value_mwh_corrected),
      ratio = corrected_mean / actual_mean,
      .groups = "drop"
    )

  # DE: Verify exact calculations
  de_2020 <- result_summary %>% filter(iso2 == "DE", year == 2020)
  de_2021 <- result_summary %>% filter(iso2 == "DE", year == 2021)
  de_2022 <- result_summary %>% filter(iso2 == "DE", year == 2022)

  expect_equal(de_2020$actual_mean, 5760, tolerance = 0.1)
  expect_equal(de_2021$actual_mean, 9600, tolerance = 0.1)
  expect_equal(de_2022$actual_mean, 14400, tolerance = 0.1)
  expect_equal(de_2020$ratio, 4/3, tolerance = 0.001)  # 0.4/0.3 = 1.333...
  expect_equal(de_2021$ratio, 1.0, tolerance = 0.001)
  expect_equal(de_2022$ratio, 0.8, tolerance = 0.001)
  expect_equal(de_2020$corrected_mean, 5760 * (4/3), tolerance = 0.1)
  expect_equal(de_2022$corrected_mean, 14400 * 0.8, tolerance = 0.1)

  # FR: Constant CF, all ratios should be 1.0
  fr_all <- result_summary %>% filter(iso2 == "FR")
  expect_true(all(abs(fr_all$ratio - 1.0) < 0.001))
  expect_equal(fr_all$actual_mean, rep(9600, 3), tolerance = 0.1)
  expect_equal(fr_all$corrected_mean, rep(9600, 3), tolerance = 0.1)

  # IT: Verify exact calculations (opposite pattern from DE)
  it_2020 <- result_summary %>% filter(iso2 == "IT", year == 2020)
  it_2021 <- result_summary %>% filter(iso2 == "IT", year == 2021)
  it_2022 <- result_summary %>% filter(iso2 == "IT", year == 2022)

  expect_equal(it_2020$actual_mean, 14400, tolerance = 0.1)
  expect_equal(it_2021$actual_mean, 9600, tolerance = 0.1)
  expect_equal(it_2022$actual_mean, 5760, tolerance = 0.1)
  expect_equal(it_2020$ratio, 0.8, tolerance = 0.001)  # 0.4/0.5 = 0.8
  expect_equal(it_2021$ratio, 1.0, tolerance = 0.001)
  expect_equal(it_2022$ratio, 4/3, tolerance = 0.001)  # 0.4/0.3 = 1.333...
  expect_equal(it_2020$corrected_mean, 14400 * 0.8, tolerance = 0.1)
  expect_equal(it_2022$corrected_mean, 5760 * (4/3), tolerance = 0.1)

  # Verify that after correction, all years have the same CF (equal to average CF)
  # This is the key property: correction normalizes all years to average weather
  corrected_cf_check <- result %>%
    filter(iso2 != "EU") %>%
    mutate(year = year(date)) %>%
    left_join(
      mock_capacity_data %>%
        mutate(year = year(date)) %>%
        group_by(iso2, year) %>%
        summarise(capacity_mw = mean(value_mw), .groups = "drop"),
      by = c("iso2", "year")
    ) %>%
    group_by(iso2, year) %>%
    summarise(
      corrected_cf = mean(value_mwh_corrected) / (mean(capacity_mw) * 24),
      .groups = "drop"
    ) %>%
    group_by(iso2) %>%
    summarise(
      # All years should have the same CF after correction (the average CF)
      cf_std_dev = sd(corrected_cf),
      avg_cf = mean(corrected_cf),
      .groups = "drop"
    )

  # Standard deviation should be near zero (all years have same CF after correction)
  expect_true(all(corrected_cf_check$cf_std_dev < 0.001))

  # All countries should have average CF = 0.4 (by design of our test data)
  expect_true(all(abs(corrected_cf_check$avg_cf - 0.4) < 0.001))

  # Verify sum conservation for countries with constant capacity
  # If capacity is constant, sum should be conserved: sum(corrected) = sum(actual)
  sum_check <- result %>%
    filter(iso2 != "EU") %>%
    group_by(iso2) %>%
    summarise(
      sum_actual = sum(value_mwh_actual),
      sum_corrected = sum(value_mwh_corrected),
      .groups = "drop"
    ) %>%
    left_join(
      mock_capacity_data %>%
        group_by(iso2) %>%
        summarise(
          capacity_constant = n_distinct(value_mw) == 1,
          .groups = "drop"
        ),
      by = "iso2"
    )

  # FR has constant capacity, so sum should be conserved
  fr_sum <- sum_check %>% filter(iso2 == "FR")
  expect_equal(fr_sum$sum_actual, fr_sum$sum_corrected, tolerance = 0.1)

  # DE and IT have varying capacity, so sum is NOT conserved
  # (The correction normalizes to average weather, but capacity changes affect totals)
  de_sum <- sum_check %>% filter(iso2 == "DE")
  it_sum <- sum_check %>% filter(iso2 == "IT")
  expect_true(abs(de_sum$sum_actual - de_sum$sum_corrected) > 100)  # Should differ
  expect_true(abs(it_sum$sum_actual - it_sum$sum_corrected) > 100)  # Should differ
})


