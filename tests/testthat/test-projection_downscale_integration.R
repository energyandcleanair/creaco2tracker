library(testthat)
library(dplyr)
library(lubridate)
library(tibble)
library(purrr)
library(tidyr)

# Mock data creation functions
create_growing_trend_data <- function(value_init = 0, add_per_period = 100,
                                     date_min = as.Date("2020-01-01"), date_max = as.Date("2020-05-01"),
                                     by = "month", # "day" or "month"
                                     fuels = c("coal", "gas"), sectors = c("electricity", "all")) {
  # Create sequence based on period type
  date_sequence <- seq.Date(date_min, date_max, by = by)

  # Generate growing values: value_init + add_per_period * period_number
  period_numbers <- 0:(length(date_sequence) - 1)
  values <- value_init + add_per_period * period_numbers

  # Create the structure: each period gets its corresponding value
  date_value_pairs <- tibble(
    date = date_sequence,
    value = values
  )

  # Then expand to all fuel-sector combinations
  expand_grid(
    fuel = fuels,
    sector = sectors
  ) %>%
    cross_join(date_value_pairs)
}

create_pwr_generation_mock <- function(value_init = 0, add_daily = 10,
                                      date_min = as.Date("2020-01-01"), date_max = as.Date("2020-07-15"),
                                      fuels = c("coal", "gas"), sectors = c("electricity")) {
  # Map fuels to power generation sources
  fuel_to_source <- c("coal" = "Coal", "gas" = "Fossil Gas")

  # Use the unified trend data function for daily data
  trend_data <- create_growing_trend_data(
    value_init = value_init,
    add_per_period = add_daily,
    date_min = date_min,
    date_max = date_max,
    by = "day",
    fuels = fuels,
    sectors = sectors
  )

  # Convert to power generation format (daily data, no month column)
  trend_data %>%
    mutate(
      iso2 = "EU",
      source = fuel_to_source[fuel],
      value_mwh = value,
      value_mw = value
    ) %>%
    select(-value) %>%
    arrange(iso2, source, date)
}

create_gas_demand_mock <- function(value_init = 0, add_daily = 2,
                                  date_min = as.Date("2020-01-01"), date_max = as.Date("2020-07-15"),
                                  fuels = c("gas"), sectors = c("all")) {
  # Use the unified trend data function for daily data
  trend_data <- create_growing_trend_data(
    value_init = value_init,
    add_per_period = add_daily,
    date_min = date_min,
    date_max = date_max,
    by = "day",
    fuels = fuels,
    sectors = sectors
  )

  # Convert to gas demand format (daily data, no month column)
  trend_data %>%
    mutate(
      iso2 = "EU",
      unit = "m3"
    )
}

create_eurostat_cons_mock <- function(value_init = 0, add_daily = 100,
                                     date_min = as.Date("2020-01-01"), date_max = as.Date("2020-05-01"),
                                     fuels = c("coal", "gas"), sectors = c("electricity", "all")) {
  # Create daily data first, then sum to monthly
  # Ensure date_max represents the last day of the month
  actual_date_max <- ceiling_date(date_max, "month") - 1

  daily_data <- create_growing_trend_data(
    value_init = value_init,
    add_per_period = add_daily,
    date_min = date_min,
    date_max = actual_date_max,
    by = "day",
    fuels = fuels,
    sectors = sectors
  )

  # Sum daily values to monthly totals
  monthly_data <- daily_data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month, fuel, sector) %>%
    summarise(values = sum(value), .groups = "drop") %>%
    rename(time = month)

  # Map fuels to SIEC codes
  fuel_to_siec <- c("coal" = SIEC_HARD_COAL, "gas" = SIEC_NATURAL_GAS)

  # Map fuels to FUEL constants
  fuel_to_fuel_constant <- c("coal" = FUEL_COAL, "gas" = FUEL_GAS)

  monthly_data %>%
    mutate(
      iso2 = "EU",
      siec_code = fuel_to_siec[fuel],
      fuel = fuel_to_fuel_constant[fuel],
      unit = "Thousand tonnes"
    ) %>%
    select(iso2, time, siec_code, fuel, sector, unit, values)
}


test_that("Projection and downscaling work well together", {

  # We set up a case where eurostat, pwr_generation and gas_demand
  # grow at a constant incremental value per day
  # The linear relationship between the monthly values
  # should then be perfect
  # And daily co2 values should then reproduce a constant slope,
  # following pwr generation and gas demand
  fuels <- c("coal", "gas")
  sectors <- c("electricity", "all")

  # Start at 0 since LM fitting is without intercept
  # Also the LM fits on minimum one year so we need at least a year of data
  # Also there is a hard-coded rule in downscale around 2021... not great
  # For now testing after that date
  pwr_generation <- create_pwr_generation_mock(
    value_init = 0,
    add_daily = 10,
    date_min = as.Date("2024-01-01"), date_max = as.Date("2025-07-15")
  )
  ggplot(pwr_generation, aes(x = date, y = value_mw, color = fuel)) + geom_line()

  gas_demand <- create_gas_demand_mock(
    value_init = 0,
    add_daily = 2,
    date_min = as.Date("2024-01-01"), date_max = as.Date("2025-07-15")
  )
  ggplot(gas_demand, aes(x = date, y = value, color = fuel)) + geom_line()

  eurostat_cons <- create_eurostat_cons_mock(
    value_init = 0,
    add_daily = 100,
    date_min = as.Date("2024-01-01"),
    date_max = as.Date("2025-05-01")
  )
  ggplot(eurostat_cons %>% filter(sector=='electricity'), aes(x = time, y = values, color = fuel)) + geom_line() + geom_point()

  # Minimal eurostat_indprod (unused in this scenario)
  eurostat_indprod <- tibble(
    iso2 = character(),
    time = as.Date(character()),
    nace_r2_code = character(),
    unit = character(),
    s_adj = character(),
    values = numeric()
  )

  # Run the pipeline
  co2_unprojected <- get_co2_from_eurostat_cons(
    eurostat_cons,
    diagnostics_folder = NULL,
    ncv_source = "ipcc"
  )
  ggplot(co2_unprojected, aes(x = date, y = value, color = fuel)) + geom_line() + geom_point()


  # Test individual projection functions
  # Need to provide dts_month that includes the months we want to project to
  projection_months <- unique(floor_date(pwr_generation$date, "month"))

  co2_elec_projected <- project_until_now_elec(
    co2_unprojected,
    pwr_generation = pwr_generation,
    dts_month = projection_months
  )

  co2_gas_projected <- project_until_now_gas(
    co2_unprojected,
    gas_demand = gas_demand,
    dts_month = projection_months
  )

  ggplot(bind_rows(co2_gas_projected %>% mutate(type='projected'),
                   co2_unprojected %>% mutate(type='unprojected')),
         aes(x = date, y = value, color = fuel, linetype=type)) + geom_line() + geom_point() + facet_wrap(~sector)


  # Combine the projected results
  co2_monthly <- bind_rows(
    co2_elec_projected,
    co2_gas_projected
  ) %>%
    dplyr::distinct(iso2, date, fuel, sector, unit, value, .keep_all = TRUE) %>%
    mutate(estimate = "central")

  # Test 1: Projection coverage - should include months 2020-01 to 2020-07
  projected_months <- unique(floor_date(co2_monthly$date, "month"))
  expect_true(setequal(projection_months, projected_months))

  # Then downscale to daily
  co2_daily <- downscale_daily(co2_monthly, pwr_generation = pwr_generation, gas_demand = gas_demand, cut_latest_days=0)

  # Test 3: Check that daily data covers the expected date range
  co2_daily_valid_dates <- co2_daily %>%
    filter(!is.na(value)) %>%
    pull(date) %>%
    unique()

  expected_daily_dates <- seq.Date(min(co2_monthly$date), max(pwr_generation$date), by = "day")
  expect_true(setequal(co2_daily_valid_dates, expected_daily_dates))

  # Visual check
  ggplot(co2_daily, aes(x = date, y = value, color = fuel)) +
    geom_line() +
    facet_wrap(~sector) +
    scale_x_date(date_minor_breaks = "1 month")

  # Check that these are straight lines (except for coal - all that has no proxy)
  deltas <- co2_daily %>%
    group_by(fuel, sector) %>%
    arrange(desc(date)) %>%
    mutate(
      delta = value - lag(value)) %>%
    group_by(fuel, sector) %>%
    summarise(delta_mean = mean(delta, na.rm = TRUE),
              delta_sd = sd(delta, na.rm = TRUE),
              .groups = "drop")

  # Except for coal sector= all, the sd should be very close to 0
  expect_true(max(abs(deltas$delta_sd[deltas$fuel != FUEL_COAL | deltas$sector != SECTOR_ALL])) < 0.1)
})





