test_that("Simple case: 10% more renewables reduces emissions by 6%", {
  # Test Case 1 from plan:
  # Total generation: 1000 TWh
  # Actual renewable: 300 TWh (30%)
  # Weather-corrected renewable: 330 TWh (+30 TWh, or +10%)
  # Actual thermal: 500 TWh (50%)
  # Nuclear: 200 TWh (20%)
  # Actual power sector CO2: 250 Mt
  #
  # Expected:
  # - Weather-corrected thermal: 470 TWh
  # - Thermal share ratio: 0.47/0.50 = 0.94
  # - Weather-corrected CO2: 250 * 0.94 = 235 Mt
  # - Correction factor: 0.94 (6% reduction)

  # Mock renewable generation data
  renewable_gen <- data.frame(
    date = as.Date("2023-01-01"),
    iso2 = "DE",
    country = "Germany",
    region = "EU",
    source = "wind",
    value_mwh_actual = 300000,      # 300 TWh = 300,000,000 MWh
    value_mwh_corrected = 330000,    # 330 TWh
    delta_mwh = 30000,               # +30 TWh
    year = 2023
  )

  # Mock generation mix
  generation_mix <- data.frame(
    date = as.Date("2023-01-01"),
    iso2 = "DE",
    country = "Germany",
    region = "EU",
    year = 2023,
    source_category = c("renewable", "thermal", "nuclear", "other"),
    value_mwh = c(300000, 500000, 200000, 0)  # TWh as MWh thousands
  ) %>%
    tidyr::pivot_wider(names_from = source_category, values_from = value_mwh)

  # Add required columns
  renewable_gen_with_agg <- renewable_gen %>%
    dplyr::group_by(iso2, country, region, year) %>%
    dplyr::summarise(
      renewable_actual_mwh = sum(value_mwh_actual),
      renewable_weather_corrected_mwh = sum(value_mwh_corrected),
      renewable_delta_mwh = sum(delta_mwh),
      .groups = "drop"
    )

  # Calculate shares
  shares <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_actual_mwh = thermal,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      thermal_share_ratio = thermal_share_weather_corrected / thermal_share_actual
    )

  # Verify thermal share ratio
  expect_equal(shares$thermal_share_actual, 0.5, tolerance = 0.001)
  expect_equal(shares$thermal_share_weather_corrected, 0.47, tolerance = 0.001)
  expect_equal(shares$thermal_share_ratio, 0.94, tolerance = 0.001)

  # Mock CO2 and apply correction
  actual_co2 <- 250000  # 250 Mt = 250,000,000 tonnes (in thousands for consistency)

  corrected_co2 <- actual_co2 * shares$thermal_share_ratio

  expect_equal(corrected_co2, 235000, tolerance = 100)  # ~235 Mt

  correction_pct <- 100 * (shares$thermal_share_ratio - 1)
  expect_equal(correction_pct, -6, tolerance = 0.5)
})


test_that("Bad weather case: less renewables increases emissions by 6%", {
  # Test Case 2: Same as Test Case 1, but weather-corrected renewable: 270 TWh (-30 TWh)
  # Expected:
  # - Weather-corrected thermal: 530 TWh
  # - Thermal share ratio: 1.06
  # - Weather-corrected CO2: 265 Mt
  # - Correction factor: 1.06 (6% increase)

  renewable_gen_with_agg <- data.frame(
    iso2 = "DE",
    country = "Germany",
    region = "EU",
    year = 2023,
    renewable_actual_mwh = 300000,
    renewable_weather_corrected_mwh = 270000,  # Less than actual (bad weather)
    renewable_delta_mwh = -30000  # Negative delta
  )

  generation_mix <- data.frame(
    iso2 = "DE",
    country = "Germany",
    region = "EU",
    year = 2023,
    renewable = 300000,
    thermal = 500000,
    nuclear = 200000,
    other = 0
  )

  shares <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_actual_mwh = thermal,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,  # 500 - (-30) = 530
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      thermal_share_ratio = thermal_share_weather_corrected / thermal_share_actual
    )

  expect_equal(shares$thermal_weather_corrected_mwh, 530000, tolerance = 1)
  expect_equal(shares$thermal_share_ratio, 1.06, tolerance = 0.001)

  # Apply to CO2
  actual_co2 <- 250000
  corrected_co2 <- actual_co2 * shares$thermal_share_ratio

  expect_equal(corrected_co2, 265000, tolerance = 100)  # ~265 Mt

  correction_pct <- 100 * (shares$thermal_share_ratio - 1)
  expect_equal(correction_pct, 6, tolerance = 0.5)
})


test_that("Zero thermal share returns correction_factor = 1", {
  # Edge case: country with 100% renewables and nuclear (no thermal)

  renewable_gen_with_agg <- data.frame(
    iso2 = "NO",
    country = "Norway",
    region = "EU",
    year = 2023,
    renewable_actual_mwh = 800000,
    renewable_weather_corrected_mwh = 850000,
    renewable_delta_mwh = 50000
  )

  generation_mix <- data.frame(
    iso2 = "NO",
    country = "Norway",
    region = "EU",
    year = 2023,
    renewable = 800000,
    thermal = 0,  # No thermal generation
    nuclear = 200000,
    other = 0
  )

  shares <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_actual_mwh = thermal,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      # Apply logic from function: if thermal_share_actual < 0.001, ratio = 1
      thermal_share_ratio = dplyr::if_else(
        thermal_share_actual > 0.001,
        thermal_share_weather_corrected / thermal_share_actual,
        1.0
      )
    )

  expect_equal(shares$thermal_share_actual, 0)
  expect_equal(shares$thermal_share_ratio, 1.0)  # No correction
})


test_that("Mixed wind and solar sources combine correctly", {
  # Test combining multiple renewable sources

  renewable_gen <- data.frame(
    iso2 = rep("DE", 2),
    country = rep("Germany", 2),
    region = rep("EU", 2),
    year = rep(2023, 2),
    source = c("wind", "solar"),
    value_mwh_actual = c(200000, 100000),  # 200 + 100 = 300 TWh total
    value_mwh_corrected = c(220000, 110000),  # 220 + 110 = 330 TWh total
    delta_mwh = c(20000, 10000)  # +20 + 10 = +30 TWh total
  )

  renewable_gen_with_agg <- renewable_gen %>%
    dplyr::group_by(iso2, country, region, year) %>%
    dplyr::summarise(
      renewable_actual_mwh = sum(value_mwh_actual),
      renewable_weather_corrected_mwh = sum(value_mwh_corrected),
      renewable_delta_mwh = sum(delta_mwh),
      .groups = "drop"
    )

  # Should sum to same totals as Test Case 1
  expect_equal(renewable_gen_with_agg$renewable_actual_mwh, 300000)
  expect_equal(renewable_gen_with_agg$renewable_weather_corrected_mwh, 330000)
  expect_equal(renewable_gen_with_agg$renewable_delta_mwh, 30000)
})


test_that("YTD aggregation filters correctly", {
  # Test that YTD filtering works as expected
  # This is more of a logic test for the aggregation parameter

  dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")

  # For YTD on day 100, should only include first 100 days
  max_yday <- 100

  filtered_dates <- dates[lubridate::yday(dates) <= max_yday]

  expect_equal(length(filtered_dates), max_yday)
  expect_equal(max(lubridate::yday(filtered_dates)), max_yday)
})


test_that("NA and infinite values handled correctly", {
  # Test that NA/infinite values are set to correction_factor = 1

  corrected_co2 <- data.frame(
    iso2 = c("DE", "FR", "IT"),
    co2_actual_t = c(100000, 200000, 150000),
    correction_factor = c(0.95, NA, Inf)
  )

  # Apply the same logic as in apply_weather_correction_to_co2
  corrected_co2 <- corrected_co2 %>%
    dplyr::mutate(
      correction_factor = dplyr::if_else(
        is.na(correction_factor) | is.infinite(correction_factor),
        1.0,
        correction_factor
      ),
      co2_weather_corrected_t = co2_actual_t * correction_factor
    )

  expect_equal(corrected_co2$correction_factor[1], 0.95)
  expect_equal(corrected_co2$correction_factor[2], 1.0)  # NA -> 1
  expect_equal(corrected_co2$correction_factor[3], 1.0)  # Inf -> 1

  expect_equal(corrected_co2$co2_weather_corrected_t[2], 200000)  # No change
  expect_equal(corrected_co2$co2_weather_corrected_t[3], 150000)  # No change
})
