test_that("Weather correction factor calculation is correct", {
  # Test: 10% more renewables → 6% reduction in thermal share

  renewable_gen_with_agg <- data.frame(
    iso2 = "DE",
    country = "Germany",
    region = "EU",
    year = 2023,
    renewable_actual_mwh = 300000,
    renewable_weather_corrected_mwh = 330000,
    renewable_delta_mwh = 30000
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

  correction <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      correction_factor = thermal_share_weather_corrected / thermal_share_actual
    )

  expect_equal(correction$thermal_share_actual, 0.5, tolerance = 0.001)
  expect_equal(correction$thermal_share_weather_corrected, 0.47, tolerance = 0.001)
  expect_equal(correction$correction_factor, 0.94, tolerance = 0.001)
})


test_that("Bad weather case increases correction factor", {
  # Less renewables than climatology → more thermal needed

  renewable_gen_with_agg <- data.frame(
    iso2 = "FR",
    country = "France",
    region = "EU",
    year = 2023,
    renewable_actual_mwh = 300000,
    renewable_weather_corrected_mwh = 270000,
    renewable_delta_mwh = -30000
  )

  generation_mix <- data.frame(
    iso2 = "FR",
    country = "France",
    region = "EU",
    year = 2023,
    renewable = 300000,
    thermal = 500000,
    nuclear = 200000,
    other = 0
  )

  correction <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      correction_factor = thermal_share_weather_corrected / thermal_share_actual
    )

  expect_equal(correction$thermal_weather_corrected_mwh, 530000)
  expect_equal(correction$correction_factor, 1.06, tolerance = 0.001)
  expect_gt(correction$correction_factor, 1.0)
})


test_that("Zero thermal share returns correction_factor = 1", {
  # Country with 100% renewables and nuclear (no thermal)

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
    thermal = 0,
    nuclear = 200000,
    other = 0
  )

  correction <- generation_mix %>%
    dplyr::left_join(renewable_gen_with_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      total_generation_mwh = renewable + thermal + nuclear + other,
      thermal_share_actual = thermal / total_generation_mwh,
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,
      thermal_share_weather_corrected = thermal_weather_corrected_mwh / total_generation_mwh,
      correction_factor = dplyr::if_else(
        thermal_share_actual > 0.001,
        thermal_share_weather_corrected / thermal_share_actual,
        1.0
      )
    )

  expect_equal(correction$thermal_share_actual, 0)
  expect_equal(correction$correction_factor, 1.0)
})


test_that("NA and infinite correction factors handled correctly", {
  correction <- data.frame(
    iso2 = c("DE", "FR", "IT"),
    correction_factor_raw = c(0.95, NA, Inf)
  )

  correction <- correction %>%
    dplyr::mutate(
      correction_factor = dplyr::if_else(
        is.na(correction_factor_raw) | is.infinite(correction_factor_raw),
        1.0,
        correction_factor_raw
      )
    )

  expect_equal(correction$correction_factor[1], 0.95)
  expect_equal(correction$correction_factor[2], 1.0)
  expect_equal(correction$correction_factor[3], 1.0)
})
