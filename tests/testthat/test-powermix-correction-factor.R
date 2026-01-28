test_that("calculate_powermix_correction_factor handles basic case correctly", {
  # Case 1: Good weather year - more renewable generation
  # If renewables generated 30,000 MWh more than actual,
  # thermal would have been 30,000 MWh less
  # 500,000 - 30,000 = 470,000 → correction factor = 470,000/500,000 = 0.94

  thermal <- c(500000, 600000, 550000)
  renewable_delta <- c(30000, 30000, 30000)  # More renewable = positive delta

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # Expected: (thermal - renewable_delta) / thermal
  # But capped at max(thermal) = 600,000
  expected <- c(
    (500000 - 30000) / 500000,  # 0.94
    (600000 - 30000) / 600000,  # 0.95
    (550000 - 30000) / 550000   # 0.945
  )

  expect_equal(result, expected, tolerance = 0.001)
})


test_that("calculate_powermix_correction_factor handles bad weather (negative delta)", {
  # Case 2: Bad weather year - less renewable generation
  # If renewables generated 20,000 MWh less than actual,
  # thermal would have been 20,000 MWh more
  # 500,000 + 20,000 = 520,000 → correction factor = 520,000/500,000 = 1.04

  thermal <- c(500000, 600000, 550000)
  renewable_delta <- c(-20000, -20000, -20000)  # Less renewable = negative delta

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # Expected: (thermal - renewable_delta) / thermal
  # thermal - (-20000) = thermal + 20000
  # But capped at max(thermal) = 600,000
  expected <- c(
    (500000 - (-20000)) / 500000,  # 520,000 / 500,000 = 1.04
    (600000 - (-20000)) / 600000,  # 620,000 / 600,000 = 1.033 BUT capped at 600,000 → 1.0
    (550000 - (-20000)) / 550000   # 570,000 / 550,000 = 1.036
  )
  expected[2] <- 600000 / 600000  # Cap at historical max

  expect_equal(result, expected, tolerance = 0.001)
})


test_that("calculate_powermix_correction_factor caps at historical max thermal", {
  # Case 3: Historical cap constraint
  # In countries with low thermal, prevent unrealistic correction factors
  # Max thermal = 100,000, but delta suggests 120,000
  # Should cap at 100,000 instead of allowing 120,000

  thermal <- c(50000, 100000, 80000)  # Max = 100,000
  renewable_delta <- c(-60000, -60000, -60000)  # Bad weather: need way more thermal

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # Without cap: (50000 + 60000) / 50000 = 2.2 (unrealistic!)
  # With cap: min(100000, 110000) / 50000 = 2.0
  # For middle value: min(100000, 160000) / 100000 = 1.0
  # For last value: min(100000, 140000) / 80000 = 1.25

  expected <- c(
    pmin(100000, 50000 - (-60000)) / 50000,   # 100,000 / 50,000 = 2.0 (capped)
    pmin(100000, 100000 - (-60000)) / 100000, # 100,000 / 100,000 = 1.0 (capped)
    pmin(100000, 80000 - (-60000)) / 80000    # 100,000 / 80,000 = 1.25 (capped)
  )

  expect_equal(result, expected, tolerance = 0.001)
  expect_true(all(result <= 2.5))  # All values reasonably bounded
})


test_that("calculate_powermix_correction_factor prevents negative thermal", {
  # Case 4: Non-negativity constraint
  # If renewable delta exceeds thermal, ensure we don't get negative

  thermal <- c(100000, 200000, 150000)
  renewable_delta <- c(120000, 50000, 150000)  # Extreme good weather

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # Expected: max(0, thermal - renewable_delta) / thermal
  # First: max(0, 100000 - 120000) / 100000 = 0 / 100000 = 0
  # Second: (200000 - 50000) / 200000 = 0.75
  # Third: max(0, 150000 - 150000) / 150000 = 0

  expected <- c(
    pmax(0, pmin(200000, 100000 - 120000)) / 100000,  # 0
    pmax(0, pmin(200000, 200000 - 50000)) / 200000,   # 0.75
    pmax(0, pmin(200000, 150000 - 150000)) / 150000   # 0
  )

  expect_equal(result, expected, tolerance = 0.001)
  expect_true(all(result >= 0))  # All non-negative
})


test_that("calculate_powermix_correction_factor handles zero delta (no weather effect)", {
  # Case 5: No weather correction needed
  # Delta = 0 means actual weather = average weather

  thermal <- c(500000, 600000, 550000)
  renewable_delta <- c(0, 0, 0)

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # Expected: thermal / thermal = 1.0 for all
  expect_equal(result, c(1, 1, 1), tolerance = 0.001)
})


test_that("calculate_powermix_correction_factor handles edge cases", {
  # Case 6: Single value
  thermal <- 500000
  renewable_delta <- 30000

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)
  expect_equal(result, (500000 - 30000) / 500000, tolerance = 0.001)

  # Case 7: Very small thermal (edge case for low-thermal countries)
  thermal <- c(100, 50, 75)  # Max = 100
  renewable_delta <- c(10, 10, 10)

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  # All capped at max = 100
  expected <- c(
    (100 - 10) / 100,  # 0.9
    pmin(100, 50 - 10) / 50,  # 40/50 = 0.8
    pmin(100, 75 - 10) / 75   # 65/75 = 0.867
  )

  expect_equal(result, expected, tolerance = 0.001)
})


test_that("calculate_powermix_correction_factor matches inline formula", {
  # Verify the standalone function produces identical results to the original inline formula

  thermal <- c(500000, 600000, 400000, 550000)
  renewable_delta <- c(30000, -20000, 50000, 0)

  # Original inline formula
  inline_result <- pmax(0, pmin(max(thermal), (thermal - renewable_delta)) / thermal)

  # New standalone function
  function_result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  expect_equal(function_result, inline_result)
})


test_that("calculate_powermix_correction_factor real-world scenario", {
  # Real scenario: Germany 2024
  # Bad weather for renewables → higher emissions

  # Hypothetical: Germany had 500 TWh thermal in a year
  # Weather was worse than average, so renewables were 30 TWh below average
  # Under average weather, thermal would have been 30 TWh less (470 TWh)
  # Correction factor = 470/500 = 0.94

  thermal <- 500000000  # 500 TWh in MWh
  renewable_delta <- 30000000  # 30 TWh more renewable under avg weather

  result <- calculate_powermix_correction_factor(thermal, renewable_delta)

  expect_equal(result, 0.94, tolerance = 0.001)

  # This means: actual emissions should be multiplied by 0.94
  # to get weather-corrected emissions (6% lower due to good renewable conditions)
})


test_that("EU emission-weighted average is correct", {
  # Scenario: 3 countries with different thermal generation and correction factors
  # Country A: 500 TWh thermal, correction = 0.90
  # Country B: 300 TWh thermal, correction = 1.05
  # Country C: 200 TWh thermal, correction = 0.95

  thermal <- c(500000, 300000, 200000)
  correction_factors <- c(0.90, 1.05, 0.95)

  # EU weighted average = (0.90*500 + 1.05*300 + 0.95*200) / (500 + 300 + 200)
  #                     = (450 + 315 + 190) / 1000
  #                     = 955 / 1000
  #                     = 0.955

  eu_weighted_avg <- sum(correction_factors * thermal) / sum(thermal)

  expect_equal(eu_weighted_avg, 0.955, tolerance = 0.001)

  # Verify this is different from simple average
  simple_avg <- mean(correction_factors)
  expect_equal(simple_avg, 0.9667, tolerance = 0.001)
  expect_false(abs(eu_weighted_avg - simple_avg) < 0.001)  # Should be different
})
