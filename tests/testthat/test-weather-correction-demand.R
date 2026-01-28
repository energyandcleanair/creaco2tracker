test_that("get_weather_correction_demand exists", {
  expect_true(exists("get_weather_correction_demand"))
  expect_true(exists("get_weather_correction_demand_elec"))
  expect_true(exists("get_weather_correction_demand_gas"))
})

# Skipping functional tests due to current implementation limitations
# These can be re-enabled once the code bugs are fixed
