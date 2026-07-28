test_that("Carbon Monitor validation data includes country and EU annual rows", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  carbon_monitor <- tibble(
    country = "Austria",
    date = format(dates, "%d/%m/%Y"),
    sector = "Power",
    value = 1
  )

  local_mocked_bindings(
    load_carbonmonitor_raw = function() carbon_monitor
  )

  result <- suppressWarnings(
    load_carbonmonitor("Carbon Monitor", c("AT", "EU"))
  )

  expect_setequal(result$iso2, c("AT", "EU"))
  expect_equal(result$value[result$iso2 == "AT"], 366)
  expect_equal(result$value[result$iso2 == "EU"], 366)
})
