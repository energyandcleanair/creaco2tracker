test_that("held-out fuel observations and provenance cannot enter the background", {
  context <- tidyr::crossing(iso2 = c("GR", "SE"),
    siec = c(SIEC_HARD_COAL, SIEC_BROWN_COAL),
    time = as.Date(c("2024-01-01", "2025-01-01"))) %>%
    mutate(fuel = FUEL_COAL, unit = "THS_T", sector = SECTOR_OTHERS, values = 1)
  attr(context, "coal_allocation") <- mutate(context, original_value = values)
  result <- coal_replay_background(context, SIEC_HARD_COAL, "GR")
  expect_equal(nrow(result), 6L)
  expect_false(any(result$iso2 == "GR" & result$siec == SIEC_HARD_COAL))
  expect_true(any(result$iso2 == "SE" & result$siec == SIEC_HARD_COAL))
  expect_true(any(result$iso2 == "GR" & result$siec == SIEC_BROWN_COAL))
  provenance <- attr(result, "coal_allocation")
  expect_equal(nrow(provenance), nrow(result))
  expect_false(any(provenance$iso2 == "GR" & provenance$siec == SIEC_HARD_COAL))
  expect_equal(nrow(coal_replay_background(context, SIEC_HARD_COAL, c("GR", "SE"))), 4L)
  expect_equal(coal_replay_background(context, SIEC_OIL_SHALE, "GR"), context)
})
