test_that("coking diagnostic data retains context only for affected countries", {
  diagnostics <- tibble::tribble(
    ~iso2, ~time, ~frequency, ~original_value, ~resolved_value, ~method,
    ~conflict, ~duplicate,
    "DE", as.Date("2024-01-01"), "annual", 10, 10, "reported", FALSE, FALSE,
    "DE", as.Date("2025-01-01"), "annual", 20, 20, "reported", FALSE, FALSE,
    "FR", as.Date("2024-01-01"), "annual", 0, 12, "resolved_monthly_sum", TRUE, FALSE,
    "FR", as.Date("2025-01-01"), "annual", 14, 14, "reported", FALSE, FALSE,
    "FR", as.Date("2025-02-01"), "monthly", NA, 1, "steel_activity", FALSE, FALSE,
    "CY", as.Date("2025-01-01"), "annual", NA, 0, "previous_year", FALSE, FALSE
  )

  annual <- coal_coking_diagnostic_data(diagnostics, "annual")
  expect_setequal(unique(annual$iso2), "FR")
  expect_equal(nrow(annual), 4)
  expect_setequal(unique(annual$series), c("Original", "Resolved"))
  expect_true(all(annual$changed[annual$time == as.Date("2024-01-01")]))
  expect_false(any(annual$changed[annual$time == as.Date("2025-01-01")]))
})


test_that("coking diagnostic plots compare original and resolved series", {
  diagnostics <- tibble::tibble(
    iso2 = "FR",
    time = as.Date(c("2024-01-01", "2024-02-01")),
    frequency = "monthly",
    original_value = c(10, NA_real_),
    resolved_value = c(10, 12),
    method = c("reported", "steel_activity"),
    conflict = FALSE,
    duplicate = FALSE
  )

  plot <- plot_coal_coking_diagnostics(diagnostics, "monthly")
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Monthly coking corrections")

  unchanged <- diagnostics %>%
    mutate(original_value = resolved_value, method = "reported")
  expect_null(plot_coal_coking_diagnostics(unchanged, "monthly"))
})


test_that("coking diagnostic data validates its provenance input", {
  expect_error(
    coal_coking_diagnostic_data(tibble::tibble(iso2 = "FR"), "annual"),
    "missing required provenance columns"
  )
})
