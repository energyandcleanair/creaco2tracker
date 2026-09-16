library(testthat)
library(dplyr)

coal_experiment_env <- new.env(parent = globalenv())
sys.source(
  test_path("..", "..", "scripts", "investigate_eu_coal_sector_imputation.R"),
  envir = coal_experiment_env
)

test_that("coal experiment distinguishes validated targets from residual sectors", {
  monthly <- tidyr::crossing(
    iso2 = "GR", year = 2024L, month = 1:2, siec = "C0200",
    sector = c("electricity", "non_power")
  ) %>%
    mutate(
      consumption_kt = if_else(sector == "electricity", 10, 2),
      production_kt = 12, imports_kt = 0, exports_kt = 0, stock_draw_kt = 0
    )
  annual <- tibble::tribble(
    ~iso2, ~year, ~siec, ~sector, ~annual_kt,
    "GR", 2024L, "C0200", "electricity", 120,
    "GR", 2024L, "C0200", "non_power", 24
  )
  ember <- tibble::tibble(iso2 = "GR", year = 2024L, month = 1:2, coal_twh = 1)

  result <- coal_experiment_env$coal_prepare_data(monthly, annual, ember)

  expect_equal(
    result$monthly %>% filter(sector == "total") %>% pull(consumption_kt),
    c(12, 12)
  )
  expect_equal(
    result$monthly %>% filter(sector == "non_power") %>% distinct(target_status) %>% pull(),
    "unvalidated_residual"
  )
  audit <- coal_experiment_env$coal_emissions_audit(result$monthly)
  expect_true(audit$include_in_validated_co2[audit$sector == "electricity"])
  expect_false(audit$include_in_validated_co2[audit$sector == "total"])
})

test_that("accounting requires three recent complete reconciled years", {
  reconciliation <- tibble::tibble(
    iso2 = "SE", year = 2020:2023, siec = "C0100",
    within_bounds = c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_true(coal_experiment_env$coal_accounting_eligible(
    reconciliation, "SE", "C0100", 2024
  ))
  reconciliation$within_bounds[reconciliation$year == 2022] <- FALSE
  expect_false(coal_experiment_env$coal_accounting_eligible(
    reconciliation, "SE", "C0100", 2024
  ))
  expect_false(coal_experiment_env$coal_accounting_eligible(
    reconciliation, "SE", "C0100", 2022
  ))
})

test_that("Ember is used only for complete power targets", {
  train <- tidyr::crossing(
    iso2 = c("GR", "DE"), year = 2020:2022, month = 1:12
  ) %>%
    mutate(sector = "electricity", consumption_kt = 2 * month, coal_twh = month)
  target <- tibble::tibble(
    iso2 = "GR", year = 2023L, month = 1:6, sector = "electricity", coal_twh = 1:6
  )

  prediction <- coal_experiment_env$coal_predict_ember(train, "GR", target)
  expect_equal(prediction, 2 * (1:6))
  expect_true(all(is.na(coal_experiment_env$coal_predict_ember(
    train, "GR", mutate(target, sector = "non_power")
  ))))
  expect_true(all(is.na(coal_experiment_env$coal_predict_ember(
    train, "GR", mutate(target, coal_twh = replace(coal_twh, 2, NA_real_))
  ))))
})

test_that("historical fallback uses only pre-holdout same-month values", {
  train <- tidyr::crossing(year = 2020:2022, month = 1:6) %>%
    mutate(consumption_kt = year - 2019 + month)
  target <- tibble::tibble(year = 2023L, month = 1:6)

  one_year <- coal_experiment_env$coal_predict_historical(train, target, 1)
  three_year <- coal_experiment_env$coal_predict_historical(train, target, 3)

  expect_equal(one_year, 3 + 1:6)
  expect_equal(three_year, 2 + 1:6)
})

test_that("annual reconciliation flags differences over five percent", {
  data <- tidyr::crossing(
    iso2 = "SE", year = 2024L, month = 1:12, siec = "C0100",
    sector = "total", target_status = "validated_total"
  ) %>%
    mutate(consumption_kt = 11)
  annual <- tibble::tibble(
    iso2 = "SE", year = 2024L, siec = "C0100", sector = "total", annual_kt = 120
  )

  result <- coal_experiment_env$coal_annual_reconciliation(data, annual, 0.05)

  expect_false(result$within_bounds)
  expect_equal(result$difference_kt, 12)
  expect_equal(result$source_class, "reported")
})

test_that("chronological sequence ignores scores from the holdout year", {
  scores <- tidyr::crossing(
    iso2 = c("DE", "GR"), year = 2015:2021,
    model = c("historical_level_1y", "ember_power")
  ) %>%
    mutate(
      siec = "C0200", sector = "electricity", target_status = "validated_power",
      eligible = TRUE, h1_actual_kt = 100,
      absolute_change_error_co2_t = if_else(
        model == "ember_power" & year < 2021, 1, if_else(model == "ember_power", 1000, 10)
      ),
      absolute_change_error_kt = absolute_change_error_co2_t,
      change_error_kt = absolute_change_error_kt,
      monthly_mae_kt = absolute_change_error_kt
    )
  settings <- list(min_evaluation_years = 5L, min_activity_coverage = 0.70)

  sequence <- coal_experiment_env$coal_sequence_before_year(
    scores, 2021, "C0200", "electricity", "validated_power", settings
  )

  expect_equal(sequence[[1]], "ember_power")
})

test_that("uncertainty replays the target missing-cell pattern", {
  predictions <- tibble::tribble(
    ~iso2, ~siec, ~target_status, ~eligible, ~year, ~month, ~actual_kt, ~predicted_kt,
    "SE", "C0100", "validated_power", TRUE, 2023L, 1L, 10, 11,
    "SE", "C0100", "validated_power", TRUE, 2024L, 1L, 10, 9,
    "DE", "C0100", "validated_power", TRUE, 2023L, 1L, 100, 200
  )
  target <- tibble::tibble(
    iso2 = "SE", siec = "C0100", target_status = "validated_power",
    method = "historical_level_1y", month = 1L
  )
  factors <- tibble::tibble(iso2 = c("SE", "DE"), siec = "C0100", co2_t_per_kt = 2)

  result <- coal_experiment_env$coal_target_pattern_uncertainty(
    predictions, target, factors, central_change_co2_t = 10
  )

  expect_equal(result$historical_years, 2L)
  expect_equal(result$target_cells, 1L)
  expect_equal(result$lower_change_co2_t, 8.2)
  expect_equal(result$upper_change_co2_t, 11.8)
})
