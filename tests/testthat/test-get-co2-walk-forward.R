library(testthat)
library(dplyr)


test_that(
  "walk-forward month ends use calendar month ends",
  {
    target_months <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01"))

    month_ends <- getFromNamespace(
      ".get_co2_walk_forward_month_ends",
      "creaco2tracker"
    )(target_months)

    expect_equal(month_ends, as.Date(c("2025-01-31", "2025-02-28", "2025-03-31")))
  }
)


test_that(
  "walk-forward comparison keeps only months available in each as-of run",
  {
    target_months <- as.Date(c("2025-01-01", "2025-02-01"))
    final_co2 <- tibble(
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      date = target_months,
      value = c(100, 200)
    )
    walk_forward_co2 <- tibble(
      as_of_date = as.Date(c("2025-01-31", "2025-02-28", "2025-02-28")),
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      date = as.Date(c("2025-01-01", "2025-01-01", "2025-02-01")),
      value = c(90, 95, 220)
    )

    comparison <- getFromNamespace(
      ".compare_get_co2_walk_forward",
      "creaco2tracker"
    )(
      walk_forward_co2 = walk_forward_co2,
      final_co2 = final_co2,
      target_months = target_months
    )

    expect_equal(nrow(comparison), 3)
    expect_equal(
      comparison %>%
        filter(as_of_date == as.Date("2025-01-31")) %>%
        pull(target_month),
      as.Date("2025-01-01")
    )
    expect_equal(
      comparison %>%
        filter(as_of_date == as.Date("2025-02-28"), target_month == as.Date("2025-02-01")) %>%
        pull(diff),
      20
    )
    expect_equal(
      comparison %>%
        filter(as_of_date == as.Date("2025-02-28"), target_month == as.Date("2025-02-01")) %>%
        pull(pct_diff),
      0.1
    )
  }
)


test_that(
  "walk-forward workflow protects controlled get_co2 arguments",
  {
    expect_error(
      getFromNamespace(
        ".validate_get_co2_walk_forward_args",
        "creaco2tracker"
      )(list(date_to = "2025-01-31")),
      "date_to"
    )
    expect_error(
      getFromNamespace(
        ".validate_get_co2_walk_forward_args",
        "creaco2tracker"
      )(list(data_masking = DATA_MASKING_NONE)),
      "data_masking"
    )
  }
)


test_that(
  "walk-forward masking summary uses annual publication months",
  {
    masking_cutoffs <- getFromNamespace(
      ".summarise_get_co2_walk_forward_masking",
      "creaco2tracker"
    )(
      as_of_dates = as.Date(c("2025-01-31", "2025-02-01", "2025-07-01")),
      run_type = c("walk_forward", "walk_forward", "final"),
      lags = default_source_lags(),
      publication_months = default_source_publication_months()
    )

    ember_cutoffs <- masking_cutoffs %>%
      filter(source_name == "ember_power_yearly") %>%
      arrange(as_of_date)
    eurostat_cutoffs <- masking_cutoffs %>%
      filter(source_name == "eurostat_gas_yearly") %>%
      arrange(as_of_date)

    expect_equal(ember_cutoffs$mask_date_from, c("2024-01-01", "2025-01-01", "2025-01-01"))
    expect_equal(
      eurostat_cutoffs$mask_date_from,
      c("2024-01-01", "2024-01-01", "2025-01-01")
    )
  }
)
