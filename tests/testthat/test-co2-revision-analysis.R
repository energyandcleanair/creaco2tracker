library(testthat)
library(dplyr)

make_mock_revision_analysis_data <- function(
  country_iso2s = c("DE", "FR"),
  fuels = c("coal", "gas"),
  sectors = c(SECTOR_ELEC, SECTOR_OTHERS),
  target_months = seq.Date(as.Date("2024-01-01"), as.Date("2025-06-01"), by = "month"),
  vintage_months = seq.Date(as.Date("2025-01-01"), as.Date("2025-06-01"), by = "month"),
  reference_vintage_month = as.Date("2025-12-01")
) {
  component_reference <- bind_rows(lapply(seq_along(country_iso2s), function(i_country) {
    country <- country_iso2s[[i_country]]

    bind_rows(lapply(seq_along(fuels), function(i_fuel) {
      fuel <- fuels[[i_fuel]]

      bind_rows(lapply(seq_along(sectors), function(i_sector) {
        sector <- sectors[[i_sector]]

        bind_rows(lapply(seq_along(target_months), function(i_month) {
          target_month <- target_months[[i_month]]
          reference_value <- 1000000 +
            i_country * 150000 +
            i_fuel * 60000 +
            i_sector * 30000 +
            i_month * 4000

          if (country == "FR" && fuel == "gas" && sector == SECTOR_OTHERS) {
            reference_value <- 0
          }
          if (country == "DE" && fuel == "gas" && sector == SECTOR_OTHERS) {
            reference_value <- 5000 + i_month * 10
          }

          tibble(
            iso2 = country,
            fuel = fuel,
            sector = sector,
            target_month = target_month,
            reference_value = reference_value
          )
        }))
      }))
    }))
  }))

  component_revision <- bind_rows(lapply(seq_along(vintage_months), function(i_vintage) {
    vintage_month <- vintage_months[[i_vintage]]

    component_reference %>%
      filter(target_month <= vintage_month) %>%
      mutate(
        lag_months = getFromNamespace(
          ".get_co2_revision_analysis_month_lag",
          "creaco2tracker"
        )(target_month, vintage_month),
        month_index = match(target_month, target_months),
        country_index = match(iso2, country_iso2s),
        fuel_index = match(fuel, fuels),
        sector_index = match(sector, sectors),
        revision_pct = (
          0.01 * pmin(lag_months, 4) +
            0.004 * country_index +
            0.003 * fuel_index +
            0.002 * sector_index
        ) * ifelse((month_index + lag_months) %% 2 == 0, 1, -1),
        revision_pct = if_else(reference_value == 0, 0, revision_pct),
        vintage_value = reference_value * (1 + revision_pct),
        revision = vintage_value - reference_value,
        absolute_revision = abs(revision),
        vintage_month = vintage_month
      ) %>%
      select(
        vintage_month,
        iso2,
        fuel,
        sector,
        target_month,
        lag_months,
        reference_value,
        vintage_value,
        revision,
        absolute_revision,
        revision_pct
      )
  }))

  summarise_totals <- function(x, value_col, vintage_col = FALSE) {
    grouping <- c("iso2", "target_month")
    if (isTRUE(vintage_col)) {
      grouping <- c("vintage_month", grouping, "lag_months")
    }

    x %>%
      group_by(across(all_of(grouping))) %>%
      summarise(
        !!value_col := sum(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        fuel = FUEL_TOTAL,
        sector = SECTOR_ALL
      )
  }

  country_reference <- summarise_totals(component_reference, "reference_value")
  country_revision <- component_revision %>%
    group_by(vintage_month, iso2, target_month, lag_months) %>%
    summarise(
      reference_value = sum(reference_value, na.rm = TRUE),
      vintage_value = sum(vintage_value, na.rm = TRUE),
      revision = sum(revision, na.rm = TRUE),
      absolute_revision = abs(revision),
      revision_pct = if_else(reference_value == 0, NA_real_, revision / reference_value),
      .groups = "drop"
    ) %>%
    mutate(
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL
    )

  eu_reference <- country_reference %>%
    group_by(target_month) %>%
    summarise(
      reference_value = sum(reference_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL
    ) %>%
    select(iso2, fuel, sector, target_month, reference_value)
  eu_revision <- country_revision %>%
    group_by(vintage_month, target_month, lag_months) %>%
    summarise(
      reference_value = sum(reference_value, na.rm = TRUE),
      vintage_value = sum(vintage_value, na.rm = TRUE),
      revision = sum(revision, na.rm = TRUE),
      absolute_revision = abs(revision),
      revision_pct = if_else(reference_value == 0, NA_real_, revision / reference_value),
      .groups = "drop"
    ) %>%
    mutate(
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL
    ) %>%
    select(
      vintage_month,
      iso2,
      fuel,
      sector,
      target_month,
      lag_months,
      reference_value,
      vintage_value,
      revision,
      absolute_revision,
      revision_pct
    )

  revision_comparison <- bind_rows(
    component_revision,
    country_revision,
    eu_revision
  ) %>%
    mutate(
      estimate = "central",
      unit = "t",
      reference_vintage_month = reference_vintage_month,
      absolute_revision_pct = abs(revision_pct)
    ) %>%
    rename(
      estimation_lag = lag_months,
      reference_vintage_value = reference_value
    ) %>%
    select(
      vintage_month,
      iso2,
      fuel,
      sector,
      estimate,
      unit,
      target_month,
      reference_vintage_month,
      estimation_lag,
      vintage_value,
      reference_vintage_value,
      revision,
      absolute_revision,
      revision_pct,
      absolute_revision_pct
    )

  reference_vintage_co2 <- bind_rows(
    component_reference,
    country_reference,
    eu_reference
  ) %>%
    mutate(
      vintage_month = reference_vintage_month,
      estimate = "central",
      unit = "t",
      date = target_month,
      value = reference_value
    ) %>%
    select(vintage_month, iso2, fuel, sector, estimate, unit, date, value)

  vintage_co2 <- bind_rows(
    component_revision %>%
      mutate(date = target_month, value = vintage_value) %>%
      select(vintage_month, iso2, fuel, sector, date, value),
    country_revision %>%
      mutate(date = target_month, value = vintage_value) %>%
      select(vintage_month, iso2, fuel, sector, date, value),
    eu_revision %>%
      mutate(date = target_month, value = vintage_value) %>%
      select(vintage_month, iso2, fuel, sector, date, value)
  ) %>%
    mutate(
      estimate = "central",
      unit = "t"
    ) %>%
    select(vintage_month, iso2, fuel, sector, estimate, unit, date, value)

  list(
    revision_comparison = revision_comparison,
    vintage_co2 = vintage_co2,
    reference_vintage_co2 = reference_vintage_co2,
    vintage_months = vintage_months,
    vintage_dates = getFromNamespace(
      ".get_co2_revision_analysis_month_ends",
      "creaco2tracker"
    )(vintage_months),
    target_months = target_months,
    reference_vintage_month = reference_vintage_month
  )
}


test_that(
  "revision-analysis month ends use calendar month ends",
  {
    target_months <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01"))

    month_ends <- getFromNamespace(
      ".get_co2_revision_analysis_month_ends",
      "creaco2tracker"
    )(target_months)

    expect_equal(month_ends, as.Date(c("2025-01-31", "2025-02-28", "2025-03-31")))
  }
)


test_that(
  "revision-analysis comparison target months include the previous calendar year",
  {
    comparison_target_months <- getFromNamespace(
      ".get_co2_revision_analysis_comparison_target_months",
      "creaco2tracker"
    )(2025)

    expect_equal(comparison_target_months[[1]], as.Date("2024-01-01"))
    expect_equal(
      comparison_target_months[[length(comparison_target_months)]],
      as.Date("2025-12-01")
    )
    expect_length(comparison_target_months, 24)
  }
)


test_that(
  "revision-analysis lag buckets use the requested exact boundaries",
  {
    lag_bucket <- getFromNamespace(
      ".get_co2_revision_analysis_lag_bucket",
      "creaco2tracker"
    )

    expect_equal(
      as.character(lag_bucket(c(0, 1, 4, 5, 8, 9, 12, 13, 24, 25))),
      c("0", "1-4", "1-4", "5-8", "5-8", "9-12", "9-12", "13-24", "13-24", "24+")
    )
  }
)


test_that(
  "revision-analysis chart stage labels use the short display names",
  {
    stage_levels <- getFromNamespace(
      ".get_co2_revision_analysis_data_maturity_levels",
      "creaco2tracker"
    )()
    stage_labels <- getFromNamespace(
      ".format_get_co2_revision_analysis_data_maturity_stage",
      "creaco2tracker"
    )(stage_levels)

    expect_equal(
      stage_labels,
      c(
        "No monthly",
        "Partial monthly",
        "Monthly only",
        "Partial annual",
        "Complete annual"
      )
    )
  }
)


test_that(
  "revision-analysis source availability uses cadence counts and annual publication months",
  {
    source_availability <- getFromNamespace(
      ".get_co2_revision_analysis_source_availability",
      "creaco2tracker"
    )(
      vintage_dates = as.Date(c("2025-01-31", "2025-12-31")),
      vintage_months = as.Date(c("2025-01-01", "2025-12-01")),
      target_months = as.Date(c("2024-12-01", "2025-01-01")),
      lags = default_source_lags() * 0L,
      publication_months = c(
        ember_power_yearly = 1L,
        eurostat_oil_yearly = 1L,
        eurostat_solid_yearly = 1L,
        eurostat_gas_yearly = 1L
      )
    )

    jan_2025 <- source_availability %>%
      filter(vintage_month == as.Date("2025-01-01"), target_month == as.Date("2025-01-01"))
    dec_2025 <- source_availability %>%
      filter(vintage_month == as.Date("2025-12-01"), target_month == as.Date("2024-12-01"))

    expect_equal(jan_2025$total_monthly_sources, 10)
    expect_equal(jan_2025$total_annual_sources, 4)
    expect_equal(jan_2025$monthly_availability_share, 1)
    expect_equal(jan_2025$annual_availability_share, 0)
    expect_equal(jan_2025$data_maturity_stage, "monthly_complete_annual_missing")
    expect_equal(dec_2025$data_maturity_stage, "monthly_complete_annual_complete")
  }
)


test_that(
  "revision-analysis summary charts include EU outputs and use bars for lag summaries",
  {
    tracker <- new.env(parent = emptyenv())
    tracker$bar_x_vars <- character()
    tracker$bar_paths <- character()
    tracker$line_calls <- 0L
    tracker$heatmap_paths <- character()
    tracker$heatmap_titles <- character()
    tracker$stage_paths <- character()
    tracker$stage_titles <- character()

    local_mocked_bindings(
      .plot_get_co2_revision_analysis_summary_bar = function(
        plot_data,
        x_var,
        y_var,
        filepath,
        ...
      ) {
        tracker$bar_x_vars <- c(tracker$bar_x_vars, x_var)
        tracker$bar_paths <- c(tracker$bar_paths, basename(filepath))
        invisible(filepath)
      },
      .plot_get_co2_revision_analysis_summary_line = function(...) {
        tracker$line_calls <- tracker$line_calls + 1L
        invisible(NULL)
      },
      .plot_get_co2_revision_analysis_revision_heatmap = function(
        group_data,
        filepath,
        title_prefix,
        ...
      ) {
        tracker$heatmap_paths <- c(tracker$heatmap_paths, basename(filepath))
        tracker$heatmap_titles <- c(tracker$heatmap_titles, title_prefix)
        invisible(filepath)
      },
      .plot_get_co2_revision_analysis_revision_by_data_maturity_stage = function(
        group_data,
        filepath,
        title_prefix,
        ...
      ) {
        tracker$stage_paths <- c(tracker$stage_paths, basename(filepath))
        tracker$stage_titles <- c(tracker$stage_titles, title_prefix)
        invisible(filepath)
      },
      .plot_get_co2_revision_analysis_top_contributors = function(...) invisible(NULL),
      .plot_get_co2_revision_analysis_summary_estimate_vs_reference = function(...) invisible(NULL),
      .plot_get_co2_revision_analysis_trend_direction_agreement = function(...) invisible(NULL),
      .package = "creaco2tracker"
    )

    plot_paths <- getFromNamespace(
      ".plot_get_co2_revision_analysis_summary_charts",
      "creaco2tracker"
    )(
      lag_summary = tibble(
        aggregation_level = c("total", "country", "component"),
        lag_bucket = c("0", "1-4", "5-8"),
        weighted_absolute_revision_pct = c(0.1, 0.2, 0.3),
        signed_revision_pct = c(0.05, -0.03, 0.01)
      ),
      stage_summary = tibble(
        aggregation_level = c("total", "country", "component"),
        data_maturity_stage = c(
          "monthly_missing",
          "monthly_partial",
          "monthly_complete_annual_complete"
        ),
        weighted_absolute_revision_pct = c(0.1, 0.2, 0.3),
        signed_revision_pct = c(0.05, -0.03, 0.01)
      ),
      country_summary = tibble(
        country = "DE",
        gross_revision = 10,
        gross_revision_share = 1
      ),
      country_component_summary = tibble(
        country = "DE",
        component = "Coal - Power Generation",
        component_id = "coal_power_generation",
        gross_revision = 10,
        gross_revision_share = 1
      ),
      trend_direction_agreement = tibble(),
      comparison_internal = tibble(
        aggregation_level = c("total", "country", "component"),
        vintage_month = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
        reference_vintage = as.Date(c("2025-03-01", "2025-03-01", "2025-03-01")),
        target_month = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
        reference_estimate = c(1000000, 500000, 250000),
        estimate = c(1010000, 505000, 255000),
        revision = c(10000, 5000, 5000),
        absolute_revision = c(10000, 5000, 5000),
        data_maturity_stage = c(
          "monthly_missing",
          "monthly_partial",
          "monthly_complete_annual_complete"
        ),
        is_always_zero_country_component = c(FALSE, FALSE, FALSE)
      ),
      output_dir = tempdir(),
      width = 10,
      height = 6,
      dpi = 72,
      top_n = 15
    )

    expect_true(all(c(
      "revision_by_lag_bucket",
      "revision_by_data_maturity_stage",
      "signed_revision_by_lag_bucket",
      "signed_revision_by_data_maturity_stage",
      "eu_revision_heatmap",
      "eu_revision_by_data_maturity_stage"
    ) %in% names(plot_paths)))
    expect_equal(tracker$line_calls, 0L)
    expect_equal(
      tracker$bar_paths,
      c(
        "revision_by_lag_bucket.png",
        "revision_by_data_maturity_stage.png",
        "signed_revision_by_lag_bucket.png",
        "signed_revision_by_data_maturity_stage.png"
      )
    )
    expect_equal(
      tracker$bar_x_vars,
      c("lag_bucket", "data_maturity_stage", "lag_bucket", "data_maturity_stage")
    )
    expect_equal(tracker$heatmap_paths, "eu_revision_heatmap.png")
    expect_equal(tracker$heatmap_titles, "EU")
    expect_equal(tracker$stage_paths, "eu_revision_by_data_maturity_stage.png")
    expect_equal(tracker$stage_titles, "EU")
  }
)


test_that(
  "revision-analysis comparison builder keeps comparable central rows and flags zero handling",
  {
    mock <- make_mock_revision_analysis_data()
    source_availability <- tibble(
      vintage_month = mock$vintage_months[1],
      target_month = as.Date(c("2025-01-01", "2025-02-01")),
      data_maturity_stage = c(
        "monthly_complete_annual_missing",
        "monthly_complete_annual_partial"
      ),
      monthly_availability_share = 1,
      annual_availability_share = c(0, 0.5),
      available_monthly_sources = 10L,
      total_monthly_sources = 10L,
      available_annual_sources = c(0L, 2L),
      total_annual_sources = 4L
    )

    central_rows <- mock$revision_comparison %>%
      filter(vintage_month == mock$vintage_months[1], target_month %in% source_availability$target_month)
    test_input <- bind_rows(
      central_rows,
      central_rows %>% mutate(estimate = "upper"),
      tibble(
        vintage_month = mock$vintage_months[1],
        iso2 = "DE",
        fuel = FUEL_TOTAL,
        sector = SECTOR_TRANSPORT,
        estimate = "central",
        unit = "t",
        target_month = as.Date("2025-01-01"),
        reference_vintage_month = mock$reference_vintage_month,
        estimation_lag = 0L,
        vintage_value = 12345,
        reference_vintage_value = 10000,
        revision = 2345,
        absolute_revision = 2345,
        revision_pct = 0.2345,
        absolute_revision_pct = 0.2345
      )
    ) %>%
      mutate(
        reference_vintage_value = if_else(
          iso2 == "DE" & fuel == "coal" & sector == SECTOR_ELEC &
            target_month == as.Date("2025-02-01"),
          NA_real_,
          reference_vintage_value
        )
      )

    comparison_internal <- getFromNamespace(
      ".build_get_co2_revision_analysis_vintage_revision_comparison",
      "creaco2tracker"
    )(
      revision_comparison = test_input,
      source_availability = source_availability,
      near_zero_reference_threshold_tonnes_co2 = 100000
    )

    expect_true(all(comparison_internal$lag_months >= 0))
    expect_true(all(!is.na(comparison_internal$estimate)))
    expect_false(any(comparison_internal$country == "EU" & comparison_internal$aggregation_level != "total"))
    expect_true(any(comparison_internal$component_type == "sector"))
    expect_true(any(comparison_internal$is_near_zero_reference))
    expect_true(any(comparison_internal$is_always_zero_country_component))
    expect_true(all(is.na(
      comparison_internal$relative_revision[comparison_internal$is_near_zero_reference]
    )))
    expect_false(any(comparison_internal$target_month == as.Date("2025-02-01") &
      comparison_internal$country == "DE" &
      comparison_internal$fuel == "coal" &
      comparison_internal$sector == SECTOR_ELEC))
  }
)


test_that(
  "revision-analysis metric rows compute weighted and percentile summaries safely",
  {
    metric_row <- getFromNamespace(
      ".get_co2_revision_analysis_metric_row",
      "creaco2tracker"
    )(
      tibble(
        reference_estimate = c(100, 200, 5000),
        estimate = c(110, 180, 6000),
        revision = c(10, -20, 1000),
        absolute_revision = c(10, 20, 1000),
        relative_revision = c(0.1, -0.1, NA_real_),
        is_near_zero_reference = c(FALSE, FALSE, TRUE)
      ),
      near_zero_reference_threshold_tonnes_co2 = 10
    )

    expect_equal(metric_row$n_observations, 3)
    expect_equal(metric_row$sum_reference, 5300)
    expect_equal(metric_row$gross_revision, 1030)
    expect_equal(metric_row$weighted_absolute_revision_pct, 30 / 300)
    expect_equal(metric_row$signed_revision_pct, -10 / 300)
    expect_equal(metric_row$median_abs_revision_pct, 0.1)
    expect_equal(metric_row$p90_abs_revision_pct, 0.1)
    expect_equal(metric_row$max_abs_revision_pct, 0.1)
    expect_equal(metric_row$rmse, sqrt(mean(c(10, -20, 1000)^2)))
  }
)


test_that(
  "revision-analysis comparison keeps only months available in each vintage",
  {
    target_months <- as.Date(c("2025-01-01", "2025-02-01"))
    reference_vintage_co2 <- tibble(
      vintage_month = as.Date("2025-03-01"),
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      date = target_months,
      value = c(100, 200)
    )
    vintage_co2 <- tibble(
      vintage_month = as.Date(c("2025-01-01", "2025-02-01", "2025-02-01")),
      iso2 = "EU",
      fuel = FUEL_TOTAL,
      sector = SECTOR_ALL,
      estimate = "central",
      unit = "t",
      date = as.Date(c("2025-01-01", "2025-01-01", "2025-02-01")),
      value = c(90, 95, 220)
    )

    revision_comparison <- getFromNamespace(
      ".compare_get_co2_revision_analysis",
      "creaco2tracker"
    )(
      vintage_co2 = vintage_co2,
      reference_vintage_co2 = reference_vintage_co2,
      target_months = target_months,
      reference_vintage_month = as.Date("2025-03-01")
    )

    expect_equal(nrow(revision_comparison), 3)
    expect_equal(
      revision_comparison %>%
        filter(vintage_month == as.Date("2025-01-01")) %>%
        pull(target_month),
      as.Date("2025-01-01")
    )
    expect_equal(
      revision_comparison %>%
        filter(vintage_month == as.Date("2025-02-01"), target_month == as.Date("2025-02-01")) %>%
        pull(revision),
      20
    )
  }
)


test_that(
  "revision-analysis workflow protects controlled get_co2 arguments",
  {
    expect_error(
      getFromNamespace(
        ".validate_get_co2_revision_analysis_args",
        "creaco2tracker"
      )(list(date_to = "2025-01-31")),
      "date_to"
    )
    expect_error(
      getFromNamespace(
        ".validate_get_co2_revision_analysis_args",
        "creaco2tracker"
      )(list(data_masking = DATA_MASKING_NONE)),
      "data_masking"
    )
  }
)


test_that(
  "validate_get_co2_revision_analysis returns the new public result shape",
  {
    output_folder <- tempfile("revision-analysis-workflow-")
    dir.create(output_folder, recursive = TRUE)
    on.exit(unlink(output_folder, recursive = TRUE), add = TRUE)

    local_mocked_bindings(
      get_co2 = function(date_to, ...) {
        month_seq <- seq.Date(
          from = as.Date("2024-01-01"),
          to = lubridate::floor_date(as.Date(date_to), "month"),
          by = "month"
        )

        bind_rows(
          tibble(
            iso2 = "DE",
            fuel = "coal",
            sector = SECTOR_ELEC,
            estimate = "central",
            unit = "t",
            date = month_seq,
            value = seq_along(month_seq) * 10
          ),
          tibble(
            iso2 = "DE",
            fuel = "gas",
            sector = SECTOR_OTHERS,
            estimate = "central",
            unit = "t",
            date = month_seq,
            value = seq_along(month_seq) * 5
          ),
          tibble(
            iso2 = "EU",
            fuel = FUEL_TOTAL,
            sector = SECTOR_ALL,
            estimate = "central",
            unit = "t",
            date = month_seq,
            value = seq_along(month_seq) * 15
          )
        )
      },
      plot_get_co2_revision_analysis_validation = function(...) {
        list(
          vintage_revision_comparison = tibble(example = 1),
          debug_revision_summary = tibble(example = 2),
          revision_outliers = tibble(example = 3),
          summary_plot_paths = c(example = "summary.png")
        )
      },
      .package = "creaco2tracker"
    )

    result <- validate_get_co2_revision_analysis(
      output_folder = output_folder,
      validation_year = 2025,
      reference_vintage_month = "2026-03-31",
      iso2s = c("DE", "EU"),
      use_cache = FALSE,
      reuse_run_cache = FALSE,
      save_runs = FALSE
    )

    expect_true(all(c(
      "vintage_run_info",
      "reference_vintage_co2",
      "vintage_co2",
      "vintage_revision_comparison",
      "debug_revision_summary",
      "revision_outliers",
      "summary_plot_paths"
    ) %in% names(result)))
    expect_false(any(c(
      "data_availability_regime",
      "revision_comparison",
      "lag_0_revision_comparison",
      "country_component_chart_inventory",
      "plot_paths",
      "drilldown_manifest"
    ) %in% names(result)))
  }
)
