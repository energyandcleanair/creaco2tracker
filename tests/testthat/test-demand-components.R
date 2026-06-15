library(testthat)
library(dplyr)
library(lubridate)
library(tibble)

make_mock_demand_component_sources <- function() {
  dates <- seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "day")
  yday <- lubridate::yday(dates)
  year <- lubridate::year(dates)

  hdd_climatology <- 2 + pmax(0, 13 + 12 * cos(2 * pi * (yday - 15) / 365))
  cdd_climatology <- pmax(0, 8 * cos(2 * pi * (yday - 200) / 365))
  hdd <- pmax(0.1, hdd_climatology + ifelse(year == 2022, -1.2, 1.2))
  cdd <- pmax(0, cdd_climatology + ifelse(year == 2022, 0.8, -0.8))

  wday_effect <- ifelse(lubridate::wday(dates) %in% c(1, 7), -35, 0)
  gas_elec_mw <- 130 + 12 * sin(2 * pi * yday / 365) + ifelse(year == 2023, 5, 0)
  elec_total_mw <- 1000 + 24 * hdd + 16 * cdd + wday_effect + ifelse(year == 2023, 20, 0)
  gas_total_m3 <- 6000 + 75 * hdd + 3.2 * gas_elec_mw + 4 * wday_effect +
    ifelse(year == 2023, 90, 0)

  list(
    gas_demand = tibble(
      iso2 = "EU",
      date = dates,
      value = gas_total_m3,
      fuel = "fossil_gas",
      sector = "total",
      data_source = "mock_gas",
      unit = "m3",
      frequency = "daily",
      region_type = "region"
    ),
    power_generation = bind_rows(
      tibble(
        iso2 = "EU",
        country = "EU total",
        region = "EU",
        date = dates,
        source = "Fossil Gas",
        value_mw = gas_elec_mw,
        value_mwh = gas_elec_mw * 24
      ),
      tibble(
        iso2 = "EU",
        country = "EU total",
        region = "EU",
        date = dates,
        source = "Total",
        value_mw = elec_total_mw,
        value_mwh = elec_total_mw * 24
      )
    ),
    weather = bind_rows(
      tibble(
        region_id = "EU",
        region_iso2 = "EU",
        region_type = "region",
        region_name = "EU",
        date = dates,
        variable = "hdd",
        value = hdd,
        unit = "degree_days",
        averaging_period = "daily",
        source = "mock_weather"
      ),
      tibble(
        region_id = "EU",
        region_iso2 = "EU",
        region_type = "region",
        region_name = "EU",
        date = dates,
        variable = "cdd",
        value = cdd,
        unit = "degree_days",
        averaging_period = "daily",
        source = "mock_weather"
      )
    )
  )
}

mock_demand_component_sources <- function(source_data, .env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$gas <- list()
  calls$power <- list()
  calls$weather <- list()

  local_mocked_bindings(
    get_gas_demand = function(iso2 = NULL, iso2s = iso2, date_to = NULL, ...) {
      calls$gas[[length(calls$gas) + 1]] <- c(
        list(iso2 = iso2, iso2s = iso2s, date_to = date_to),
        list(...)
      )

      result <- source_data$gas_demand
      if (!is.null(iso2s)) {
        result <- result %>% filter(.data$iso2 %in% iso2s)
      }
      if (!is.null(date_to)) {
        result <- result %>% filter(.data$date <= as.Date(date_to))
      }
      result
    },
    get_power_generation = function(
      iso2s,
      date_from,
      date_to = Sys.Date(),
      use_cache = TRUE,
      diagnostics_folder = NULL,
      ...
    ) {
      calls$power[[length(calls$power) + 1]] <- c(
        list(
          iso2s = iso2s,
          date_from = date_from,
          date_to = date_to,
          use_cache = use_cache,
          diagnostics_folder = diagnostics_folder
        ),
        list(...)
      )

      source_data$power_generation %>%
        filter(
          .data$iso2 %in% iso2s,
          .data$date >= as.Date(date_from),
          .data$date <= as.Date(date_to)
        )
    },
    weather_data_access_get_filled = function(
      variable,
      region_id,
      date_from,
      date_to,
      use_cache = TRUE,
      ...
    ) {
      calls$weather[[length(calls$weather) + 1]] <- c(
        list(
          variable = variable,
          region_id = region_id,
          date_from = date_from,
          date_to = date_to,
          use_cache = use_cache
        ),
        list(...)
      )

      variables <- strsplit(tolower(variable), ",")[[1]]
      variables <- trimws(variables)

      source_data$weather %>%
        filter(
          .data$variable %in% variables,
          .data$region_id %in% region_id,
          .data$date >= as.Date(date_from),
          .data$date <= as.Date(date_to)
        )
    },
    .package = "creaco2tracker",
    .env = .env
  )

  calls
}

expect_components_sum_to_observed <- function(result, source_data) {
  component_sums <- result %>%
    filter(!is.na(.data$value)) %>%
    group_by(.data$iso2, .data$date, .data$fuel) %>%
    summarise(total_components = sum(.data$value, na.rm = TRUE), .groups = "drop")

  observed_totals <- bind_rows(
    source_data$gas_demand %>%
      select(iso2, date, fuel, observed = value),
    source_data$power_generation %>%
      filter(.data$source == "Total") %>%
      transmute(
        iso2,
        date,
        fuel = "electricity",
        observed = value_mw
      )
  )

  comparison <- component_sums %>%
    left_join(observed_totals, by = c("iso2", "date", "fuel"))

  expect_false(any(is.na(comparison$observed)))
  expect_equal(comparison$total_components, comparison$observed, tolerance = 1e-6)
}

test_that("get_demand_components with LM decomposes mocked source data", {
  source_data <- make_mock_demand_component_sources()
  calls <- mock_demand_component_sources(source_data)

  result <- suppressMessages(
    get_demand_components(
      iso2s = "EU",
      date_from = "2022-01-01",
      date_to = "2023-12-31",
      use_cache = FALSE,
      model_type = "lm",
      diagnostics_folder = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_setequal(
    names(result),
    c(
      "iso2", "date", "fuel", "component", "value",
      "value_weather_corrected", "unit", "frequency", "data_source"
    )
  )
  expect_setequal(unique(result$fuel), c("electricity", "fossil_gas"))
  expect_setequal(
    unique(result$component[result$fuel == "electricity"]),
    c("heating", "cooling", "others")
  )
  expect_setequal(
    unique(result$component[result$fuel == "fossil_gas"]),
    c("heating", "electricity", "others")
  )

  expect_components_sum_to_observed(result, source_data)

  non_weather_components <- result %>%
    filter(
      !is.na(.data$value),
      .data$component == "others" |
        (.data$fuel == "fossil_gas" & .data$component == "electricity")
    )
  expect_equal(
    non_weather_components$value,
    non_weather_components$value_weather_corrected,
    tolerance = 1e-8
  )

  heating <- result %>%
    filter(.data$component == "heating", !is.na(.data$value))
  expect_gt(mean(heating$value > 0, na.rm = TRUE), 0.95)
  expect_gt(mean(abs(heating$value - heating$value_weather_corrected) > 1e-6), 0.5)

  others <- result %>%
    filter(.data$component == "others", !is.na(.data$value))
  expect_gt(mean(others$value > 0, na.rm = TRUE), 0.95)

  expect_length(calls$gas, 1)
  expect_equal(calls$gas[[1]]$iso2, "EU")
  expect_false(calls$gas[[1]]$use_cache)
  expect_true(calls$gas[[1]]$correct_to_eurostat)
  expect_null(calls$gas[[1]]$data_masking)

  expect_length(calls$power, 1)
  expect_equal(calls$power[[1]]$iso2s, "EU")
  expect_equal(as.Date(calls$power[[1]]$date_from), as.Date("2022-01-01"))
  expect_false(calls$power[[1]]$use_cache)
  expect_null(calls$power[[1]]$data_masking)

  expect_length(calls$weather, 1)
  expect_equal(calls$weather[[1]]$variable, "HDD,CDD")
  expect_equal(calls$weather[[1]]$region_id, "EU")
  expect_false(calls$weather[[1]]$use_cache)
})

test_that("get_demand_components with GAM returns the same component structure", {
  source_data <- make_mock_demand_component_sources()
  mock_demand_component_sources(source_data)

  result <- suppressMessages(
    get_demand_components(
      iso2s = "EU",
      date_from = "2022-01-01",
      date_to = "2023-12-31",
      use_cache = FALSE,
      model_type = "gam",
      diagnostics_folder = NULL
    )
  )

  expect_gt(nrow(result), 0)
  expect_setequal(
    names(result),
    c(
      "iso2", "date", "fuel", "component", "value",
      "value_weather_corrected", "unit", "frequency", "data_source"
    )
  )
  expect_setequal(unique(result$fuel), c("electricity", "fossil_gas"))
  expect_setequal(
    unique(result$component[result$fuel == "electricity"]),
    c("heating", "cooling", "others")
  )
  expect_setequal(
    unique(result$component[result$fuel == "fossil_gas"]),
    c("heating", "electricity", "others")
  )

  expect_components_sum_to_observed(result, source_data)

  heating <- result %>%
    filter(.data$component == "heating", !is.na(.data$value))
  expect_gt(mean(heating$value > 0, na.rm = TRUE), 0.95)
  expect_gt(mean(abs(heating$value - heating$value_weather_corrected) > 1e-6), 0.5)
})

test_that("get_demand_components resolves historical defaults before fetching sources", {
  source_data <- make_mock_demand_component_sources()
  calls <- mock_demand_component_sources(source_data)

  suppressMessages(
    get_demand_components(
      iso2s = "EU",
      date_from = "2023-01-01",
      date_to = "2023-12-31",
      use_cache = FALSE,
      model_type = "lm",
      diagnostics_folder = NULL,
      data_masking = DATA_MASKING_HISTORICAL_DEFAULTS
    )
  )

  expected_config <- data_masking_as_of("2023-12-31")

  expect_equal(calls$gas[[1]]$data_masking, expected_config)
  expect_equal(calls$power[[1]]$data_masking, expected_config)
  expect_equal(calls$weather[[1]]$data_masking, expected_config)
})
