library(testthat)
library(dplyr)


test_that(
  "apply_source_data_mask applies source-specific and global rules",
  {
    weather <- tibble(
      region_iso2 = c("EU", "EU", "DE"),
      date = as.Date(c("2024-01-01", "2024-02-01", "2024-02-01")),
      variable = "hdd",
      value = c(10, 20, 30)
    )

    masks <- list(
      all = list(date_to = "2024-01-31", region_iso2 = "EU"),
      weather = list(
        list(date_from = "2024-02-01", region_iso2 = "DE")
      )
    )

    masked <- apply_source_data_mask(weather, "weather", masks)

    expect_equal(nrow(masked), 1)
    expect_equal(masked$region_iso2, "EU")
    expect_equal(masked$date, as.Date("2024-02-01"))
    expect_equal(masked$value, 20)
  }
)


test_that(
  "apply_source_data_mask rejects non-canonical source names",
  {
    pwr <- tibble(
      iso2 = c("EU", "EU"),
      source = c("Wind", "Solar"),
      date = as.Date(c("2024-01-01", "2024-01-01")),
      value_mw = c(100, 120),
      value_mwh = c(2400, 2500)
    )

    masks <- list(
      entsoe_power_daily = list(
        list(date_from = "2024-01-01", source = "Wind")
      )
    )

    expect_error(
      apply_source_data_mask(pwr, "power", masks),
      "canonical source key"
    )
  }
)


test_that(
  "apply_source_data_mask rejects removed compatibility keys",
  {
    weather <- tibble(
      region_iso2 = "EU",
      date = as.Date("2024-01-01"),
      value = 10
    )

    expect_error(
      apply_source_data_mask(
        weather,
        "weather",
        list(gas_demand = list(date_from = "2024-01-01"))
      ),
      "Unknown masking source keys"
    )
  }
)


test_that(
  "get_data_masking_config returns expected top-level keys",
  {
    cfg <- get_data_masking_config()

    expect_true(
      all(
        c(
          "all",
          "entsog_flow_raw",
          "agsi_storage_daily",
          "eurostat_gas_monthly_for_correction",
          "entsoe_power_daily",
          "ember_power_monthly",
          "ember_power_yearly",
          "eurostat_oil_monthly",
          "eurostat_oil_yearly",
          "eurostat_solid_monthly",
          "eurostat_solid_yearly",
          "eurostat_gas_monthly",
          "eurostat_gas_yearly",
          "eurostat_indprod",
          "weather"
        ) %in% names(cfg)
      )
    )
  }
)


test_that(
  "public pipeline defaults make masking intent explicit",
  {
    expect_identical(
      formals(get_co2)$data_masking,
      quote(DATA_MASKING_NONE)
    )
    expect_identical(
      formals(get_gas_demand)$data_masking,
      quote(DATA_MASKING_NONE)
    )
    expect_identical(
      formals(get_power_generation)$data_masking,
      quote(DATA_MASKING_NONE)
    )
    expect_identical(
      formals(get_demand_components)$data_masking,
      quote(DATA_MASKING_NONE)
    )
    expect_identical(
      formals(get_corrected_demand)$data_masking,
      quote(DATA_MASKING_NONE)
    )
  }
)


test_that(
  "explicit masking modes resolve to configs",
  {
    resolver <- getFromNamespace(".resolve_data_masking_config", "creaco2tracker")
    custom_config <- list(weather = list(date_from = "2024-01-01"))

    expect_null(
      resolver(
        data_masking = DATA_MASKING_NONE,
        reference_date = "2024-06-01"
      )
    )
    expect_equal(
      resolver(
        data_masking = DATA_MASKING_HISTORICAL_DEFAULTS,
        reference_date = "2024-06-01"
      ),
      data_masking_as_of("2024-06-01")
    )
    expect_equal(
      resolver(
        data_masking = custom_config,
        reference_date = "2024-06-01"
      ),
      custom_config
    )

    expect_error(
      resolver(
        data_masking = "custom",
        reference_date = "2024-06-01"
      ),
      "DATA_MASKING_NONE"
    )
  }
)


test_that(
  "default_source_lags returns the expected source keys",
  {
    lags <- default_source_lags()

    expect_named(
      lags,
      c(
        "entsoe_power_daily",
        "ember_power_monthly",
        "ember_power_yearly",
        "entsog_flow_raw",
        "agsi_storage_daily",
        "eurostat_gas_monthly_for_correction",
        "eurostat_oil_monthly",
        "eurostat_oil_yearly",
        "eurostat_solid_monthly",
        "eurostat_solid_yearly",
        "eurostat_gas_monthly",
        "eurostat_gas_yearly",
        "eurostat_indprod",
        "weather"
      )
    )
    expect_true(all(vapply(lags, is.integer, logical(1))))
  }
)


test_that(
  "default_source_publication_months returns the expected annual source keys",
  {
    publication_months <- default_source_publication_months()

    expect_named(
      publication_months,
      c(
        "ember_power_yearly",
        "eurostat_oil_yearly",
        "eurostat_solid_yearly",
        "eurostat_gas_yearly"
      )
    )
    expect_true(all(vapply(publication_months, is.integer, logical(1))))
  }
)


test_that(
  "data_masking_as_of applies the default publication schedule",
  {
    cfg <- data_masking_as_of("2022-06-01")

    expect_equal(cfg$entsoe_power_daily$date_from, "2022-05-31")
    expect_equal(cfg$ember_power_monthly$date_from, "2022-04-01")
    expect_equal(cfg$ember_power_yearly$date_from, "2022-01-01")
    expect_equal(cfg$eurostat_gas_monthly$date_from, "2022-02-01")
    expect_equal(cfg$weather$date_from, "2022-05-31")
    expect_equal(cfg$all, list())
    expect_false("gas_demand" %in% names(cfg))
  }
)


test_that(
  "data_masking_as_of respects custom lag overrides",
  {
    cfg <- data_masking_as_of(
      "2022-06-01",
      lags = c(eurostat_gas_monthly = 0L, weather = 5L)
    )

    expect_equal(cfg$eurostat_gas_monthly$date_from, "2022-07-01")
    expect_equal(cfg$weather$date_from, "2022-05-28")
    expect_equal(cfg$entsoe_power_daily$date_from, "2022-05-31")
  }
)


test_that(
  "data_masking_as_of_batch returns named configs",
  {
    cfgs <- data_masking_as_of_batch(c("2022-01-01", "2023-01-01"))

    expect_named(cfgs, c("2022-01-01", "2023-01-01"))
    expect_equal(length(cfgs), 2)
    expect_true(is.list(cfgs[[1]]))
    expect_equal(cfgs[[1]]$weather$date_from, "2021-12-31")
    expect_equal(cfgs[[1]]$ember_power_yearly$date_from, "2021-01-01")
    expect_equal(cfgs[[2]]$eurostat_gas_monthly$date_from, "2022-09-01")
    expect_equal(cfgs[[2]]$eurostat_gas_yearly$date_from, "2022-01-01")
  }
)


test_that(
  "data_masking_as_of accepts zero-day overrides",
  {
    cfg <- data_masking_as_of("2022-06-01", lags = c(weather = 0L))

    expect_equal(cfg$weather$date_from, "2022-06-02")
  }
)


test_that(
  "default annual publication months apply before and on release boundaries",
  {
    january_cfg <- data_masking_as_of("2025-01-31")
    february_cfg <- data_masking_as_of("2025-02-01")
    june_cfg <- data_masking_as_of("2025-06-30")
    july_cfg <- data_masking_as_of("2025-07-01")

    expect_equal(january_cfg$ember_power_yearly$date_from, "2024-01-01")
    expect_equal(february_cfg$ember_power_yearly$date_from, "2025-01-01")
    expect_equal(june_cfg$eurostat_gas_yearly$date_from, "2024-01-01")
    expect_equal(july_cfg$eurostat_gas_yearly$date_from, "2025-01-01")
    expect_equal(june_cfg$eurostat_oil_yearly$date_from, "2024-01-01")
    expect_equal(july_cfg$eurostat_solid_yearly$date_from, "2025-01-01")
  }
)


test_that(
  "annual sources can fall back to lag-based handling",
  {
    cfg_without_months <- data_masking_as_of("2022-06-01", publication_months = NULL)
    cfg_partial_months <- data_masking_as_of(
      "2022-06-01",
      publication_months = c(ember_power_yearly = 2L)
    )

    expect_equal(cfg_without_months$ember_power_yearly$date_from, "2021-01-01")
    expect_equal(cfg_without_months$eurostat_gas_yearly$date_from, "2021-01-01")
    expect_equal(cfg_partial_months$ember_power_yearly$date_from, "2022-01-01")
    expect_equal(cfg_partial_months$eurostat_gas_yearly$date_from, "2021-01-01")
  }
)


test_that(
  "data_masking_as_of validates publication_months",
  {
    expect_error(
      data_masking_as_of("2022-06-01", publication_months = c(2L, 7L)),
      "named vector"
    )
    expect_error(
      data_masking_as_of("2022-06-01", publication_months = c(weather = 2L)),
      "yearly sources"
    )
    expect_error(
      data_masking_as_of("2022-06-01", publication_months = c(ember_power_yearly = 13L)),
      "between 1 and 12"
    )
  }
)


test_that(
  "data_masking_as_of validates lag overrides strictly",
  {
    expect_error(
      data_masking_as_of("2022-06-01", lags = c(2L, 7L)),
      "named vector"
    )
    expect_error(
      data_masking_as_of("2022-06-01", lags = c(gas_demand = 2L)),
      "default publication lags"
    )
    expect_error(
      data_masking_as_of("2022-06-01", lags = c(weather = -1L)),
      "greater than or equal to 0"
    )
  }
)


test_that(
  "data_masking_as_of removes data after the availability cutoff",
  {
    cfg <- data_masking_as_of("2022-06-01")
    weather <- tibble(
      date = as.Date(c("2022-05-30", "2022-05-31", "2022-06-01")),
      value = c(1, 2, 3)
    )

    masked <- apply_source_data_mask(weather, "weather", cfg)

    expect_equal(masked$value[masked$date == as.Date("2022-05-30")], 1)
    expect_false(as.Date("2022-05-31") %in% masked$date)
    expect_false(as.Date("2022-06-01") %in% masked$date)
  }
)


test_that(
  "apply_source_data_mask rejects legacy fields and unsupported columns",
  {
    pwr <- tibble(
      iso2 = "EU",
      source = "Wind",
      date = as.Date("2024-01-01"),
      value_mw = 100,
      value_mwh = 2400
    )

    expect_error(
      apply_source_data_mask(
        pwr,
        "entsoe_power_daily",
        list(entsoe_power_daily = list(available_from = "2024-01-01"))
      ),
      "removed legacy fields"
    )

    expect_error(
      apply_source_data_mask(
        pwr,
        "entsoe_power_daily",
        list(entsoe_power_daily = list(date_from = "2024-01-01", value_cols = "value"))
      ),
      "missing columns"
    )
  }
)


test_that(
  "granular entsoe masking removes matching rows",
  {
    pwr <- tibble(
      iso2 = c("EU", "EU"),
      source = c("Wind", "Solar"),
      date = as.Date(c("2024-01-01", "2024-01-01")),
      value_mw = c(100, 120),
      value_mwh = c(2400, 2500)
    )

    masks <- list(
      entsoe_power_daily = list(
        date_from = "2024-01-01",
        source = "Wind"
      )
    )

    masked <- apply_source_data_mask(pwr, "entsoe_power_daily", masks)

    expect_equal(nrow(masked), 1)
    expect_equal(masked$source, "Solar")
    expect_equal(masked$value_mw, 120)
    expect_equal(masked$value_mwh, 2500)
  }
)


test_that(
  "granular entsog masking supports flow-type filters",
  {
    entsog_raw <- tibble(
      destination_iso2 = c("DE", "DE", "DE"),
      departure_iso2 = c("NL", "NL", "NL"),
      date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      type = c("consumption", "storage", "production"),
      value_m3 = c(1, 2, 3)
    )

    masks <- list(
      entsog_flow_raw = list(type = c("storage", "production"))
    )

    masked <- apply_source_data_mask(entsog_raw, "entsog_flow_raw", masks)

    expect_equal(masked$value_m3[1], 1)
    expect_equal(nrow(masked), 1)
    expect_equal(masked$type, "consumption")
  }
)


test_that(
  "granular eurostat monthly masking removes rows using time column",
  {
    eurostat_gas <- tibble(
      iso2 = c("EU", "EU"),
      time = as.Date(c("2020-01-01", "2020-02-01")),
      values = c(100, 120)
    )

    masks <- list(
      eurostat_gas_monthly = list(date_from = "2020-02-01")
    )

    masked <- apply_source_data_mask(eurostat_gas, "eurostat_gas_monthly", masks)

    expect_equal(masked$values[1], 100)
    expect_equal(nrow(masked), 1)
    expect_false(as.Date("2020-02-01") %in% masked$time)
  }
)


test_that(
  "fill_weather does not forward fill across explicitly masked rows",
  {
    weather_raw <- tibble(
      date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04")),
      variable = "hdd",
      unit = "degC",
      region_id = "EU",
      region_type = "region",
      region_iso2 = "EU",
      averaging_period = "daily",
      source = "mock",
      region_name = "EU",
      value = c(10, NA, 4)
    )

    filled <- fill_weather(weather_raw)

    expect_equal(filled$value[filled$date == as.Date("2024-01-01")], 10)
    expect_true(is.na(filled$value[filled$date == as.Date("2024-01-02")]))
    expect_true(is.na(filled$value[filled$date == as.Date("2024-01-03")]))
    expect_equal(filled$value[filled$date == as.Date("2024-01-04")], 4)
  }
)


test_that(
  "fill_weather still fills inserted gaps when no explicit NA is present",
  {
    weather_raw <- tibble(
      date = as.Date(c("2024-01-01", "2024-01-03")),
      variable = "hdd",
      unit = "degC",
      region_id = "EU",
      region_type = "region",
      region_iso2 = "EU",
      averaging_period = "daily",
      source = "mock",
      region_name = "EU",
      value = c(10, 12)
    )

    filled <- fill_weather(weather_raw)

    expect_equal(filled$value[filled$date == as.Date("2024-01-02")], 10)
  }
)


test_that(
  "get_gas_demand_consdist treats all-missing daily aggregates as zero",
  {
    entsog_data <- tibble(
      destination_iso2 = c("DE", "DE", "DE"),
      departure_iso2 = c("NL", "NL", "NL"),
      date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      type = c("consumption", "consumption", "consumption"),
      value_m3 = c(-10, NA, -30)
    )

    result <- get_gas_demand_consdist(entsog_data, years = 2024)

    expect_equal(result$value_m3[result$date == as.Date("2024-01-01")], 10)
    expect_equal(result$value_m3[result$date == as.Date("2024-01-02")], 0)
    expect_equal(result$value_m3[result$date == as.Date("2024-01-03")], 30)
  }
)


test_that(
  "get_gas_demand_apparent interpolates internal gaps",
  {
    entsog_data <- tibble(
      destination_iso2 = c("DE", "DE", "DE"),
      departure_iso2 = c("NL", "NL", "NL"),
      date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      type = c("crossborder", "crossborder", "crossborder"),
      value_m3 = c(10, NA, 30)
    )

    result <- get_gas_demand_apparent(
      entsog_data = entsog_data,
      years = 2024,
      use_agsi_for_storage = FALSE
    )

    expect_equal(result$value_m3[result$iso2 == "DE" & result$date == as.Date("2024-01-01")], 10)
    expect_equal(result$value_m3[result$iso2 == "DE" & result$date == as.Date("2024-01-02")], 20)
    expect_equal(result$value_m3[result$iso2 == "DE" & result$date == as.Date("2024-01-03")], 30)
  }
)
