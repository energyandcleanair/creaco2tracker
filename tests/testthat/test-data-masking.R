library(testthat)
library(dplyr)


test_that(
  "apply_source_data_mask applies source-specific and global rules",
  {
    gas <- tibble(
      iso2 = c("EU", "EU", "DE"),
      date = as.Date(c("2024-01-01", "2024-02-01", "2024-02-01")),
      value = c(10, 20, 30)
    )

    masks <- list(
      all = list(date_to = "2024-01-31", iso2 = "EU"),
      gas_demand = list(
        list(date_from = "2024-02-01", iso2 = "DE")
      )
    )

    masked <- apply_source_data_mask(gas, "gas_demand", masks)

    expect_true(is.na(masked$value[1]))
    expect_false(is.na(masked$value[2]))
    expect_true(is.na(masked$value[3]))
  }
)


test_that(
  "apply_source_data_mask supports source aliases and custom value_cols",
  {
    pwr <- tibble(
      iso2 = c("EU", "EU"),
      source = c("Wind", "Solar"),
      date = as.Date(c("2024-01-01", "2024-01-01")),
      value_mw = c(100, 120),
      value_mwh = c(2400, 2500)
    )

    masks <- list(
      power_generation = list(
        list(date_from = "2024-01-01", source = "Wind", value_cols = "value_mwh")
      )
    )

    masked <- apply_source_data_mask(pwr, "power", masks)

    expect_true(is.na(masked$value_mwh[masked$source == "Wind"]))
    expect_false(is.na(masked$value_mw[masked$source == "Wind"]))
    expect_false(is.na(masked$value_mwh[masked$source == "Solar"]))
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
        "gas_demand",
        "power_generation",
        "eurostat_cons",
        "eurostat_indprod",
        "weather"
      ) %in% names(cfg))
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
  "data_masking_as_of applies the default publication lags",
  {
    cfg <- data_masking_as_of("2022-06-01")

    expect_equal(cfg$entsoe_power_daily$date_to, "2022-05-30")
    expect_equal(cfg$ember_power_monthly$date_to, "2022-03-01")
    expect_equal(cfg$ember_power_yearly$date_to, "2020-01-01")
    expect_equal(cfg$eurostat_gas_monthly$date_to, "2022-01-01")
    expect_equal(cfg$weather$date_to, "2022-05-30")
    expect_equal(cfg$all, list())
    expect_equal(cfg$gas_demand, list())
  }
)


test_that(
  "data_masking_as_of respects custom lag overrides",
  {
    cfg <- data_masking_as_of(
      "2022-06-01",
      lags = c(eurostat_gas_monthly = 0L, weather = 5L)
    )

    expect_equal(cfg$eurostat_gas_monthly$date_to, "2022-06-01")
    expect_equal(cfg$weather$date_to, "2022-05-27")
    expect_equal(cfg$entsoe_power_daily$date_to, "2022-05-30")
  }
)


test_that(
  "data_masking_as_of_batch returns named configs",
  {
    cfgs <- data_masking_as_of_batch(c("2022-01-01", "2023-01-01"))

    expect_named(cfgs, c("2022-01-01", "2023-01-01"))
    expect_equal(length(cfgs), 2)
    expect_true(is.list(cfgs[[1]]))
    expect_equal(cfgs[[1]]$weather$date_to, "2021-12-30")
    expect_equal(cfgs[[2]]$eurostat_gas_monthly$date_to, "2022-08-01")
  }
)


test_that(
  "data_masking_as_of accepts zero-day overrides",
  {
    cfg <- data_masking_as_of("2022-06-01", lags = c(weather = 0L))

    expect_equal(cfg$weather$date_to, "2022-06-01")
  }
)


test_that(
  "granular entsoe masking masks both value columns",
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

    masked <- apply_source_data_mask(pwr, "entsoe_daily", masks)

    expect_true(is.na(masked$value_mw[masked$source == "Wind"]))
    expect_true(is.na(masked$value_mwh[masked$source == "Wind"]))
    expect_false(is.na(masked$value_mw[masked$source == "Solar"]))
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

    masked <- apply_source_data_mask(entsog_raw, "entsog_raw", masks)

    expect_equal(masked$value_m3[1], 1)
    expect_true(is.na(masked$value_m3[2]))
    expect_true(is.na(masked$value_m3[3]))
  }
)


test_that(
  "granular eurostat monthly masking uses time column",
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
    expect_true(is.na(masked$values[2]))
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
  "get_gas_demand_consdist preserves explicitly masked gaps",
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
    expect_true(is.na(result$value_m3[result$date == as.Date("2024-01-02")]))
    expect_equal(result$value_m3[result$date == as.Date("2024-01-03")], 30)
  }
)


test_that(
  "get_gas_demand_apparent preserves explicitly masked gaps",
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
    expect_true(is.na(result$value_m3[result$iso2 == "DE" & result$date == as.Date("2024-01-02")]))
    expect_equal(result$value_m3[result$iso2 == "DE" & result$date == as.Date("2024-01-03")], 30)
  }
)
