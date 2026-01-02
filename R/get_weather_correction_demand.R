#' Get Weather Correction Factors for Energy Demand
#'
#' Calculates weather correction factors for electricity and gas demand using
#' temperature variables (HDD/CDD). Returns correction factors that can be
#' applied to CO2 emissions data.
#'
#' @param iso2s Character vector of ISO2 country codes. Default is "EU".
#' @param date_from Start date for analysis. Default is "2020-01-01".
#' @param date_to End date for analysis. Default is today.
#' @param use_cache Whether to use cached data. Default is TRUE.
#' @param diagnostics_folder Optional folder path for saving diagnostic plots.
#'
#' @return A tibble with columns:
#'   - iso2: Country code
#'   - date: Date
#'   - fuel: "electricity" or "fossil_gas"
#'   - sector: "total" (for electricity) or "except_power" (for gas)
#'   - correction_factor: Numeric correction factor (typically 0.8 to 1.2)
#'   - hdd: Heating degree days (for diagnostics)
#'   - cdd: Cooling degree days (for diagnostics, electricity only)
#'
#' @details
#' This function combines electricity and gas demand weather corrections.
#' Electricity uses both HDD and CDD, while gas uses only HDD.
#' The correction factor represents: corrected_demand / actual_demand
#'
#' @export
get_weather_correction_demand <- function(iso2s = "EU",
                                          date_from = "2015-01-01",
                                          date_to = Sys.Date(),
                                          use_cache = TRUE,
                                          diagnostics_folder = "diagnostics/weather_correction") {

  # Get both electricity and gas correction factors
  elec_factors <- get_weather_correction_demand_elec(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    diagnostics_folder = diagnostics_folder
  )

  gas_factors <- get_weather_correction_demand_gas(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    diagnostics_folder = diagnostics_folder
  )

  # Combine results
  correction_factors <- bind_rows(elec_factors, gas_factors)

  # Diagnostic plots
  if (!is.null(diagnostics_folder) && nrow(correction_factors) > 0) {
    create_dir(diagnostics_folder)
    plot_demand_correction(correction_factors, diagnostics_folder)
  }
 
  return(correction_factors)
}


#' Get Weather Correction Factors for Electricity Demand
#'
#' @inheritParams get_weather_correction_demand
#' @return Tibble with electricity demand correction factors
#' @keywords internal
get_weather_correction_demand_elec <- function(iso2s = "EU",
                                               date_from = "2020-01-01",
                                               date_to = Sys.Date(),
                                               use_cache = TRUE,
                                               diagnostics_folder = NULL) {

  # Only support EU for now
  stopifnot("Only EU is supported for electricity demand correction" = all(iso2s == "EU"))

  # Get power generation data
  pwr <- entsoe.get_power_generation(date_from = date_from, use_cache = use_cache)

  # Get weather data (HDD and CDD)
  weather_raw <- get_weather(variable = "HDD,CDD", region_id = iso2s)
  weather <- fill_weather(weather_raw)

  # Filter to requested date range
  weather <- weather %>%
    filter(date >= as.Date(date_from), date <= as.Date(date_to))

  # Process each country separately
  correction_factors <- pblapply(iso2s, function(iso2) {

    # Get country-specific power generation (Total)
    country_pwr <- pwr %>%
      filter(iso2 == !!iso2, source == 'Total') %>%
      select(date, value_mw)

    # Check if we have sufficient data
    if (nrow(country_pwr) < 100) {
      warning(glue("Insufficient power data for {country}, returning factor=1.0"))
      return(tibble(
        iso2 = iso2,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        sector = SECTOR_ELEC,
        correction_factor = 1.0
      ))
    }

    # Get country-specific weather
    country_weather <- weather %>%
      filter(region_iso2 == iso2) %>%
      select(date, variable, value) %>%
      spread(variable, value)

    # Merge power and weather data
    model_data <- country_pwr %>%
      left_join(country_weather, by = "date") %>%
      filter(!is.na(hdd), !is.na(cdd), !is.na(value_mw)) %>%
      mutate(wday = lubridate::wday(date),
             yday = lubridate::yday(date))

    # Check for sufficient merged data
    if (nrow(model_data) < 100) {
      warning(glue("Insufficient merged data for {country}, returning factor=1.0"))
      return(tibble(
        iso2 = iso2,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        sector = SECTOR_ELEC,
        correction_factor = 1.0
      ))
    }

    # Fit regression model: demand ~ HDD + CDD + day-of-week
    model <- lm(value_mw ~ hdd + cdd + as.factor(wday), data = model_data)

    # Show model summary
    # summary(model)

    # Predict at ACTUAL weather (fitted values)
    model_data$fitted_actual <- predict(model, model_data)

    # Create weather-averaged data
    model_data_averaged <- model_data %>%
      group_by(yday) %>%
      mutate(hdd = mean(hdd, na.rm = TRUE),
             cdd = mean(cdd, na.rm = TRUE)) %>%
      ungroup()

    # Predict at NORMAL weather (weather-averaged)
    model_data_averaged$fitted_normal <- predict(model, model_data_averaged)

    # Calculate correction factor: 1 + (fitted_normal - fitted_actual) / value
    model_data_averaged <- model_data_averaged %>%
      mutate(correction_factor = 1 + (fitted_normal - fitted_actual) / value_mw)

    # Return correction factors
    model_data_averaged %>%
      select(date, correction_factor) %>%
      tidyr::complete(
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fill = list(correction_factor = 1.0)) %>%
      tidyr::crossing(sector = SECTOR_ELEC,
                      fuel = FUEL_TOTAL,
                      iso2 = iso2)

    }) %>%
    bind_rows()

  return(correction_factors)
}


#' Get Weather Correction Factors for Gas Demand (excluding power sector)
#'
#' @inheritParams get_weather_correction_demand
#' @return Tibble with gas demand correction factors
#' @keywords internal
get_weather_correction_demand_gas <- function(iso2s = "EU",
                                              date_from = "2015-01-01",
                                              date_to = Sys.Date(),
                                              use_cache = TRUE,
                                              diagnostics_folder = NULL) {

  # Expand EU to member countries
  stopifnot("demand correction at country level is not supported yet" = all(iso2s == "EU"))

  # Get CO2 data for gas demand (outside power sector)
  co2 <- if (use_cache) {
    download_co2()
  } else {
    get_co2(diagnostics_folder = diagnostics_folder, downscale_daily = TRUE)
  }

  # Extract gas demand (excluding power sector)
  gas_demand <- co2 %>%
    filter(fuel == FUEL_GAS,
           !sector %in% c(SECTOR_ELEC, SECTOR_ALL)) %>%
    group_by(iso2, date) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    select(iso2, date, value) %>%
    filter(date >= as.Date(date_from))

  non_power_sectors <- co2 %>%
    filter(
      fuel == FUEL_GAS,
      !sector %in% c(SECTOR_ELEC, SECTOR_ALL)) %>%
    distinct(sector) %>%
    pull(sector)
  
  # Get weather data (HDD only for gas)
  weather_raw <- get_weather(variable = "HDD", region_id = iso2s, use_cache = use_cache)
  weather <- fill_weather(weather_raw)

  # Filter to requested date range
  weather <- weather %>%
    filter(date >= as.Date(date_from), date <= as.Date(date_to))

  # Process each country separately
  correction_factors <- pblapply(iso2s, function(iso2) {

    # Get country-specific gas demand
    country_gas <- gas_demand %>%
      filter(iso2 == !!iso2) # date >= as.Date("2021-01-01")


    # Get country-specific weather
    country_weather <- weather %>%
      filter(region_id == !!iso2) %>%
      select(date, variable, value) %>%
      spread(variable, value)

    # Merge gas and weather data
    model_data <- country_gas %>%
      left_join(country_weather, by = "date") %>%
      filter(!is.na(hdd), !is.na(value)) %>%
      mutate(wday = lubridate::wday(date),
             yday = lubridate::yday(date)
      )

    # Gas demand derived from CO2 is monthly data before certain dates and daily after that
    # The date changes by country, and is based on when ENTSOG starts matching EUROSTAT data
    # To reduce biases, we use monthly data for the training
    # But keep daily records for convenience purposes
    model_data <- model_data %>%
      group_by(iso2, date_month = floor_date(date, "month")) %>%
      mutate_at(vars(value, hdd), mean, na.rm = TRUE) %>%
      ungroup() %>%
      select(-date_month)

    # Check for sufficient merged data
    if (nrow(model_data) < 100) {
      warning(glue("Insufficient merged data for {iso2}, returning factor=1.0"))
      return(tibble(
        iso2 = iso2,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fuel = "fossil_gas",
        sector = "except_power",
        correction_factor = 1.0,
        hdd = NA_real_,
        cdd = NA_real_
      ) %>%
        tidyr::crossing(sector = non_power_sectors))
    }

    # Fit regression model: gas demand ~ HDD + day-of-week
    model <- lm(value ~ hdd + as.factor(wday), data = model_data)

    # Show model summary
    # summary(model)

    # Predict at ACTUAL weather (fitted values)
    model_data$fitted_actual <- predict(model, model_data)

    # Create weather-averaged data (normal weather conditions)
    model_data_averaged <- model_data %>%
      group_by(iso2, yday) %>%
      mutate(hdd = mean(hdd, na.rm = TRUE)) %>%
      ungroup()

    # Predict at NORMAL weather (weather-averaged)
    model_data_averaged$fitted_normal <- predict(model, model_data_averaged)

    # Calculate correction factor: 1 + (fitted_normal - fitted_actual) / value
    model_data_averaged <- model_data_averaged %>%
      mutate(correction_factor = 1 + (fitted_normal - fitted_actual) / value)
    
    # Return correction factors
    model_data_averaged %>%
      select(date, correction_factor) %>%
      tidyr::complete(
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fill = list(correction_factor = 1.0)) %>%
      tidyr::crossing(sector = non_power_sectors,
                      iso2 = iso2,
                      fuel = FUEL_GAS)
  }) %>%
    bind_rows()

  return(correction_factors)
}


#' Plot Gas Demand Weather Correction Diagnostics
#'
#' Creates a heatmap showing gas demand correction factors averaged per year and per sector.
#'
#' @param correction_factors Data frame with columns: iso2, date, sector, correction_factor
#' @param diagnostics_folder Path to save diagnostic plot
#'
#' @return NULL (plot is saved to file)
#' @keywords internal
plot_demand_correction <- function(correction_factors, diagnostics_folder) {

  # Aggregate correction factors by year and sector
  plot_data <- correction_factors %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(iso2, year, sector, fuel) %>%
    summarise(correction_factor = mean(correction_factor, na.rm = TRUE), .groups = "drop")

  # Create heatmap
  p <- plot_data %>%
    ggplot(aes(x = factor(year), y = iso2)) +
    geom_tile(aes(fill = correction_factor), color = "white", linewidth = 0.5) +
    facet_wrap(fuel_code_to_label(fuel)~sector_code_to_label(sector)) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 1,
      name = "Correction\nFactor"
    ) +
    labs(
      title = "Demand Weather Correction Factors",
      subtitle = "Averaged per year and sector. Weather-corrected demand = Correction factor * Observed demand",
      x = NULL,
      y = NULL
    ) +
    theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  quicksave(
    file.path(diagnostics_folder, "demand_correction_factors.png"),
    plot = p,
    width = 12,
    height = 8,
    preview=F
  )
}
