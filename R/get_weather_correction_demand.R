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
  bind_rows(elec_factors, gas_factors)
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

  # Expand EU to member countries
  if (all(iso2s == "EU")) {
    iso2s <- get_eu_iso2s(include_eu = FALSE)
  }

  # Get power generation data
  pwr <- entsoe.get_power_generation(date_from = date_from, use_cache = use_cache)

  # Get weather data (HDD and CDD)
  weather_raw <- get_weather(variable = "HDD,CDD", region_id = iso2s)
  weather <- fill_weather(weather_raw)

  # Filter to requested date range
  weather <- weather %>%
    filter(date >= as.Date(date_from), date <= as.Date(date_to))

  # Process each country separately
  country_factors <- purrr::map_dfr(iso2s, function(country) {

    # Get country-specific power generation (Total)
    country_pwr <- pwr %>%
      filter(iso2 == country, source == 'Total') %>%
      mutate(value_gw = value_mw / 1000) %>%
      select(date, value_gw)

    # Check if we have sufficient data
    if (nrow(country_pwr) < 100) {
      warning(glue("Insufficient power data for {country}, returning factor=1.0"))
      return(tibble(
        iso2 = country,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fuel = "electricity",
        sector = "total",
        correction_factor = 1.0,
        hdd = NA_real_,
        cdd = NA_real_
      ))
    }

    # Get country-specific weather
    country_weather <- weather %>%
      filter(region_id == country) %>%
      select(date, variable, value) %>%
      spread(variable, value)

    # Merge power and weather data
    modeldata <- country_pwr %>%
      left_join(country_weather, by = "date") %>%
      filter(!is.na(HDD), !is.na(CDD), !is.na(value_gw)) %>%
      mutate(wday = lubridate::wday(date))

    # Check for sufficient merged data
    if (nrow(modeldata) < 100) {
      warning(glue("Insufficient merged data for {country}, returning factor=1.0"))
      return(tibble(
        iso2 = country,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fuel = "electricity",
        sector = "total",
        correction_factor = 1.0,
        hdd = NA_real_,
        cdd = NA_real_
      ))
    }

    # Fit regression model: demand ~ HDD + CDD + day-of-week
    model <- lm(value_gw ~ HDD + CDD + as.factor(wday), data = modeldata)

    # Predict with actual weather
    modeldata$value_pred <- predict(model, modeldata)

    # Calculate mean predicted value (what demand would be with average weather)
    mean_pred <- mean(modeldata$value_pred, na.rm = TRUE)

    # Calculate correction factor: weather-corrected / actual
    # weather-corrected = mean_pred + (actual - predicted)
    modeldata <- modeldata %>%
      mutate(
        anomaly = value_gw - value_pred,
        value_weather_corrected = mean_pred + anomaly,
        correction_factor = value_weather_corrected / value_gw
      )

    # Cap correction factors at reasonable bounds
    modeldata <- modeldata %>%
      mutate(
        correction_factor = pmax(0.5, pmin(1.5, correction_factor))
      )

    # Save diagnostics if requested
    if (!is.null(diagnostics_folder)) {
      create_dir(diagnostics_folder)

      # Time series plot
      plot_data <- modeldata %>%
        mutate(year = lubridate::year(date),
               plotdate = date %>% 'year<-'(2022)) %>%
        pivot_longer(c(value_gw, value_pred, value_weather_corrected),
                     names_to = 'measure')

      p <- plot_data %>%
        filter(measure == 'value_weather_corrected') %>%
        group_by(year) %>%
        mutate(value = zoo::rollmean(value, k = 7, fill = NA, align = "right")) %>%
        ggplot(aes(plotdate, value, col = as.factor(year))) +
        geom_line(linewidth = 1) +
        labs(title = glue("{country} - Weather Corrected Electricity Demand"),
             y = "GW (7-day mean)",
             x = "",
             col = "year") +
        theme_crea_new() +
        scale_color_crea_d('change', col.index = c(1:3, 5:7)) +
        scale_x_date(date_labels = '%b')

      quicksave(file.path(diagnostics_folder,
                         glue("electricity_demand_{country}.png")),
               plot = p)
    }

    # Return correction factors
    modeldata %>%
      select(date, value_gw, value_pred, correction_factor, HDD, CDD) %>%
      mutate(
        iso2 = country,
        fuel = "electricity",
        sector = "total"
      ) %>%
      rename(hdd = HDD, cdd = CDD) %>%
      select(iso2, date, fuel, sector, correction_factor, hdd, cdd)
  })

  # Add EU aggregate if we have all member countries
  eu_iso2s <- get_eu_iso2s(include_eu = FALSE)
  if (all(eu_iso2s %in% unique(country_factors$iso2))) {

    # Get EU total power generation for weighting
    eu_pwr <- pwr %>%
      filter(iso2 %in% eu_iso2s, source == 'Total') %>%
      group_by(date) %>%
      summarise(total_mw = sum(value_mw, na.rm = TRUE), .groups = "drop")

    # Calculate demand-weighted average correction factor
    eu_factors <- country_factors %>%
      left_join(pwr %>%
                 filter(iso2 %in% eu_iso2s, source == 'Total') %>%
                 select(iso2, date, value_mw),
               by = c("iso2", "date")) %>%
      group_by(date) %>%
      summarise(
        correction_factor = sum(correction_factor * value_mw, na.rm = TRUE) /
                           sum(value_mw, na.rm = TRUE),
        hdd = mean(hdd, na.rm = TRUE),
        cdd = mean(cdd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU",
        fuel = "electricity",
        sector = "total"
      ) %>%
      select(iso2, date, fuel, sector, correction_factor, hdd, cdd)

    country_factors <- bind_rows(country_factors, eu_factors)
  }

  return(country_factors)
}


#' Get Weather Correction Factors for Gas Demand (excluding power sector)
#'
#' @inheritParams get_weather_correction_demand
#' @return Tibble with gas demand correction factors
#' @keywords internal
get_weather_correction_demand_gas <- function(iso2s = "EU",
                                              date_from = "2020-01-01",
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
    filter(fuel == "Gas",
           !sector %in% c("Electricity", "All")) %>%
    group_by(iso2, date) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    select(iso2, date, value)

  non_power_sectors <- co2 %>% filter(fuel == "Gas",
           !sector %in% c("Electricity", "All")) %>%
    distinct(sector) %>%
    pull(sector) %>%
  # Recode from db names to local names
  # Database names were made display-friendly for Flourish.
  # TODO: Ideally should have sector_code and sector_label in the db.
  recode("Others" = SECTOR_OTHERS)

  # Get weather data (HDD only for gas)
  weather_raw <- get_weather(variable = "HDD", region_id = iso2s)
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

    # Create weather-averaged data
    model_data_averaged <- model_data %>%
      group_by(iso2, yday) %>%
      mutate(hdd = mean(hdd, na.rm = TRUE)) %>%
      ungroup()

    # Predict with actual weather
    model_data_averaged$value_pred <- predict(model, model_data_averaged)

    # Calculate correction factor
    model_data_averaged <- model_data_averaged %>%
      mutate(correction_factor = value_pred / value)

    # Return correction factors
    model_data_averaged %>%
      select(iso2, date, correction_factor) %>%
      tidyr::complete(
        iso2,
        date = seq(as.Date(date_from), as.Date(date_to), by = "day"),
        fill = list(correction_factor = 1.0)) %>%
      tidyr::crossing(sector = non_power_sectors)
  }) %>%
    bind_rows()

  # Diagnostic plots
  if (!is.null(diagnostics_folder) && nrow(correction_factors) > 0) {
    create_dir(diagnostics_folder)
    plot_gas_demand_correction(correction_factors, diagnostics_folder)
  }


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
plot_gas_demand_correction <- function(correction_factors, diagnostics_folder) {

  # Aggregate correction factors by year and sector
  plot_data <- correction_factors %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(iso2, year, sector) %>%
    summarise(correction_factor = mean(correction_factor, na.rm = TRUE), .groups = "drop")

  # Create heatmap
  p <- plot_data %>%
    ggplot(aes(x = factor(year), y = sector)) +
    geom_tile(aes(fill = correction_factor), color = "white", linewidth = 0.5) +
    facet_wrap(~iso2) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 1,
      name = "Correction\nFactor"
    ) +
    labs(
      title = "Gas Demand Weather Correction Factors",
      subtitle = "Averaged per year and sector. Weather-corrected emissions= Factor * actual emissions",
      x = NULL,
      y = "Sector"
    ) +
    theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  quicksave(
    file.path(diagnostics_folder, "gas_demand_correction_factors.png"),
    plot = p,
    width = 12,
    height = 8,
    preview=F
  )
}
