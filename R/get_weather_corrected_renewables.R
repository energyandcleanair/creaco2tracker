#' Get Weather-Corrected Renewable Generation
#'
#' Retrieves weather-corrected generation for renewable sources (wind, solar)
#' and compares with actual generation to calculate the delta.
#'
#' @inheritParams get_weather_corrected_co2_power
#' @return Data frame with actual and weather-corrected renewable generation
#'
#' @keywords internal
get_weather_corrected_renewables <- function(iso2s,
                                             date_from,
                                             pwr_generation,
                                             sources = c("wind", "solar", "hydro"),
                                             use_cache = TRUE) {

  renewable_data <- list()

  # Get weather-corrected wind
  if ("wind" %in% tolower(sources)) {
    renewable_data[["wind"]] <- get_weather_corrected_wind(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      use_cache = use_cache
    )
  }

  # Get weather-corrected solar
  if ("solar" %in% tolower(sources)) {
    renewable_data[["solar"]] <- get_weather_corrected_solar(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      use_cache = use_cache
    )
  }

  # Get weather-corrected hydro
  if ("hydro" %in% tolower(sources)) {
    renewable_data[["hydro"]] <- get_weather_corrected_hydro(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      use_cache = use_cache
    )
  }

  # Combine all renewable sources
  result <- dplyr::bind_rows(renewable_data)

  return(result)
}

#' Get Weather-Corrected Wind Power Generation
#'
#' Corrects wind power generation for weather variations using wind speed data.
#' Uses regression to remove the effect of wind speed variations, similar to
#' temperature correction in get_corrected_demand.
#'
#' @param countries Character vector of ISO2 country codes. Use "EU" for all EU countries,
#'   or specify individual countries like c("DE", "FR"). Default is "EU".
#' @param date_from Start date for analysis (default: "2020-01-01")
#' @param diagnostics_folder Path to save diagnostic plots and outputs
#' @param use_cache Logical, whether to use cached data (default: TRUE)
#' @param use_local Logical, whether to use localhost API for weather data (default: FALSE)
#'
#' @return A data frame with weather-corrected wind generation matching the power
#'   generation format (columns: date, iso2, source, value_mw, value_mwh, etc.)
#'
#' @export
get_weather_corrected_wind <- function(iso2s = "EU",
                                       pwr_generation,
                                       date_from = "2020-01-01",
                                       diagnostics_folder = 'diagnostics/wind_correction',
                                       use_cache = TRUE,
                                       use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Handle EU case
  if (length(iso2s) == 1 && "EU" == iso2s) {
    iso2s <- get_eu_iso2s(include_eu = FALSE)
  }

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date)==pwr_generation$date))

  # Filter for wind generation in specified countries
  wind_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("wind", source, ignore.case = TRUE))


  message("Getting wind speed and temperature data from CREA DB")
  # Get both wind speed and temperature in one call
  weather <- tryCatch({
    get_weather(
      variable = "ws50m_daily,t10m_daily",
      region_iso2 = paste(iso2s, collapse = ","),
      region_type = "station",
      station_source = "gem",
      date_from = date_from,
      use_cache = use_cache,
      use_local = use_local,
      aggregate_by = "region_iso2,date",
      aggregate_fn = "mean"
    )
  }, error = function(e) {
    warning(sprintf("Failed to get weather data: %s", e$message))
    return(NULL)
  })

  if (is.null(weather) || nrow(weather) == 0) {
    stop("No weather data retrieved. Check API availability and parameters.")
  }

  # Ensure these are daily as well
  stopifnot(all(lubridate::date(weather$date)==weather$date))


  # Merge wind generation with weather data
  model_data <- wind_generation %>%
    select(iso2, date, value_mwh) %>%
    left_join(weather %>%
                select(iso2=region_iso2, date, variable, value) %>%
                tidyr::pivot_wider(names_from = variable, values_from = value),
              by = c("iso2", "date")) %>%
    filter(complete.cases(.)) %>%
    # Add time vars
    mutate(
      yday = lubridate::yday(date),  # Day of year for climatology
      year = lubridate::year(date),
      year_month = lubridate::floor_date(date, "month"),
      month = lubridate::month(date)
    ) %>%
    # Add physics-based feature
    mutate(
      ws_1 = ws50m_daily,
      ws_2 = ws50m_daily^2,
      ws_3 = ws50m_daily^3,
      inv_temp = 1 / (t10m_daily + 273.15),
      temp = (t10m_daily + 273.15)
    )

  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_data <- pblapply(iso2s, function(country_iso2) {

    model_data_country <- model_data %>%
      filter(iso2 == country_iso2)

    # Check if we have data
    if (nrow(model_data_country) == 0) {
      warning(sprintf("No data for %s, skipping", country_iso2))
      return(NULL)
    }

    if (nrow(model_data_country) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", country_iso2, nrow(model_data)))
      return(NULL)
    }

    tryCatch({

      # Fit random forest model
      formula_str <- paste("value_mwh ~ year  + ws_1 + ws_2 + ws_3 + inv_temp + temp")
      model_formula <- as.formula(formula_str)
      model <- randomForest::randomForest(
        model_formula,
        data = model_data_country,
        ntree = 1000,
        importance = TRUE
      )

      # Plot importance
      # randomForest::varImpPlot(model)

      # Create average climatology
      model_data_averaged <- model_data_country %>%
        group_by(lubridate::yday(date)) %>%
        mutate_at(
          vars(ws_1, ws_2, ws_3, inv_temp, temp),
          list(~ mean(., na.rm = TRUE)),
          .groups = "drop"
        )

      # Predict actual generation
      model_data_country$value_pred <- predict(model, model_data_averaged)

      # Scale to have exact same generation figures
      model_data_country$value_pred <- model_data_country$value_pred *
        (sum(model_data_country$value_mwh) / sum(model_data_country$value_pred))

      # Plot comparison with EMBER using helper function
      if(!is.null(diagnostics_folder)){
        plot_corrected_vs_ember(model_data_country, country_iso2, "wind", diagnostics_folder)
      }

      # Return deweathered data
      model_data_country %>%
        select(date, iso2, value_mwh=value_pred) %>%
        mutate(
          source = "Wind"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", country_iso2, e$message))
      return(NULL)
    })
  }) %>%
    bind_rows()

  # Add EU total
  if (all(get_eu_iso2s() %in% corrected_data$iso2)) {
    eu_total <- corrected_data %>%
      group_by(date, source) %>%
      summarise(
        value_mw = sum(value_mw, na.rm = TRUE),
        value_mwh = sum(value_mwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  return(corrected_data)
}


#' Get Weather-Corrected Solar Power Generation
#'
#' Corrects solar power generation for weather variations using solar radiation data.
#' Uses regression to remove the effect of solar radiation variations, similar to
#' temperature correction in get_corrected_demand.
#'
#' @param iso2s Character vector of ISO2 country codes. Use "EU" for all EU countries,
#'   or specify individual countries like c("DE", "FR"). Default is "EU".
#' @param date_from Start date for analysis (default: "2020-01-01")
#' @param diagnostics_folder Path to save diagnostic plots and outputs
#' @param use_cache Logical, whether to use cached data (default: TRUE)
#' @param use_local Logical, whether to use localhost API for weather data (default: FALSE)
#'
#' @return A data frame with weather-corrected solar generation matching the power
#'   generation format (columns: date, iso2, source, value_mwh, etc.)
#'
#' @export
get_weather_corrected_solar <- function(iso2s = "EU",
                                        pwr_generation,
                                        date_from = "2020-01-01",
                                        diagnostics_folder = 'diagnostics/solar_correction',
                                        use_cache = TRUE,
                                        use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Handle EU case
  if (length(iso2s) == 1 && "EU" == iso2s) {
    iso2s <- get_eu_iso2s(include_eu = FALSE)
  }

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date) == pwr_generation$date))

  # Filter for solar generation in specified countries
  solar_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("solar", source, ignore.case = TRUE))

  message("Getting solar radiation data from CREA DB")
  # Get solar radiation data
  weather <- tryCatch({
    get_weather(
      variable = "solar_radiation",
      region_iso2 = paste(iso2s, collapse = ","),
      region_type = "country",
      date_from = date_from,
      use_cache = use_cache,
      use_local = use_local
    )
  }, error = function(e) {
    warning(sprintf("Failed to get weather data: %s", e$message))
    return(NULL)
  })

  if (is.null(weather) || nrow(weather) == 0) {
    stop("No weather data retrieved. Check API availability and parameters.")
  }

  # Ensure daily data
  stopifnot(all(lubridate::date(weather$date) == weather$date))

  # Merge solar generation with weather data
  model_data <- solar_generation %>%
    select(iso2, date, value_mwh) %>%
    left_join(weather %>%
                select(iso2 = region_iso2, date, variable, value) %>%
                tidyr::pivot_wider(names_from = variable, values_from = value),
              by = c("iso2", "date")) %>%
    filter(complete.cases(.)) %>%
    # Add time vars
    mutate(
      yday = lubridate::yday(date),
      yday_weather = lubridate::yday(date),  # A weather variable that is basically yday. Separating it for clarity.
      year = lubridate::year(date),
      year_month = lubridate::floor_date(date, "month"),
      month = lubridate::month(date)
    )

  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_data <- pblapply(iso2s, function(country_iso2) {

    model_data_country <- model_data %>%
      filter(iso2 == country_iso2)

    # Check if we have data
    if (nrow(model_data_country) == 0) {
      warning(sprintf("No data for %s, skipping", country_iso2))
      return(NULL)
    }

    if (nrow(model_data_country) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", country_iso2, nrow(model_data_country)))
      return(NULL)
    }

    tryCatch({

      # Fit random forest model
      formula_str <- paste("value_mwh ~ year + yday_weather + solar_radiation")
      model_formula <- as.formula(formula_str)
      model <- randomForest::randomForest(
        model_formula,
        data = model_data_country,
        ntree = 1000,
        importance = TRUE
      )

      # Plot importance
      # randomForest::varImpPlot(model)

      # Create averaged data with climatological weather
      model_data_averaged <- model_data_country %>%
        group_by(lubridate::yday(date)) %>%
        mutate_at(
          vars(solar_radiation),
          list(~ mean(., na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(yday_weather = 0)

      # Predict deweathered generation
      model_data_country$value_pred <- predict(model, model_data_averaged)

      # Scale to have exact same generation figures
      model_data_country$value_pred <- model_data_country$value_pred *
        (sum(model_data_country$value_mwh) / sum(model_data_country$value_pred))

      # Plot comparison with EMBER using helper function
      plot_corrected_vs_ember(model_data_country, country_iso2, "solar", diagnostics_folder)

      # Return deweathered data
      model_data_country %>%
        select(date, iso2, value_mwh = value_pred) %>%
        mutate(
          source = "Solar"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", country_iso2, e$message))
      return(NULL)
    })
  }) %>%
    bind_rows()

  # Add EU total
  if (all(get_eu_iso2s() %in% corrected_data$iso2)) {
    eu_total <- corrected_data %>%
      group_by(date, source) %>%
      summarise(
        value_mwh = sum(value_mwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  return(corrected_data)
}


#' Get Weather-Corrected Hydro Power Generation
#'
#' Corrects hydro power generation using capacity-based approach:
#' - Uses ENTSOE capacity data where available
#' - For countries without capacity data, assumes constant capacity
#' - Calculates 10-year rolling average capacity factor
#' - Weather-corrected generation = capacity × mean CF × 24 hours
#' - Scales daily values to match actual yearly total
#'
#' @param iso2s Character vector of ISO2 country codes. Use "EU" for all EU countries,
#'   or specify individual countries like c("DE", "FR"). Default is "EU".
#' @param pwr_generation Data frame with power generation data from entsoe.get_power_generation
#' @param date_from Start date for analysis (default: "2015-01-01")
#' @param diagnostics_folder Path to save diagnostic plots and outputs
#' @param use_cache Logical, whether to use cached data (default: TRUE)
#' @param use_local Logical, whether to use localhost API for weather data (default: FALSE)
#'
#' @return A data frame with weather-corrected hydro generation matching the power
#'   generation format (columns: date, iso2, source, value_mwh, etc.)
#'
#' @export
get_weather_corrected_hydro <- function(iso2s = "EU",
                                        pwr_generation,
                                        date_from = "2015-01-01",
                                        diagnostics_folder = 'diagnostics/hydro_correction',
                                        use_cache = TRUE,
                                        use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Handle EU case
  if (length(iso2s) == 1 && "EU" == iso2s) {
    iso2s <- get_eu_iso2s(include_eu = FALSE)
  }

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date) == pwr_generation$date))

  # Filter for hydro generation in specified countries
  hydro_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("hydro", source, ignore.case = TRUE))

  message("Getting ENTSOE capacity data")
  # Get ENTSOE installed capacity data for hydro
  capacity_raw <- tryCatch({
    entsoe.get_installed_capacity(iso2s = iso2s, date_from = date_from) %>%
      filter(grepl("hydro", source, ignore.case = TRUE)) %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(iso2, year) %>%
      summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")
  }, error = function(e) {
    warning(sprintf("Failed to get ENTSOE capacity data: %s", e$message))
    tibble(iso2 = character(), year = integer(), capacity_mw = numeric())
  })

  # Fill missing capacity data
  # 1. Forward fill within each country (2025 = 2024)
  # 2. Interpolate gaps
  # 3. Backward fill for early years
  # 4. For countries without capacity data, use a constant capacity (doesn't matter what level)
  all_years <- seq(lubridate::year(as.Date(date_from)), lubridate::year(Sys.Date()))

  capacity <- capacity_raw %>%
    complete(iso2 = iso2s, year = all_years) %>%
    group_by(iso2) %>%
    arrange(year) %>%
    # Forward fill (2025 uses 2024 capacity)
    fill(capacity_mw, .direction = "down") %>%
    # Backward fill for early years
    fill(capacity_mw, .direction = "up") %>%
    # Linear interpolation for gaps
    mutate(capacity_mw = zoo::na.approx(capacity_mw, na.rm = FALSE)) %>%
    ungroup() %>%
    # Fill countries with no capacity data with constant value
    group_by(iso2) %>%
    mutate(capacity_mw = case_when(
      all(is.na(capacity_mw)) ~ 1,
      TRUE ~ capacity_mw
    )) %>%
    ungroup()

  # We don't have much more than 10 years of data, so we use a simple rolling average
  message("Calculating 10-year rolling average capacity factors")

  # Merge generation with capacity
  cf <- hydro_generation %>%
    group_by(iso2, year = lubridate::year(date)) %>%
    summarise(value_mwh = mean(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    left_join(capacity, by = c("iso2", "year")) %>%
    mutate(cf = value_mwh / (capacity_mw * 24)) %>%
    group_by(iso2) %>%
    arrange(year) %>%
    mutate(cf_10yr = zoo::rollmean(cf, k = 10, fill = NA, align = "right")) %>%
    # Then fill backwards to fill missing values
    fill(cf_10yr, .direction = "up") %>%
    ungroup() %>%
    # Then derive a ratio to apply to generation time series
    mutate(ratio = cf_10yr / cf) %>%
    select(iso2, year, ratio)

  # Apply ratio to generation time series, by year
  corrected_data <- hydro_generation %>%
    mutate(year = lubridate::year(date)) %>%
    left_join(cf, by = c("iso2", "year")) %>%
    mutate(value_mwh_corrected = value_mwh * ratio) %>%
    # Scale to preserve total generation per country (across all years)
    group_by(iso2) %>%
    mutate(
      scaling_factor = sum(value_mwh, na.rm = TRUE) / sum(value_mwh_corrected, na.rm = TRUE),
      value_mwh = value_mwh_corrected * scaling_factor
    ) %>%
    ungroup() %>%
    select(date, iso2, value_mwh) %>%
    mutate(source = "Hydro")

  # Add EU total
  if (all(get_eu_iso2s() %in% corrected_data$iso2)) {
    eu_total <- corrected_data %>%
      group_by(date, source) %>%
      summarise(
        value_mwh = sum(value_mwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  return(corrected_data)
}


#' Plot Corrected Generation vs EMBER Capacity
#'
#' Helper function to create faceted plots comparing weather-corrected generation
#' with EMBER capacity estimates at both yearly and monthly aggregations.
#'
#' @param model_data Data frame with corrected generation predictions
#' @param country_iso2 ISO2 country code
#' @param source_type Either "wind", "solar", or "hydro"
#' @param diagnostics_folder Path to save diagnostic plot
#'
#' @return NULL (plot is saved to file)
plot_corrected_vs_ember <- function(model_data, country_iso2, source_type, diagnostics_folder) {

  # Get EMBER capacity data once
  ember_capacity_raw <- tryCatch({
    ember.get_installed_capacity(iso2s = country_iso2) %>%
      filter(grepl(source_type, source, ignore.case = TRUE)) %>%
      filter(date >= min(model_data$date))
  }, error = function(e) {
    warning(sprintf("Could not get EMBER data for %s: %s", country_iso2, e$message))
    tibble(date = as.Date(character()), value_mw = numeric())
  })

  # Aggregate to yearly
  ember_capacity_yearly <- ember_capacity_raw %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  # Aggregate to monthly
  ember_capacity_monthly <- ember_capacity_raw %>%
    mutate(year_month = lubridate::floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  # Prepare plot data for yearly comparison
  plot_data_yearly <- bind_rows(
    model_data %>%
      group_by(year) %>%
      summarise(value = mean(value_pred, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Corrected generation"),
    ember_capacity_yearly %>%
      select(year, value = capacity_mw) %>%
      mutate(source = "Ember capacity-based estimate")
  ) %>%
    group_by(source) %>%
    mutate(value = if(n() > 0 && !all(is.na(value))) value / value[year == min(year)] * 100 else NA_real_) %>%
    ungroup() %>%
    mutate(time_scale = "Yearly",
           date = as.Date(paste0(year, "-01-01")))

  # Prepare plot data for monthly comparison
  plot_data_monthly <- bind_rows(
    model_data %>%
      group_by(year_month) %>%
      summarise(value = mean(value_pred, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Corrected generation"),
    ember_capacity_monthly %>%
      select(year_month, value = capacity_mw) %>%
      mutate(source = "Ember capacity-based estimate")
  ) %>%
    group_by(source) %>%
    mutate(value = if(n() > 0 && !all(is.na(value))) value / value[year_month == min(year_month)] * 100 else NA_real_) %>%
    ungroup() %>%
    mutate(time_scale = "Monthly",
           date = year_month)

  # Combine and plot both
  plt_vs_ember <- bind_rows(plot_data_yearly, plot_data_monthly) %>%
    ggplot(aes(date, value, col = source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    facet_wrap(~time_scale, scales = "free_x", ncol = 1) +
    labs(
      title = paste("Weather-corrected", source_type, "generation vs Ember estimates -", country_iso2),
      y = "Index 100 (base period)",
      x = "",
      col = "Source"
    ) +
    theme_crea_new() +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", date_labels = "%Y") +
    rcrea::scale_y_crea_zero()

  quicksave(
    file.path(diagnostics_folder, paste0(source_type, "_corrected_vs_ember_", country_iso2, ".png")),
    plot = plt_vs_ember,
    preview = FALSE
  )
}
