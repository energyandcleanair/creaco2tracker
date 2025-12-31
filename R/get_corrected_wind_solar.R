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
get_corrected_wind <- function(countries = "EU",
                               date_from = "2020-01-01",
                               diagnostics_folder = 'diagnostics/wind_correction',
                               use_cache = TRUE,
                               use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Handle EU case
  if ("EU" %in% countries) {
    countries <- get_eu_iso2s()
  }

  message("Getting wind power generation data...")
  # Get wind power generation from ENTSOE
  pwr_generation_all <- entsoe.get_power_generation(
    date_from = date_from,
    use_cache = use_cache
  )

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation_all$date)==pwr_generation_all$date))

  # Filter for wind generation in specified countries
  wind_generation <- pwr_generation_all %>%
    filter(iso2 %in% countries) %>%
    filter(grepl("wind", source, ignore.case = TRUE))

  message("Getting wind speed and temperature data from weather stations...")
  # Get both wind speed and temperature in one call
  # Using aggregate_by='region_iso2,date' and aggregate_fn='mean' to aggregate stations by country
  weather_raw <- tryCatch({
    get_weather(
      variable = "ws50m_daily,t10m_daily",
      region_iso2 = paste(countries, collapse = ","),
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

  if (is.null(weather_raw) || nrow(weather_raw) == 0) {
    stop("No weather data retrieved. Check API availability and parameters.")
  }

  # Just ensure these are daily
  stopifnot(all(lubridate::date(weather_raw$date)==weather_raw$date))

  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_results <- lapply(countries, function(country_iso2) {

    print(country_iso2)

    # Get wind generation for this country
    country_wind <- wind_generation %>%
      filter(iso2 == country_iso2)

    # Get weather data for this country and pivot to wide format
    country_weather <- weather_raw %>%
      filter(region_iso2 == country_iso2) %>%
      select(date, variable, value) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value)

    # Check if we have data
    if (nrow(country_wind) == 0 || nrow(country_weather) == 0) {
      warning(sprintf("No data for %s, skipping", country_iso2))
      return(NULL)
    }

    # Merge wind generation with weather data
    model_data <- country_wind %>%
      left_join(country_weather, by = "date") %>%
      filter(complete.cases(.)) %>%
      mutate(
        yday = lubridate::yday(date),  # Day of year for climatology
        year = lubridate::year(date)
      )

    if (nrow(model_data) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", country_iso2, nrow(model_data)))
      return(NULL)
    }

    # Engineer physics-based feature
    model_data <- model_data %>%
      mutate(
        ws_1 = ws50m_daily,
        ws_2 = ws50m_daily^2,
        ws_3 = ws50m_daily^3,
        inv_temp = 1 / (t10m_daily + 273.15),
        temp = (t10m_daily + 273.15)
        )

    # Add time vars
    model_data <- model_data %>%
      mutate(
        year = lubridate::year(date),
        year_month = lubridate::floor_date(date, "month"),
        month = lubridate::month(date)
      )

    tryCatch({
      # Fit random forest model
      message(sprintf("  Fitting random forest for %s...", country_iso2))

      formula_str <- paste("value_mwh ~ year  + ws_1 + ws_2 + ws_3 + inv_temp + temp")
      model_formula <- as.formula(formula_str)
      message(sprintf("  Formula: %s", formula_str))

      model <- randomForest::randomForest(
        model_formula,
        data = model_data,
        ntree = 1000,
        importance = TRUE
      )

      # Plot importance
      randomForest::varImpPlot(model)

      # Create average climatology
      model_data_averaged <- model_data %>%
        group_by(lubridate::yday(date)) %>%
        mutate_at(
          vars(ws_1, ws_2, ws_3, inv_temp),
          list(~ mean(., na.rm = TRUE)),
          .groups = "drop"
        )

      # Predict actual generation
      model_data$value_pred <- predict(model, model_data_averaged)

      # Plot comparison with EMBER using helper function
      plot_corrected_vs_ember(model_data, country_iso2, "wind", diagnostics_folder)

      # Return deweathered data
      model_data %>%
        select(date, iso2, value_mwh_deweathered=value_pred) %>%
        rename(
          value_mwh = value_deweathered,
          value_mw = value_mw_deweathered
        ) %>%
        mutate(
          source = "Wind",
          data_source = "entsoe_deweathered",
          country = countrycode::countrycode(iso2, "iso2c", "country.name"),
          region = "EU"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", country_iso2, e$message))
      return(NULL)
    })
  })

  # Combine results
  corrected_data <- bind_rows(corrected_results)

  # Add EU total
  if (length(countries) > 1) {
    eu_total <- corrected_data %>%
      group_by(date) %>%
      summarise(
        value_mw = sum(value_mw, na.rm = TRUE),
        value_mwh = sum(value_mwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU",
        country = "EU total",
        source = "Wind",
        data_source = "entsoe_deweathered",
        region = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  message(sprintf("Weather correction complete for %d countries", length(countries)))
  return(corrected_data)
}


#' Get Weather-Corrected Solar Power Generation
#'
#' Corrects solar power generation for weather variations using solar radiation data.
#' Uses regression to remove the effect of solar radiation variations, similar to
#' temperature correction in get_corrected_demand.
#'
#' @param countries Character vector of ISO2 country codes. Use "EU" for all EU countries,
#'   or specify individual countries like c("DE", "FR"). Default is "EU".
#' @param date_from Start date for analysis (default: "2020-01-01")
#' @param diagnostics_folder Path to save diagnostic plots and outputs
#' @param use_cache Logical, whether to use cached data (default: TRUE)
#' @param use_local Logical, whether to use localhost API for weather data (default: FALSE)
#'
#' @return A data frame with weather-corrected solar generation matching the power
#'   generation format (columns: date, iso2, source, value_mw, value_mwh, etc.)
#'
#' @export
get_corrected_solar <- function(countries = "EU",
                                date_from = "2020-01-01",
                                diagnostics_folder = 'diagnostics/solar_correction',
                                use_cache = TRUE,
                                use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Handle EU case
  if ("EU" %in% countries) {
    countries <- get_eu_iso2s()
  }

  message("Getting solar power generation data...")
  # Get solar power generation from ENTSOE
  pwr_generation_all <- entsoe.get_power_generation(
    date_from = date_from,
    use_cache = use_cache
  )

  # Filter for solar generation in specified countries
  solar_generation <- pwr_generation_all %>%
    filter(iso2 %in% countries) %>%
    filter(grepl("solar", source, ignore.case = TRUE))

  message("Getting solar radiation data...")
  # Get solar radiation data
  weather_raw <- tryCatch({
    get_weather(
      variable = "solar_radiation",
      region_iso2 = paste(countries, collapse = ","),
      region_type = "country",
      date_from = date_from,
      use_cache = use_cache,
      use_local = use_local
    )
  }, error = function(e) {
    warning(sprintf("Failed to get weather data: %s", e$message))
    return(NULL)
  })

  if (is.null(weather_raw) || nrow(weather_raw) == 0) {
    stop("No weather data retrieved. Check API availability and parameters.")
  }

  # Ensure daily data
  stopifnot(all(lubridate::date(weather_raw$date) == weather_raw$date))

  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_results <- lapply(countries, function(country_iso2) {

    # Get solar generation for this country
    country_solar <- solar_generation %>%
      filter(iso2 == country_iso2)

    # Get weather data for this country and pivot to wide format
    country_weather <- weather_raw %>%
      filter(region_iso2 == country_iso2) %>%
      select(date, variable, value) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value)

    # Check if we have data
    if (nrow(country_solar) == 0 || nrow(country_weather) == 0) {
      warning(sprintf("No data for %s, skipping", country_iso2))
      return(NULL)
    }

    # Merge solar generation with weather data
    model_data <- country_solar %>%
      left_join(country_weather, by = "date") %>%
      filter(complete.cases(.)) %>%
      mutate(
        yday = lubridate::yday(date),
        yday_weather = lubridate::yday(date), # A weather variable that is basically yday. Separating it for clarity.
        year = lubridate::year(date)
      )

    if (nrow(model_data) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", country_iso2, nrow(model_data)))
      return(NULL)
    }

    # Add time vars
    model_data <- model_data %>%
      mutate(
        year_month = lubridate::floor_date(date, "month"),
        month = lubridate::month(date)
      )

    # # Get weather variable names dynamically (only numeric columns)
    # weather_vars <- setdiff(colnames(model_data),
    #                        c("date", "iso2", "value_mwh", "value_mw", "yday", "year",
    #                          "year_month", "month", "source", "country", "data_source",
    #                          "region", "frequency"))

    # if (length(weather_vars) == 0) {
    #   warning(sprintf("No weather variables found for %s", country_iso2))
    #   return(NULL)
    # }

    # message(sprintf("  Available weather variables: %s", paste(weather_vars, collapse = ", ")))

    # # Calculate climatology for weather variables
    # climatology <- model_data %>%
    #   group_by(yday) %>%
    #   summarise(across(all_of(weather_vars), ~mean(., na.rm = TRUE), .names = "{.col}_clim"),
    #             .groups = "drop")

    # # Add climatology to model data
    # model_data <- model_data %>%
    #   left_join(climatology, by = "yday")

    tryCatch({

      # Fit random forest model
      message(sprintf("  Fitting random forest for %s...", country_iso2))

      # For solar: use year for capacity growth + weather variables + yday for seasonality
      # Note: yday captures seasonality not fully explained by radiation alone, maybe due to:
      # - Day length variations (longer summer days = more generation hours, though it should be captured by radiation)
      # - Sun angle/elevation (higher summer sun = better panel efficiency)
      # - Possible temperature effects on panel efficiency
      formula_str <- paste("value_mwh ~ year + yday_weather + solar_radiation")
      model_formula <- as.formula(formula_str)
      message(sprintf("  Formula: %s", formula_str))

      model <- randomForest::randomForest(
        model_formula,
        data = model_data,
        ntree = 1000,
        importance = TRUE
      )

      # Create averaged data with climatological weather
      # Replace weather variables with their climatological averages
      # Keep year as is to preserve capacity growth trend
      model_data_averaged <- model_data %>%
        group_by(lubridate::yday(date)) %>%
        mutate_at(
          vars(solar_radiation),
          list(~ mean(., na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(yday_weather = 0)

      # Predict deweathered generation
      model_data$value_pred <- predict(model, model_data_averaged)

      # Plot comparison with EMBER using helper function
      plot_corrected_vs_ember(model_data, country_iso2, "solar", diagnostics_folder)

      # Return deweathered data
      model_data %>%
        select(date, iso2, value_mwh = value_pred) %>%
        mutate(
          value_mw = NA_real_,  # Will be calculated if needed
          source = "Solar",
          data_source = "entsoe_deweathered",
          country = countrycode::countrycode(iso2, "iso2c", "country.name"),
          region = "EU"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", country_iso2, e$message))
      return(NULL)
    })
  })

  # Combine results
  corrected_data <- bind_rows(corrected_results)

  # Add EU total
  if (length(countries) > 1) {
    eu_total <- corrected_data %>%
      group_by(date) %>%
      summarise(
        value_mw = sum(value_mw, na.rm = TRUE),
        value_mwh = sum(value_mwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU",
        country = "EU total",
        source = "Solar",
        data_source = "entsoe_weather_corrected",
        region = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  message(sprintf("Weather correction complete for %d countries", length(countries)))
  return(corrected_data)
}


#' Plot Corrected Generation vs EMBER Capacity
#'
#' Helper function to create faceted plots comparing weather-corrected generation
#' with EMBER capacity estimates at both yearly and monthly aggregations.
#'
#' @param model_data Data frame with corrected generation predictions
#' @param country_iso2 ISO2 country code
#' @param source_type Either "wind" or "solar"
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
    scale_y_continuous(labels = scales::comma) +
    rcrea::scale_y_crea_zero()

  quicksave(
    file.path(diagnostics_folder, paste0(source_type, "_corrected_vs_ember_", country_iso2, ".png")),
    plot = plt_vs_ember
  )
}
