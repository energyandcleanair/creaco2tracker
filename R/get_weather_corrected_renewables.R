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
                                             date_to=NULL,
                                             pwr_generation,
                                             sources = c("wind", "solar", "hydro"),
                                             use_cache = TRUE,
                                             diagnostics_folder = diagnostics_folder) {

  renewable_data <- list()

  # Get weather-corrected wind
  if ("wind" %in% tolower(sources)) {
    renewable_data[["wind"]] <- get_weather_corrected_wind(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      date_to = date_to,
      use_cache = use_cache,
      diagnostics_folder = file.path(diagnostics_folder, "wind")
    )
  }

  # Get weather-corrected solar
  if ("solar" %in% tolower(sources)) {
    renewable_data[["solar"]] <- get_weather_corrected_solar(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      date_to = date_to,
      use_cache = use_cache,
      diagnostics_folder = file.path(diagnostics_folder, "solar")
    )
  }

  # Get weather-corrected hydro
  if ("hydro" %in% tolower(sources)) {
    renewable_data[["hydro"]] <- get_weather_corrected_hydro(
      iso2s = iso2s,
      pwr_generation = pwr_generation,
      date_from = date_from,
      date_to = date_to,
      use_cache = use_cache,
      diagnostics_folder = file.path(diagnostics_folder, "hydro")
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
                                       date_from = "2015-01-01",
                                       date_to = NULL,
                                       diagnostics_folder = 'diagnostics/weather_correction/wind',
                                       use_cache = TRUE,
                                       use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date)==pwr_generation$date))

  # Filter for wind generation in specified countries
  wind_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("wind", source, ignore.case = TRUE))

  if(!is.null(date_to)) {
    wind_generation <- wind_generation %>%
      filter(date <= as.Date(date_to))
  }

  message("Getting wind speed and temperature data from CREA DB")
  # If EU, we'll use weather in all countries as predictors
  weather_iso2s <- if("EU" %in% iso2s) {
    get_eu_iso2s(include_eu = FALSE)
  } else {
    iso2s
  }

  # Get both wind speed and temperature in one call
  weather <- tryCatch({
    get_weather(
      variable = "ws50m_daily,t10m_daily",
      region_iso2 = paste(weather_iso2s, collapse = ","),
      region_type = "station",
      station_source = "gem",
      date_from = date_from,
      date_to = date_to,
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
  stopifnot(all(lubridate::date(weather$date) == weather$date))


  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_data <- pblapply(iso2s, function(iso2) {

    keep_weather_iso2s <- if(iso2=="EU"){
      get_eu_iso2s(include_eu = FALSE)
    } else {
      iso2
    }

    weather_country <- weather %>%
      filter(region_iso2 %in% keep_weather_iso2s)

    # Merge wind generation with weather data
    model_data <- wind_generation %>%
      filter(iso2 == !!iso2) %>%
      select(iso2, date, value_mwh) %>%
      left_join(weather_country %>%
                  select(weather_iso2=region_iso2, date, variable, value) %>%
                  tidyr::pivot_wider(names_from = variable, values_from = value),
                by = c("date"))


    if(nrow(model_data) == 0 || nrow(weather_country) == 0){
      warning(sprintf("No data for %s", iso2))
      return(NULL)
    }

    # Add time vars and physics-based features
    model_data <- model_data %>%
      mutate(
        yday = lubridate::yday(date),  # Day of year for climatology
        year = lubridate::year(date),
        year_month = lubridate::floor_date(date, "month"),
        month = lubridate::month(date)
      ) %>%
      mutate(
        ws_1 = ws50m_daily,
        ws_2 = ws50m_daily^2,
        ws_3 = ws50m_daily^3,
        inv_temp = 1 / (t10m_daily + 273.15),
        temp = (t10m_daily + 273.15)
      )

    # We spread variable_iso2
    model_data <- model_data %>%
      select(iso2, date, value_mwh, year, ws_1, ws_2, ws_3, inv_temp, temp, weather_iso2) %>%
      tidyr::pivot_wider(names_from = weather_iso2, values_from = c(ws_1, ws_2, ws_3, inv_temp, temp),
                         names_sep = "_")

    # Drop columns with only NAs (in case some countries don't have weather data for all iso2s)
    model_data <- model_data %>%
      select(where(~!all(is.na(.))))

    weather_vars <- grep(names(model_data), pattern = "^(ws_1|ws_2|ws_3|inv_temp|temp)_", value = TRUE)
    weather_iso2s <- unique(gsub("^(ws_1|ws_2|ws_3|inv_temp|temp)_", "", weather_vars))

    model_data_country <- model_data %>%
      filter(iso2 == !!iso2)

    # Check if we have data
    if (nrow(model_data_country) == 0) {
      warning(sprintf("No data for %s, skipping", iso2))
      return(NULL)
    }

    if (nrow(model_data_country) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", iso2, nrow(model_data)))
      return(NULL)
    }

    tryCatch({

      # Fit model using gbm
      formula_str <- paste("value_mwh ~ 0 + year +",
                           paste(glue("ws_1_{weather_iso2s}"), collapse = " + "), "+",
                           paste(glue("ws_2_{weather_iso2s}"), collapse = " + "), "+",
                           paste0(glue("ws_3_{weather_iso2s}*inv_temp_{weather_iso2s}"), collapse = " + ")
      )
      # formula_str <- paste("value_mwh ~ 0 + year  + ws_1 + ws_2 + ws_3*inv_temp")
      model_formula <- as.formula(formula_str)
      model <- gbm::gbm(
        model_formula,
        data = model_data_country,
        n.trees = 1000,
        interaction.depth = 3,
        shrinkage = 0.01,
        distribution = "gaussian"
      )

      # Predict at ACTUAL weather (fitted values)
      fitted_actual <- predict(model, model_data_country)

      # Predict at NORMAL weather
      # For each date, predict using all historical weather for that day-of-year
      # while keeping the actual year
      #
      # Attention: we should not predict at average weather for each yday,
      # because the curve is non-linear
      # -> the generation at average weather is not the same as average of generation at actual weather
      # see Jensen's inequality (E[f(X)] != f(E[X]) for non-linear f)
      crossed_weather <- model_data_country %>%
        mutate(yday = lubridate::yday(date)) %>%
        select_at(c("yday", weather_vars)) %>%
        full_join(
          model_data_country %>%
            mutate(yday = lubridate::yday(date)) %>%
            distinct(date, year, yday),
          by = "yday",
          relationship = "many-to-many"
        )

      crossed_weather$predicted_mwh <- predict(model, crossed_weather)

      fitted_normal <- crossed_weather %>%
        group_by(date) %>%
        summarise(fitted_normal = mean(predicted_mwh, na.rm = TRUE), .groups = "drop") %>%
        ungroup()

      model_data_country <- model_data_country %>%
        left_join(fitted_normal, by = c("date")) %>%
        mutate(value_mwh_corrected = value_mwh + (fitted_normal - fitted_actual))

      # Plot diagnostics
      if(!is.null(diagnostics_folder)){
        plot_wind_power_curve(model, model_data_country, iso2, diagnostics_folder)
        plot_corrected_vs_ember(model_data_country, iso2, "wind", diagnostics_folder)
      }

      # Return deweathered data with both actual and corrected values
      model_data_country %>%
        select(date, iso2, value_mwh_actual = value_mwh, value_mwh_corrected) %>%
        mutate(
          source = "Wind"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", iso2, e$message))
      return(NULL)
    })
  }) %>%
    bind_rows()


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
                                        date_to = NULL,
                                        diagnostics_folder = 'diagnostics/weather_correction/solar',
                                        use_cache = TRUE,
                                        use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date) == pwr_generation$date))

  # Filter for solar generation in specified countries
  solar_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("solar", source, ignore.case = TRUE))

  if(!is.null(date_to)) {
    solar_generation <- solar_generation %>%
      filter(date <= as.Date(date_to))
  }

  message("Getting solar radiation data from CREA DB")
  # If EU, we'll use weather in all countries as predictors
  weather_iso2s <- if("EU" %in% iso2s) {
    get_eu_iso2s(include_eu = FALSE)
  } else {
    iso2s
  }

  # Get solar radiation data
  weather <- tryCatch({
    get_weather(
      variable = "solar_radiation",
      region_iso2 = paste(weather_iso2s, collapse = ","),
      region_type = "country",
      date_from = date_from,
      use_cache = use_cache,
      use_local = use_local,
      date_to = date_to
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

  message("Fitting weather correction models...")
  # Fit model for each country
  corrected_data <- pblapply(iso2s, function(iso2) {

     keep_weather_iso2s <- if(iso2=="EU"){
      get_eu_iso2s(include_eu = FALSE)
    } else {
      iso2
    }

    # Merge solar generation with weather data
    model_data <- solar_generation %>%
      filter(iso2 == !!iso2) %>%
      select(iso2, date, value_mwh) %>%
      left_join(weather %>%
                  filter(region_iso2 %in% keep_weather_iso2s) %>%
                select(weather_iso2 = region_iso2, date, variable, value) %>%
                tidyr::pivot_wider(names_from = variable, values_from = value),
              by = c("date"))


    # We spread variable_iso2
    model_data <- model_data %>%
      select(iso2, weather_iso2, date, value_mwh, solar_radiation) %>%
      tidyr::pivot_wider(names_from = weather_iso2, values_from = solar_radiation,
                         names_prefix = "solar_radiation_") %>%
      mutate(year = year(date))

    # Drop columns with only NAs (in case some countries don't have weather data for all iso2s)
    model_data <- model_data %>%
      select(where(~!all(is.na(.))))

    weather_vars <- grep(names(model_data), pattern = "^(solar_radiation)_", value = TRUE)
    weather_iso2s <- unique(gsub("^(solar_radiation)_", "", weather_vars))

    model_data_country <- model_data %>%
      filter(iso2 == !!iso2)

    # Check if we have data
    if (nrow(model_data_country) == 0) {
      warning(sprintf("No data for %s, skipping", iso2))
      return(NULL)
    }

    if (nrow(model_data_country) < 100) {
      warning(sprintf("Insufficient data for %s (%d rows), skipping", iso2, nrow(model_data_country)))
      return(NULL)
    }

    tryCatch({

      # Fit model using gbm
      formula_str <- paste("value_mwh ~ 0 + year + ",
                          paste(weather_vars, collapse = " + "))
      model_formula <- as.formula(formula_str)
      model <- gbm::gbm(
        model_formula,
        data = model_data_country,
        n.trees = 1000,
        interaction.depth = 3,
        shrinkage = 0.01,
        distribution = "gaussian"
      )

      # Predict at ACTUAL weather (fitted values)
      fitted_actual <- predict(model, model_data_country)

      # Predict at NORMAL weather
      # For each date, predict using all historical weather for that day-of-year
      # while keeping the actual year
      #
      # Attention: we should not predict at average weather for each yday,
      # because the curve is potentially non-linear
      # -> the generation at average weather is not the same as average of generation at actual weather
      # see Jensen's inequality (E[f(X)] != f(E[X]) for non-linear f)
      crossed_weather <- model_data_country %>%
        mutate(yday = lubridate::yday(date)) %>%
        select(yday, weather_vars) %>%
        full_join(
          model_data_country %>%
            mutate(yday = lubridate::yday(date)) %>%
            distinct(date, year, yday),
          by = "yday",
          relationship = "many-to-many"
        )

      crossed_weather$predicted_mwh <- predict(model, crossed_weather)

      fitted_normal <- crossed_weather %>%
        group_by(date) %>%
        summarise(fitted_normal = mean(predicted_mwh, na.rm = TRUE), .groups = "drop") %>%
        ungroup()

      model_data_country <- model_data_country %>%
        left_join(fitted_normal, by = c("date")) %>%
        mutate(value_mwh_corrected = value_mwh + (fitted_normal - fitted_actual))

      # Plot comparison with EMBER using helper function
      plot_corrected_vs_ember(model_data_country, iso2, "solar", diagnostics_folder)

      # Return deweathered data with both actual and corrected values
      model_data_country %>%
        select(date, iso2, value_mwh_actual = value_mwh, value_mwh_corrected) %>%
        mutate(
          source = "Solar"
        )

    }, error = function(e) {
      warning(sprintf("Model fitting failed for %s: %s", iso2, e$message))
      return(NULL)
    })
  }) %>%
    bind_rows()

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
                                        date_to = NULL,
                                        diagnostics_folder = 'diagnostics/weather_correction/hydro',
                                        use_cache = TRUE,
                                        use_local = FALSE) {

  create_dir(diagnostics_folder)

  # Ensure this is daily data
  stopifnot(all(lubridate::date(pwr_generation$date) == pwr_generation$date))

  # Filter for hydro generation in specified countries
  hydro_generation <- pwr_generation %>%
    filter(iso2 %in% iso2s) %>%
    filter(grepl("hydro", source, ignore.case = TRUE))

  if(!is.null(date_to)) {
    hydro_generation <- hydro_generation %>%
      filter(date <= as.Date(date_to))
  }

  message("Getting ENTSOE capacity data")
  # Get ENTSOE installed capacity data for hydro
  capacity_raw <- tryCatch({
    entsoe.get_installed_capacity(iso2s = iso2s, date_from = date_from, date_to = date_to) %>%
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

  # We don't have much more than 10 years of data, so we use a simple average
  message("Calculating average capacity factors")

  # Merge generation with capacity
  cf <- hydro_generation %>%
    group_by(iso2, year = lubridate::year(date)) %>%
    summarise(value_mwh = mean(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    left_join(capacity, by = c("iso2", "year")) %>%
    mutate(cf = value_mwh / (capacity_mw * 24)) %>%
    group_by(iso2) %>%
    mutate(cf_avg = mean(cf, na.rm = TRUE)) %>%
    ungroup() %>%
    # Then derive a ratio to apply to generation time series
    mutate(ratio = cf_avg / cf) %>%
    select(iso2, year, ratio)

  # Apply ratio to generation time series, by year
  corrected_data <- hydro_generation %>%
    mutate(year = lubridate::year(date)) %>%
    left_join(cf, by = c("iso2", "year")) %>%
    mutate(
      value_mwh_actual = value_mwh,
      value_mwh_corrected = value_mwh * ratio
    ) %>%
    ungroup() %>%
    select(date, iso2, value_mwh_actual, value_mwh_corrected) %>%
    mutate(source = "Hydro")

  # Add EU total
  if (all(get_eu_iso2s() %in% corrected_data$iso2)) {
    eu_total <- corrected_data %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = FALSE)) %>%
      group_by(date, source) %>%
      summarise(
        value_mwh_actual = sum(value_mwh_actual, na.rm = TRUE),
        value_mwh_corrected = sum(value_mwh_corrected, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        iso2 = "EU"
      )

    corrected_data <- bind_rows(corrected_data, eu_total)
  }

  return(corrected_data)
}


#' Plot Wind Power Curve
#'
#' Creates a diagnostic plot showing the fitted wind power curve by year,
#' overlaid with actual observed generation data points.
#'
#' @param model Fitted random forest model
#' @param model_data Data frame with model training data
#' @param iso2 ISO2 country code
#' @param diagnostics_folder Path to save diagnostic plot
#'
#' @return NULL (plot is saved to file)
#' @keywords internal
plot_wind_power_curve <- function(model, model_data_country, iso2, diagnostics_folder) {

  # Create prediction grid for power curves
  weather_vars <- grep(names(model_data_country), pattern = "^(ws_1|ws_2|ws_3|inv_temp|temp)_", value = TRUE)
  weather_iso2s <- unique(gsub("^(ws_1|ws_2|ws_3|inv_temp|temp)_", "", weather_vars))
  ws_1_vars <- glue("ws_1_{weather_iso2s}")
  ws_1 = seq(0, max(model_data_country[,ws_1_vars], na.rm = TRUE), 0.1)
  temp_vars <- glue("temp_{weather_iso2s}")
  temp = model_data_country[,temp_vars] %>% as.matrix() %>% mean(na.rm=T)

  plot_curve_data <- tibble(
    ws_1 = ws_1,
    temp = temp,
    inv_temp = 1 / (temp)
  ) %>%
    tidyr::crossing(year = unique(model_data_country$year)) %>%
    mutate(
      ws_2 = ws_1^2,
      ws_3 = ws_1^3
    )

  for(iso2 in weather_iso2s){
    plot_curve_data[[paste0("ws_1_", iso2)]] <- plot_curve_data$ws_1
    plot_curve_data[[paste0("ws_2_", iso2)]] <- plot_curve_data$ws_1^2
    plot_curve_data[[paste0("ws_3_", iso2)]] <- plot_curve_data$ws_1^3
    plot_curve_data[[paste0("inv_temp_", iso2)]] <- plot_curve_data$inv_temp
    plot_curve_data[[paste0("temp_", iso2)]] <- temp
  }

  # Predict power for each wind speed and year
  plot_curve_data$pred <- predict(model, plot_curve_data)

  # Create plot
  plt_curve <-  ggplot() +
    geom_line(
      data=plot_curve_data,
      aes(ws_1, pred, color = as.factor(year)),
      linewidth = 1.5) +
    {
      # We can only add dots if single country
      if(length(ws_1_vars)==1){
        geom_point(
          data = model_data_country,
          aes_string(x = ws_1_vars, y = "value_mwh", color = "as.factor(year)"),
          alpha = 0.3,
          size = 0.8
        )
      }
    } +
    labs(
      title = paste("Wind Power Curve -", iso2),
      x = "Wind Speed at 50m (m/s)",
      y = "Generation (MWh/day)",
      color = "Year"
    ) +
    theme_crea_new()

  # Save plot
  dir.create(diagnostics_folder, showWarnings = FALSE, recursive = TRUE)
  quicksave(
    file.path(diagnostics_folder, paste0("wind_power_curve_", iso2, ".png")),
    plot = plt_curve,
    preview = FALSE,
    width = 10,
    height = 6,
    logo = FALSE
  )

  return(plt_curve)
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
#' @keywords internal
plot_corrected_vs_ember <- function(model_data_country, iso2, source_type, diagnostics_folder) {


  iso2s <- if(iso2=="EU"){
    get_eu_iso2s(include_eu = FALSE)
  } else {
    iso2
  }

  # Get EMBER capacity data once
  ember_capacity_raw <- tryCatch({
    ember.get_installed_capacity(iso2s = iso2s) %>%
      filter(grepl(source_type, source, ignore.case = TRUE)) %>%
      filter(date >= min(model_data_country$date))
  }, error = function(e) {
    warning(sprintf("Could not get EMBER data for %s: %s", iso2, e$message))
    tibble(date = as.Date(character()), value_mw = numeric())
  })

  entsoe_capacity_raw <- tryCatch({
    entsoe.get_installed_capacity(iso2s = iso2s, date_from = min(model_data_country$date)) %>%
      filter(grepl(source_type, source, ignore.case = TRUE))
  }, error = function(e) {
    warning(sprintf("Could not get ENTSOE data for %s: %s", iso2, e$message))
    tibble(date = as.Date(character()), value_mw = numeric())
  })

  # Aggregate to yearly
  ember_capacity_yearly <- ember_capacity_raw %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  entsoe_capacity_yearly <- entsoe_capacity_raw %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  # Aggregate to monthly
  ember_capacity_monthly <- ember_capacity_raw %>%
    mutate(year_month = lubridate::floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  entsoe_capacity_monthly <- entsoe_capacity_raw %>%
    mutate(year_month = lubridate::floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(capacity_mw = mean(value_mw, na.rm = TRUE), .groups = "drop")

  # Prepare plot data for yearly comparison
  plot_data_yearly <- bind_rows(
    model_data_country %>%
      group_by(year) %>%
      summarise(value = mean(value_mwh_corrected, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Weather-corrected generation"),
    model_data_country %>%
      group_by(year) %>%
      summarise(value = mean(value_mwh, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Actual generation"),
    ember_capacity_yearly %>%
      select(year, value = capacity_mw) %>%
      mutate(source = "Capacity (EMBER)"),
    entsoe_capacity_yearly %>%
      select(year, value = capacity_mw) %>%
      mutate(source = "Capacity (ENTSOE)")
  ) %>%
    group_by(source) %>%
    mutate(value = if(n() > 0 && !all(is.na(value))) value / value[year == min(year)] * 100 else NA_real_) %>%
    ungroup() %>%
    mutate(time_scale = "Yearly",
           date = as.Date(paste0(year, "-01-01")))

  # Prepare plot data for monthly comparison
  plot_data_monthly <- bind_rows(
    model_data_country %>%
      group_by(year_month=floor_date(date, "month")) %>%
      summarise(value = mean(value_mwh_corrected, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Weather-corrected generation"),
    model_data_country %>%
      group_by(year_month=floor_date(date, "month")) %>%
      summarise(value = mean(value_mwh, na.rm = TRUE), .groups = "drop") %>%
      mutate(source = "Actual generation"),
    ember_capacity_monthly %>%
      select(year_month, value = capacity_mw) %>%
      mutate(source = "Capacity (EMBER)"),
    entsoe_capacity_monthly %>%
      select(year_month, value = capacity_mw) %>%
      mutate(source = "Capacity (ENTSOE)")
  ) %>%
    group_by(source) %>%
    mutate(value = if(n() > 0 && !all(is.na(value))) value / value[year_month == min(year_month)] * 100 else NA_real_) %>%
    ungroup() %>%
    mutate(time_scale = "Monthly",
           date = year_month)

  colors <- c(
    "Actual generation" = rcrea::pal_crea[["Light.gray"]],
    "Weather-corrected generation" = rcrea::pal_crea[["Dark.blue"]],
    "Capacity (EMBER)" = rcrea::pal_crea[["Green"]],
    "Capacity (ENTSOE)" = rcrea::pal_crea[["Orange"]]
  )

  # Combine and plot both
  plt_vs_ember <- bind_rows(plot_data_yearly, plot_data_monthly) %>%
    ggplot(aes(date, value, col = factor(source, levels = names(colors)))) +
    geom_line(linewidth = 1) +
    geom_point(size = 1) +
    facet_wrap(~time_scale, scales = "free_y", ncol = 1) +
    labs(
      title = paste("Weather-corrected", source_type, "generation vs Ember estimates -", iso2),
      y = "Index 100 (base period)",
      x = "",
      col = NULL
    ) +
    scale_color_manual(values = colors) +
    theme_crea_new() +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", date_labels = "%Y") +
    rcrea::scale_y_crea_zero()

  dir.create(diagnostics_folder, showWarnings = FALSE, recursive = TRUE)
  quicksave(
    file.path(diagnostics_folder, paste0(source_type, "_corrected_vs_ember_", iso2, ".png")),
    plot = plt_vs_ember,
    preview = FALSE,
    width = 12,
    height = 8,
    logo = FALSE
  )
}
