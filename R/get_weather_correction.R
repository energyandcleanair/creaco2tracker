#' Apply Weather Correction to CO2 Emissions
#'
#' Applies weather correction factors to CO2 emissions data. This is a convenience
#' wrapper that joins weather correction factors with CO2 data and calculates
#' weather-corrected emissions.
#'
#' @param co2 CO2 emissions data frame (from get_co2())
#' @param weather_correction Weather correction factors (from get_weather_correction()).
#'   If NULL, will be calculated automatically using default parameters.
#' @param ... Additional arguments passed to get_weather_correction() if weather_correction is NULL
#'
#' @return CO2 data frame with additional columns:
#'   - correction_factor: Weather correction ratio
#'   - value_weather_corrected: Weather-corrected emissions
#'   - Original columns from co2 are preserved
#'
#' @examples
#' \dontrun{
#' # Get CO2 data
#' co2 <- get_co2() %>%
#'   filter(sector == "electricity", fuel == "total")
#'
#' # Apply weather correction
#' co2_corrected <- get_weather_corrected_co2(co2)
#'
#' # Or with custom correction factors
#' wc <- get_weather_correction(countries = "DE", aggregation = "yearly")
#' co2_de <- get_co2() %>% filter(iso2 == "DE", sector == "electricity")
#' co2_de_corrected <- get_weather_corrected_co2(co2_de, wc)
#' }
#'
#' @export
get_weather_corrected_co2 <- function(co2,
                                       weather_correction = NULL,
                                       ...) {

  # If no weather correction provided, calculate it
  if (is.null(weather_correction)) {
    message("No weather correction factors provided, calculating with default parameters...")
    weather_correction <- get_weather_correction(...)
  }

  if (nrow(weather_correction) == 0) {
    warning("No weather correction factors available, returning original CO2 data")
    return(co2 %>% dplyr::mutate(correction_factor = 1, value_weather_corrected = value))
  }

  # Filter weather correction to electricity sector, total fuel
  wc_filtered <- weather_correction %>%
    dplyr::filter(sector == "electricity", fuel == "total")

  # Determine join keys based on what's available in both datasets
  possible_join_keys <- c("iso2", "year", "sector", "fuel")
  join_keys <- intersect(possible_join_keys, intersect(names(co2), names(wc_filtered)))

  # If no common keys, try just iso2 and year
  if (length(join_keys) == 0) {
    join_keys <- intersect(c("iso2", "year"), intersect(names(co2), names(wc_filtered)))
  }

  # Select only needed columns from wc for joining
  wc_for_join <- wc_filtered %>%
    dplyr::select(dplyr::any_of(c(
      join_keys,
      "correction_factor", "correction_pct",
      "thermal_share_actual_pct", "thermal_share_weather_corrected_pct",
      "renewable_delta_mwh"
    )))

  # Join and apply correction
  co2_corrected <- co2 %>%
    dplyr::left_join(wc_for_join, by = join_keys) %>%
    dplyr::mutate(
  # Default to no correction if missing
      correction_factor = dplyr::coalesce(correction_factor, 1.0),

  # Apply correction to value
      value_weather_corrected = value * correction_factor
    )

  return(co2_corrected)
}


#' Get Weather Correction Factors for Power Sector
#'
#' Calculates weather correction factors based on weather-corrected renewable
#' generation (wind and solar). Returns a table of correction factors that can
#' be joined with CO2 emissions data.
#'
#' The correction factor represents the ratio of thermal generation share under
#' climatological (average) weather conditions vs actual weather. This ratio
#' directly translates to the emission correction ratio since all power sector
#' CO2 comes from thermal sources.
#'
#' @param iso2s Character vector of ISO2 country codes or "EU" for EU aggregate
#' @param date_from Start date (YYYY-MM-DD)
#' @param date_to End date (YYYY-MM-DD), defaults to current date
#' @param aggregation Aggregation level: "yearly" or "ytd" (year-to-date)
#' @param sources Renewable sources to correct, default c("wind", "solar")
#' @param use_cache Logical, use cached data for weather corrections
#'
#' @return Tibble with correction factors:
#'   - iso2, country, region, year (or ytd_date)
#'   - sector: "electricity"
#'   - fuel: "total"
#'   - correction_factor: Ratio to apply to emissions (weather_corrected / actual)
#'   - correction_pct: Percentage change
#'   - thermal_share_actual_pct, thermal_share_weather_corrected_pct
#'   - renewable_delta_mwh: Additional renewable generation under avg weather
#'
#' @examples
#' \dontrun{
#' # Get weather correction factors for 2024
#' wc <- get_weather_correction(
#'   iso2s = "EU",
#'   date_from = "2024-01-01",
#'   date_to = "2024-12-31",
#'   aggregation = "yearly"
#' )
#'
#' # Apply to CO2 data
#' co2 <- get_co2()
#' co2_corrected <- get_weather_corrected_co2(co2, wc)
#' }
#'
#' @export
get_weather_correction <- function(iso2s = "EU",
                                    date_from = "2020-01-01",
                                    date_to = Sys.Date(),
                                    sources = c("wind", "solar"),
                                    use_cache = TRUE,
                                    diagnostics_folder = 'diagnostics/weather_correction') {


  message(sprintf("Calculating weather correction factors: %s", paste(iso2s, collapse = ", ")))

  # Get power generation once
  message("Getting power generation data...")
  pwr_generation <- entsoe.get_power_generation(
    iso2s = iso2s,
    date_from = date_from,
    use_cache = use_cache
  )

  # Step 1: Get weather-corrected renewable generation
  message("Step 1/3: Getting weather-corrected renewable generation...")
  weather_corrected_renewables <- get_weather_corrected_renewables(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    sources = sources,
    pwr_generation = pwr_generation,
    use_cache = use_cache
  )

  if (nrow(renewable_gen) == 0) {
    warning("No renewable generation data available")
    return(tibble::tibble())
  }

  # Step 3: Calculate generation shares and correction factors
  message("Step 3/3: Calculating weather correction factors...")
  correction_factors <- calculate_weather_correction_factors(
    weather_corrected_renewables = weather_corrected_renewables
  )

  message(sprintf("Weather correction complete. Generated %d correction factors.", nrow(correction_factors)))

  return(correction_factors)
}


#' Get Weather-Corrected Renewable Generation
#'
#' Retrieves weather-corrected generation for renewable sources (wind, solar)
#' and compares with actual generation to calculate the delta.
#'
#' @inheritParams get_weather_correction
#' @param pwr_generation Power generation data from entsoe.get_power_generation
#' @return Data frame with actual and weather-corrected renewable generation
#'
#' @keywords internal
get_weather_corrected_renewables <- function(iso2s,
                                             date_from,
                                             date_to,
                                             sources = c("wind", "solar"),
                                             pwr_generation,
                                             use_cache = TRUE) {

  renewable_data <- list()

  # Get weather-corrected wind
  if ("wind" %in% tolower(sources)) {
    tryCatch({
      wind_corrected <- get_weather_corrected_wind(
        iso2s = iso2s,
        pwr_generation = pwr_generation,
        date_from = date_from,
        use_cache = use_cache
      )

      # Filter to date_to if needed
      if (!is.null(date_to)) {
        wind_corrected <- wind_corrected %>%
          dplyr::filter(date <= as.Date(date_to))
      }

      renewable_data[["wind"]] <- wind_corrected
    }, error = function(e) {
      warning(sprintf("Failed to get wind data: %s", e$message))
    })
  }

  # Get weather-corrected solar
  if ("solar" %in% tolower(sources)) {
    tryCatch({
      solar_corrected <- get_weather_corrected_solar(
        iso2s = iso2s,
        pwr_generation = pwr_generation,
        date_from = date_from,
        use_cache = use_cache
      )

      # Filter to date_to if needed
      if (!is.null(date_to)) {
        solar_corrected <- solar_corrected %>%
          dplyr::filter(date <= as.Date(date_to))
      }

      renewable_data[["solar"]] <- solar_corrected
    }, error = function(e) {
      warning(sprintf("Failed to get solar data: %s", e$message))
    })
  }

  # Combine all renewable sources
  if (length(renewable_data) == 0) {
    return(tibble())
  }

  result <- dplyr::bind_rows(renewable_data)

  return(result)
}


#' Calculate Emission Correction Factor
#'
#' Computes the emission correction factor based on actual thermal generation
#' and the change in renewable generation (delta). The correction factor represents
#' the ratio of weather-corrected thermal to actual thermal generation.
#'
#' The function implements two key constraints:
#' 1. Non-negativity: Ensures thermal generation doesn't go negative
#' 2. Historical cap: Limits weather-corrected thermal to historical maximum
#'
#' @param thermal Numeric vector of actual thermal generation (MWh)
#' @param renewable_delta Numeric vector of change in renewable generation (MWh).
#'   Positive values indicate more renewable generation under average weather.
#'
#' @return Numeric vector of emission correction factors (0 to ~1+)
#'   - < 1: Good weather for renewables → less thermal needed → lower emissions
#'   - > 1: Bad weather for renewables → more thermal needed → higher emissions
#'   - = 1: No weather correction needed
#'
#' @details
#' Formula: correction_factor = max(0, min(max(thermal), thermal - renewable_delta)) / thermal
#'
#' The formula ensures:
#' - Weather-corrected thermal = thermal - renewable_delta (100% displacement assumption)
#' - Capped at historical max to prevent unrealistic values in low-thermal countries
#' - Floored at 0 to prevent negative generation
#'
#' @examples
#' # Example 1: Good weather year (more renewable generation)
#' calculate_emission_correction_factor(thermal = 500000, renewable_delta = 30000)
#' # Returns: 0.94 (6% reduction in emissions)
#'
#' # Example 2: Bad weather year (less renewable generation)
#' calculate_emission_correction_factor(thermal = 500000, renewable_delta = -20000)
#' # Returns: 1.04 (4% increase in emissions)
#'
#' @export
calculate_emission_correction_factor <- function(thermal, renewable_delta) {
  # For each country (group), cap weather-corrected thermal at historical max
  # This prevents unrealistic correction factors in countries with little thermal
  thermal_weather_corrected <- pmin(
    max(thermal, na.rm = TRUE),  # Historical max thermal for this country
    thermal - renewable_delta     # Weather-corrected thermal generation
  )

  # Ensure non-negative and compute ratio
  correction_factor <- pmax(0, thermal_weather_corrected) / thermal

  return(correction_factor)
}


#' Calculate Weather Correction Factors
#'
#' Calculates weather correction factors based on thermal generation share
#' in actual vs weather-corrected scenarios.
#'
#' @param renewable_generation Output from get_weather_corrected_renewable_generation
#' @param generation_mix Output from get_total_generation_mix
#' @param aggregation "yearly" or "ytd"
#' @return Tibble with weather correction factors
#'
#' @keywords internal
calculate_weather_correction_factors <- function(
  weather_corrected_renewables,
  pwr_generation,
  diagnostics_folder = 'diagnostics/weather_correction') {


  # Aggregate renewable data by country and year
  renewable_agg <- weather_corrected_renewables %>%
    select(iso2, source, date, value_mwh_corrected = value_mwh) %>%
    left_join(pwr_generation %>% select(iso2, source, date, value_mwh_actual = value_mwh),
              by = c("iso2", "source", "date")) %>%
    # Derive a delta mwh for all renewable together
    group_by(iso2, year = year(date)) %>%
      summarise(
        renewable_actual_mwh = sum(value_mwh_actual, na.rm = TRUE),
        renewable_weather_corrected_mwh = sum(value_mwh_corrected, na.rm = TRUE),
        renewable_delta_mwh = sum(value_mwh_corrected - value_mwh_actual, na.rm = TRUE)
      )

  stopifnot("Delta doesn't sum to zero" = abs(sum(renewable_agg$renewable_delta_mwh)) < 1e-3)

  # Then 1MWh additional renewable is considered to be 1MWh less thermal
  # And we keep the same thermal generation mix
  add_source_category <- function(source) {
    x = case_when(
      source %in% c("Wind", "Solar", "Hydro") ~ "renewable",
      source %in% c("Coal", "Fossil Gas") ~ "thermal",
      source == "Nuclear" ~ "nuclear",
      source %in% c("Storage", "Other") ~ "other",
      source == "Total" ~ "Total",
      TRUE ~ NA_character_
    )
    stopifnot("Source category not found" = !is.na(x))
    return(x)
  }

  thermal_agg <- pwr_generation %>%
    # Only keep dates that are in the weather_corrected_renewables
    filter(date %in% weather_corrected_renewables$date) %>%
    select(iso2, date, source, value_mwh) %>%
    mutate(source_category = add_source_category(source)) %>%
    group_by(iso2, source_category, year = year(date)) %>%
    summarise(value_mwh = sum(value_mwh, na.rm = TRUE)) %>%
    ungroup()

  correction_factors <- thermal_agg %>%
    pivot_wider(names_from = source_category, values_from = value_mwh, values_fill = 0) %>%
    left_join(renewable_agg %>% select(iso2, year, renewable_delta_mwh), by = c("iso2", "year")) %>%
    group_by(iso2) %>%
    mutate(emission_correction_factor = calculate_emission_correction_factor(thermal, renewable_delta_mwh)) %>%
    mutate(sector = SECTOR_ELEC) %>%
    select(iso2, year, sector, emission_correction_factor, thermal)

  # Add EU total as emission-weighted average of member states
  # Assumes no grid interconnectivity - each country's correction weighted by its thermal generation
  eu_iso2s <- get_eu_iso2s(include_eu = FALSE)

  if (all(setdiff(eu_iso2s, c("CY", "MT")) %in% correction_factors$iso2)) {
    eu_correction <- correction_factors %>%
      filter(iso2 %in% eu_iso2s) %>%
      group_by(year, sector) %>%
      summarise(
        # Emission-weighted average: weight = thermal_i / sum(thermal)
        emission_correction_factor = sum(emission_correction_factor * thermal) / sum(thermal),
        .groups = "drop"
      ) %>%
      mutate(iso2 = "EU")

    correction_factors <- bind_rows(
      correction_factors %>% select(-thermal),
      eu_correction
    )
  } else {
    correction_factors <- correction_factors %>% select(-thermal)
  }

  # Plot the correction factors
  plot_weather_correction_factors(correction_factors, diagnostics_folder)

  return(correction_factors)
}

plot_weather_correction_factors <- function(correction_factors, diagnostics_folder = 'diagnostics/weather_correction') {
  
  create_dir(diagnostics_folder)
  limit <- max(abs(correction_factors$emission_correction_factor))

  plt <- ggplot(correction_factors, aes(x=year, y=iso2)) +
    geom_tile(aes(fill = emission_correction_factor) ) +
    facet_wrap(~sector) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 1) +
    labs(title = "Weather Correction Factors",
         x = "Year",
         y = "Country") +
    theme_crea_new()

  quicksave(file.path(diagnostics_folder, 'weather_correction_factors.png'),
            plot = plt, width = 10, height = 10, preview = FALSE, logo = FALSE)

  return(plt)
}
