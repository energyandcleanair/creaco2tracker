

#' Get Weather Correction Factors for Power Generation Mix
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
#' wc <- get_weather_correction_powermix(
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
get_weather_correction_powermix <- function(iso2s = "EU",
                                             date_from = "2015-01-01",
                                             date_to = Sys.Date(),
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
  message("Step 1/2: Getting weather-corrected renewable generation...")
  weather_corrected_renewables <- get_weather_corrected_renewables(
    iso2s = iso2s,
    date_from = date_from,
    pwr_generation = pwr_generation,
    use_cache = use_cache
  )

  if (nrow(weather_corrected_renewables) == 0) {
    warning("No renewable generation data available")
    return(tibble::tibble())
  }

  # Step 2: Calculate generation shares and correction factors
  message("Step 2/2: Calculating weather correction factors...")
  correction_factors <- calculate_powermix_correction_factors(
    weather_corrected_renewables = weather_corrected_renewables,
    pwr_generation = pwr_generation
  )

  message(sprintf("Weather correction complete. Generated %d correction factors.", nrow(correction_factors)))

  return(correction_factors)
}


#' Calculate Power Mix Correction Factor
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
#' calculate_powermix_correction_factor(thermal = 500000, renewable_delta = 30000)
#' # Returns: 0.94 (6% reduction in emissions)
#'
#' # Example 2: Bad weather year (less renewable generation)
#' calculate_powermix_correction_factor(thermal = 500000, renewable_delta = -20000)
#' # Returns: 1.04 (4% increase in emissions)
#'
#' @export
calculate_powermix_correction_factor <- function(thermal, renewable_delta) {
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
calculate_powermix_correction_factors <- function(
  weather_corrected_renewables,
  pwr_generation,
  diagnostics_folder = 'diagnostics/weather_correction') {


  # Aggregate renewable data by country, source, and year for diagnostics
  renewable_agg_by_source <- weather_corrected_renewables %>%
    select(iso2, source, date, value_mwh_corrected = value_mwh) %>%
    left_join(pwr_generation %>% select(iso2, source, date, value_mwh_actual = value_mwh),
              by = c("iso2", "source", "date")) %>%
    group_by(iso2, source, year = year(date)) %>%
    summarise(
      renewable_actual_mwh = sum(value_mwh_actual, na.rm = TRUE),
      renewable_weather_corrected_mwh = sum(value_mwh_corrected, na.rm = TRUE),
      renewable_delta_mwh = sum(value_mwh_corrected - value_mwh_actual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      correction_ratio = renewable_weather_corrected_mwh / renewable_actual_mwh,
      correction_pct = (correction_ratio - 1) * 100
    )

  # Create diagnostic plot showing renewable corrections by source
  plot_renewable_correction_by_source(renewable_agg_by_source, diagnostics_folder)

  # Aggregate renewable data by country and year (all sources combined)
  renewable_agg <- renewable_agg_by_source %>%
    group_by(iso2, year) %>%
    summarise(
      renewable_actual_mwh = sum(renewable_actual_mwh, na.rm = TRUE),
      renewable_weather_corrected_mwh = sum(renewable_weather_corrected_mwh, na.rm = TRUE),
      renewable_delta_mwh = sum(renewable_delta_mwh, na.rm = TRUE),
      .groups = "drop"
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
    mutate(emission_correction_factor = calculate_powermix_correction_factor(thermal, renewable_delta_mwh)) %>%
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


#' Plot Renewable Weather Correction by Source
#'
#' Creates a heatmap showing renewable generation weather correction ratios
#' by country, year, and source type (wind, solar, hydro).
#'
#' @param renewable_agg_by_source Data frame with columns: iso2, source, year,
#'   renewable_actual_mwh, renewable_weather_corrected_mwh, correction_pct
#' @param diagnostics_folder Path to save diagnostic plot
#'
#' @return ggplot object
#' @keywords internal
plot_renewable_correction_by_source <- function(renewable_agg_by_source, diagnostics_folder = 'diagnostics/weather_correction') {

  create_dir(diagnostics_folder)

  # Calculate average correction for ordering countries
  country_order <- renewable_agg_by_source %>%
    group_by(iso2) %>%
    summarise(avg_correction_pct = mean(correction_pct, na.rm = TRUE), .groups = "drop") %>%
    arrange(avg_correction_pct) %>%
    pull(iso2)

  # Create faceted heatmap by source
  plt <- renewable_agg_by_source %>%
    mutate(iso2 = factor(iso2, levels = country_order)) %>%
    ggplot(aes(x = factor(year), y = iso2)) +
    geom_tile(aes(fill = correction_pct), color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", correction_pct)), size = 2.5) +
    facet_wrap(~source, ncol = 1) +
    scale_fill_gradient2(
      low = "#D84315",    # Red = less renewable under avg weather
      mid = "white",
      high = "#2E7D32",   # Green = more renewable under avg weather
      midpoint = 0,
      name = "Weather\nCorrection (%)",
      limits = c(-30, 30),
      oob = scales::squish
    ) +
    labs(
      title = "Renewable Generation Weather Correction by Source",
      subtitle = "Percentage change: weather-corrected vs actual generation",
      x = "Year",
      y = "Country"
    ) +
    theme_crea_new() +
    theme(
      axis.text.x = element_text(angle = 0),
      panel.spacing = unit(1, "lines"),
      strip.text = element_text(face = "bold", size = 11)
    )

  quicksave(
    file.path(diagnostics_folder, 'renewable_correction_by_source.png'),
    plot = plt,
    width = 12,
    height = 18,
    preview = FALSE,
    logo = FALSE
  )

  return(plt)
}
