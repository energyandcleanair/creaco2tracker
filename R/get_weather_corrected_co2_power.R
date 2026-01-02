#' Get Weather-Corrected CO2 Emissions from Power Sector
#'
#' Derives a weather correction factor for power sector CO2 emissions based on
#' weather-corrected renewable generation (wind and solar). The correction applies
#' the ratio of thermal generation share (actual vs weather-corrected) directly to
#' total power sector emissions.
#'
#' @param countries Character vector of ISO2 country codes or "EU" for EU aggregate
#' @param date_from Start date (YYYY-MM-DD)
#' @param date_to End date (YYYY-MM-DD), defaults to current date
#' @param aggregation Aggregation level: "yearly" or "ytd" (year-to-date)
#' @param sources Renewable sources to correct, default c("wind", "solar")
#' @param use_cache Logical, use cached data for weather corrections
#'
#' @return Data frame with columns:
#'   - iso2, country, region, year (or ytd_date)
#'   - co2_actual_t: Actual emissions from power sector (tonnes)
#'   - co2_weather_corrected_t: Weather-corrected emissions (tonnes)
#'   - correction_factor: Ratio (weather_corrected / actual)
#'   - correction_pct: Percentage change
#'   - thermal_share_actual_pct, thermal_share_weather_corrected_pct
#'   - renewable_actual_mwh, renewable_weather_corrected_mwh, renewable_delta_mwh
#'   - thermal_actual_mwh, thermal_weather_corrected_mwh
#'
#' @examples
#' \dontrun{
#' # Get weather-corrected CO2 for Germany in 2023
#' result <- get_weather_corrected_co2_power(
#'   countries = "DE",
#'   date_from = "2023-01-01",
#'   date_to = "2023-12-31",
#'   aggregation = "yearly"
#' )
#'
#' # Get YTD correction for all EU countries
#' result_ytd <- get_weather_corrected_co2_power(
#'   countries = "EU",
#'   date_from = "2020-01-01",
#'   aggregation = "ytd"
#' )
#' }
#'
#' @export
get_weather_corrected_co2_power <- function(countries = "EU",
                                             date_from = "2020-01-01",
                                             date_to = Sys.Date(),
                                             aggregation = c("yearly", "ytd"),
                                             sources = c("wind", "solar"),
                                             use_cache = TRUE) {

  aggregation <- match.arg(aggregation)

  message(sprintf("Getting weather-corrected CO2 for power sector: %s", paste(countries, collapse = ", ")))

  # Step 1: Get weather-corrected renewable generation
  message("Step 1/3: Getting weather-corrected renewable generation...")
  weather_corrected_renewables <- get_weather_corrected_renewables(
    iso2s = countries,
    date_from = date_from,
    pwr_generation = pwr_generation,
    use_cache = use_cache
  )

  # Step 2: Calculate generation shares
  message("Step 2/3: Calculating generation shares...")
  thermal_shares <- calculate_generation_shares(
    weather_corrected_renewables = weather_corrected_renewables,
    pwr_generation = pwr_generation
  )

  if (nrow(thermal_shares) == 0) {
    warning("No thermal share data calculated")
    return(data.frame())
  }

  # Step 4: Apply thermal share ratio to CO2 emissions
  message("Step 4/4: Applying weather correction to CO2 emissions...")
  corrected_co2 <- apply_weather_correction_to_co2(
    thermal_shares = thermal_shares,
    aggregation = aggregation
  )

  message(sprintf("Weather correction complete. Processed %d country-year combinations.", nrow(corrected_co2)))

  return(corrected_co2)
}



#' Get Total Generation Mix
#'
#' Retrieves power generation data for all sources and categorizes them
#' (renewable, thermal, nuclear, other).
#'
#' @inheritParams get_weather_corrected_co2_power
#' @return Data frame with generation by source category
#'
#' @keywords internal
get_total_generation_mix <- function(countries, date_from, date_to) {

  # Get all generation sources
  all_gen <- entsoe.get_power_generation(
    iso2s = countries,
    date_from = date_from,
    use_cache = TRUE
  )

  # Categorize sources
  generation_mix <- all_gen %>%
    dplyr::filter(source != "Total") %>%  # Exclude total to avoid double counting
    dplyr::mutate(
      source_category = dplyr::case_when(
        source %in% c("Wind", "Solar", "Hydro") ~ "renewable",
        source %in% c("Coal", "Fossil Gas", "Other Fossil") ~ "thermal",
        source == "Nuclear" ~ "nuclear",
        TRUE ~ "other"  # Bioenergy, etc.
      )
    )

  return(generation_mix)
}


#' Calculate Generation Shares
#'
#' Calculates thermal share of total generation in both actual and
#' weather-corrected scenarios.
#'
#' @param renewable_generation Output from get_weather_corrected_renewable_generation
#' @param generation_mix Output from get_total_generation_mix
#' @param aggregation "yearly" or "ytd"
#' @return Data frame with thermal shares and ratios
#'
#' @keywords internal
calculate_generation_shares <- function(renewable_generation,
                                         generation_mix,
                                         aggregation = c("yearly", "ytd")) {

  aggregation <- match.arg(aggregation)

  # Add year column to renewable data
  renewable_generation <- renewable_generation %>%
    dplyr::mutate(year = lubridate::year(date))

  generation_mix <- generation_mix %>%
    dplyr::mutate(year = lubridate::year(date))

  # For YTD: filter to same day-of-year across all years
  if (aggregation == "ytd") {
    max_yday <- max(lubridate::yday(renewable_generation$date))

    renewable_generation <- renewable_generation %>%
      dplyr::filter(lubridate::yday(date) <= max_yday)

    generation_mix <- generation_mix %>%
      dplyr::filter(lubridate::yday(date) <= max_yday)
  }

  # Aggregate renewable data by country and year
  renewable_agg <- renewable_generation %>%
    dplyr::group_by(iso2, country, region, year) %>%
    dplyr::summarise(
      renewable_actual_mwh = sum(value_mwh_actual, na.rm = TRUE),
      renewable_weather_corrected_mwh = sum(value_mwh_corrected, na.rm = TRUE),
      renewable_delta_mwh = sum(delta_mwh, na.rm = TRUE),
      .groups = "drop"
    )

  # Aggregate all generation by source category
  generation_agg <- generation_mix %>%
    dplyr::group_by(iso2, country, region, year, source_category) %>%
    dplyr::summarise(
      value_mwh = sum(value_mwh, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = source_category,
      values_from = value_mwh,
      values_fill = 0
    )

  # Calculate shares
  shares <- generation_agg %>%
    dplyr::left_join(renewable_agg, by = c("iso2", "country", "region", "year")) %>%
    dplyr::mutate(
      # Handle missing columns (some countries may not have all categories)
      renewable = dplyr::coalesce(renewable, 0),
      thermal = dplyr::coalesce(thermal, 0),
      nuclear = dplyr::coalesce(nuclear, 0),
      other = dplyr::coalesce(other, 0),

      # Total generation stays constant (demand unchanged assumption)
      total_generation_mwh = renewable + thermal + nuclear + other,

      # Weather-corrected thermal = actual thermal - additional renewables
      thermal_weather_corrected_mwh = thermal - renewable_delta_mwh,

      # Calculate shares
      thermal_share_actual = dplyr::if_else(
        total_generation_mwh > 0,
        thermal / total_generation_mwh,
        0
      ),
      thermal_share_weather_corrected = dplyr::if_else(
        total_generation_mwh > 0,
        thermal_weather_corrected_mwh / total_generation_mwh,
        0
      ),

      # Ratio to apply to emissions
      # If thermal_share_actual is 0 or very small, set ratio to 1 (no correction)
      thermal_share_ratio = dplyr::if_else(
        thermal_share_actual > 0.001,
        thermal_share_weather_corrected / thermal_share_actual,
        1.0
      )
    ) %>%
    dplyr::rename(
      thermal_actual_mwh = thermal
    )

  return(shares)
}


#' Apply Weather Correction to CO2 Emissions
#'
#' Applies the thermal share ratio directly to power sector CO2 emissions.
#'
#' @param thermal_shares Output from calculate_generation_shares
#' @param aggregation "yearly" or "ytd"
#' @return Data frame with weather-corrected CO2 emissions
#'
#' @keywords internal
apply_weather_correction_to_co2 <- function(thermal_shares,
                                            aggregation = c("yearly", "ytd")) {

  aggregation <- match.arg(aggregation)

  # Get actual emissions from power sector (total, all fuels)
  message("  - Fetching CO2 emissions data...")
  actual_co2 <- get_co2() %>%
    dplyr::filter(
      sector == "electricity",
      fuel == "total",
      estimate == "central"
    ) %>%
    dplyr::mutate(year = lubridate::year(date))

  # For YTD: filter to same day-of-year
  if (aggregation == "ytd") {
    max_date <- max(actual_co2$date)
    max_yday <- lubridate::yday(max_date)

    actual_co2 <- actual_co2 %>%
      dplyr::filter(lubridate::yday(date) <= max_yday)
  }

  # Aggregate by country and year
  actual_co2_agg <- actual_co2 %>%
    dplyr::group_by(iso2, country, region, year) %>%
    dplyr::summarise(
      co2_actual_t = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Apply thermal share ratio
  corrected_co2 <- actual_co2_agg %>%
    dplyr::left_join(
      thermal_shares,
      by = c("iso2", "country", "region", "year")
    ) %>%
    dplyr::mutate(
      # Direct application of ratio
      co2_weather_corrected_t = co2_actual_t * thermal_share_ratio,

      # Same as thermal_share_ratio
      correction_factor = thermal_share_ratio,

      # Percentage change
      correction_pct = 100 * (correction_factor - 1),

      # For reference/diagnostics (convert to percentages)
      thermal_share_actual_pct = 100 * thermal_share_actual,
      thermal_share_weather_corrected_pct = 100 * thermal_share_weather_corrected,

      # Metadata
      sector = "electricity",
      unit = "t",
      aggregation = aggregation
    ) %>%
    dplyr::select(
      iso2, country, region, year,
      co2_actual_t, co2_weather_corrected_t,
      correction_factor, correction_pct,
      thermal_share_actual_pct, thermal_share_weather_corrected_pct,
      renewable_actual_mwh, renewable_weather_corrected_mwh, renewable_delta_mwh,
      thermal_actual_mwh, thermal_weather_corrected_mwh,
      total_generation_mwh,
      sector, unit, aggregation
    )

  # Handle countries that failed (no thermal share data)
  # Set correction_factor = 1 (no correction) for these
  corrected_co2 <- corrected_co2 %>%
    dplyr::mutate(
      correction_factor = dplyr::if_else(
        is.na(correction_factor) | is.infinite(correction_factor),
        1.0,
        correction_factor
      ),
      co2_weather_corrected_t = dplyr::if_else(
        is.na(co2_weather_corrected_t),
        co2_actual_t,
        co2_weather_corrected_t
      ),
      correction_pct = 100 * (correction_factor - 1)
    )

  return(corrected_co2)
}
