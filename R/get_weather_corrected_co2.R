#' Apply Weather Corrections to CO2 Emissions
#'
#' Applies weather correction factors to CO2 emissions data. Supports both
#' power generation mix corrections (renewable displacement) and demand
#' corrections (temperature effects). Automatically detects parameters from
#' the CO2 data.
#'
#' @param co2 CO2 emissions data frame (from get_co2())
#' @param apply_powermix Logical, apply power generation mix correction (default TRUE)
#' @param apply_demand Logical, apply demand correction (default FALSE)
#' @param use_cache Logical, use cached data (default TRUE)
#' @param diagnostics_folder Optional folder for diagnostic plots
#'
#' @return CO2 data frame with additional columns:
#'   - correction_factor_powermix: Power mix correction ratio (if apply_powermix=TRUE)
#'   - correction_factor_demand: Demand correction ratio (if apply_demand=TRUE)
#'   - correction_factor_total: Combined correction (product of applicable factors)
#'   - value_weather_corrected: Weather-corrected emissions
#'   - Original columns from co2 are preserved
#'
#' @examples
#' \dontrun{
#' # Get CO2 data
#' co2 <- get_co2() %>%
#'   filter(sector == "electricity", fuel == "total")
#'
#' # Apply only power mix correction (default)
#' co2_corrected <- get_weather_corrected_co2(co2)
#'
#' # Apply both corrections
#' co2_corrected <- get_weather_corrected_co2(co2, apply_powermix = TRUE, apply_demand = TRUE)
#' }
#'
#' @export
get_weather_corrected_co2 <- function(co2,
                                      apply_demand_correction = TRUE,
                                      apply_powermix_correction = TRUE,
                                      use_cache = TRUE,
                                      diagnostics_folder = "diagnostics/weather_correction") {

  # Derive parameters from co2 data
  iso2s <- unique(co2$iso2)
  date_from <- min(co2$date, na.rm = TRUE)
  date_to <- max(co2$date, na.rm = TRUE)
  
  # Initialize result with original data
  result <- co2

  # Add year column if not present (needed for powermix join)
  if (!"year" %in% names(result) && "date" %in% names(result)) {
    result <- result %>%
      mutate(year = lubridate::year(date))
  }

  # Apply power mix correction
  if (apply_powermix_correction) {
    message("Applying power generation mix correction...")
    wc_powermix <- get_weather_correction_powermix(
      iso2s = iso2s,
      date_from = date_from,
      date_to = date_to,
      use_cache = use_cache,
      diagnostics_folder = diagnostics_folder
    )

    if (nrow(wc_powermix) > 0) {
      # Join on year and iso2 (powermix is yearly)
      result <- result %>%
        left_join(
          wc_powermix %>%
            select(iso2, year, sector,
                   correction_factor_powermix = emission_correction_factor),
          by = c("iso2", "year", "sector")
        ) %>%
        mutate(correction_factor_powermix = coalesce(correction_factor_powermix, 1.0))
    } else {
      warning("No powermix correction factors available, skipping")
      result <- result %>% mutate(correction_factor_powermix = 1.0)
    }
  } else {
    result <- result %>% mutate(correction_factor_powermix = 1.0)
  }

  # Apply demand correction
  if (apply_demand_correction) {

    message("Applying demand correction...")
    wc_demand <- get_weather_correction_demand(
      iso2s = iso2s,
      date_from = date_from,
      date_to = date_to,
      use_cache = use_cache,
      diagnostics_folder = diagnostics_folder
    )

    # Expand fuel total to all fuels
    fuels_total <- c(FUEL_COAL, FUEL_OIL, FUEL_GAS, FUEL_OIL)
    wc_demand <- wc_demand %>%
      full_join(tibble(fuel = FUEL_TOTAL, fuels = fuels_total), by = "fuel", relationship = "many-to-many") %>%
      mutate(fuel = coalesce(fuels, fuel)) %>%
      select(-fuels)

    if (nrow(wc_demand) > 0) {
      # Join on date, iso2, fuel, sector (demand is daily)
      result <- result %>%
        left_join(
          wc_demand %>%
            select(iso2, date, fuel, sector, correction_factor_demand = correction_factor),
          by = c("iso2", "date", "fuel", "sector")
        ) %>%
        mutate(correction_factor_demand = coalesce(correction_factor_demand, 1.0))
    } else {
      warning("No demand correction factors available, skipping")
      result <- result %>% mutate(correction_factor_demand = 1.0)
    }
  } else {
    result <- result %>% mutate(correction_factor_demand = 1.0)
  }

  # Combine corrections
  result <- result %>%
    mutate(
      correction_factor_total = correction_factor_powermix * correction_factor_demand,
      value = value * correction_factor_total
    )

  # Recompute total / all
  result <- result %>%
    filter(sector != SECTOR_ALL) %>%
    bind_rows(
      result %>%
        filter(sector != SECTOR_ALL) %>%
        group_by(iso2, date, unit, estimate, region, year) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(sector = SECTOR_ALL, fuel = FUEL_TOTAL)
    ) %>%
    ungroup()

  return(result)
}
