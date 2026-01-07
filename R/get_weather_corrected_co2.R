#' Apply Weather Corrections to CO2 Emissions
#'
#' Applies weather correction factors to CO2 emissions data. Supports both
#' power generation mix corrections (renewable displacement) and demand
#' corrections (temperature effects). Automatically detects parameters from
#' the CO2 data.
#'
#' @param co2 CO2 emissions data frame (from get_co2())
#' @param apply_demand_correction Logical, apply demand correction (default TRUE)
#' @param apply_powermix_correction Logical, apply power generation mix correction (default TRUE)
#' @param sources Character vector of renewable sources to use in powermix correction.
#'   Default c("hydro", "solar", "wind"). Passed to get_weather_correction_powermix().
#' @param demand_fuels Character vector of demand types to correct. Default c("electricity", "fossil_gas").
#'   Valid values: "electricity", "fossil_gas". Passed to get_weather_correction_demand().
#' @param use_cache Logical, use cached data (default TRUE)
#' @param diagnostics_folder Optional folder for diagnostic plots
#'
#' @return A list containing:
#'   - \code{co2_weather_corrected}: CO2 data frame with weather corrections applied.
#'     Contains original columns plus:
#'     - \code{correction_factor_powermix}: Power mix correction ratio (if apply_powermix_correction=TRUE)
#'     - \code{correction_factor_demand}: Demand correction ratio (if apply_demand_correction=TRUE)
#'     - \code{correction_factor_total}: Combined correction (product of applicable factors)
#'     - \code{value}: Weather-corrected emissions (original value column is modified)
#'   - \code{weather_correction_powermix}: Power mix correction factors data frame (if apply_powermix_correction=TRUE)
#'   - \code{weather_correction_demand}: Demand correction factors data frame (if apply_demand_correction=TRUE)
#'
#'
#' @export
get_weather_corrected_co2 <- function(co2,
                                      apply_demand_correction = TRUE,
                                      apply_powermix_correction = TRUE,
                                      sources = c("hydro", "solar", "wind"),
                                      demand_fuels = c("electricity", "fossil_gas"),
                                      use_cache = TRUE,
                                      diagnostics_folder = "diagnostics/weather_correction") {


  # Derive parameters from co2 data
  iso2s <- unique(co2$iso2)
  date_from <- min(co2$date, na.rm = TRUE)
  date_to <- max(co2$date, na.rm = TRUE)

  # Initialize result with original data
  result <- list()
  result_co2 <- co2 %>%
    mutate(year = year(date))


  # Apply power mix correction
  if (apply_powermix_correction) {
    message("Getting power generation mix correction...")
    wc_powermix <- get_weather_correction_powermix(
      iso2s = iso2s,
      date_from = date_from,
      date_to = date_to,
      sources = sources,
      use_cache = use_cache,
      diagnostics_folder = diagnostics_folder
    )

    result[["weather_corrected_renewables"]] <- wc_powermix$weather_corrected_renewables
    result[["weather_correction_powermix"]] <- wc_powermix$weather_correction_factors

    wc_powermix <- wc_powermix$weather_correction_factors
    if (nrow(wc_powermix) > 0) {
      # Join on year and iso2 (powermix is yearly)
      result_co2 <- result_co2 %>%
        left_join(
          wc_powermix %>%
            select(iso2, year, sector,
                   correction_factor_powermix = emission_correction_factor),
          by = c("iso2", "year", "sector")
        ) %>%
        mutate(correction_factor_powermix = coalesce(correction_factor_powermix, 1.0))
    } else {
      warning("No powermix correction factors available, skipping")
      result_co2 <- result_co2 %>% mutate(correction_factor_powermix = 1.0)
    }
  } else {
    result_co2 <- result_co2 %>% mutate(correction_factor_powermix = 1.0)
  }

  # Apply demand correction
  if (apply_demand_correction) {

    message("Getting demand correction...")
    wc_demand <- get_weather_correction_demand(
      iso2s = iso2s,
      date_from = date_from,
      date_to = date_to,
      demand_fuels = demand_fuels,
      use_cache = use_cache,
      diagnostics_folder = diagnostics_folder
    )

    # Expand fuel total to all fuels
    fuels_total <- c(FUEL_COAL, FUEL_OIL, FUEL_GAS, FUEL_OIL)
    wc_demand <- wc_demand %>%
      full_join(tibble(fuel = FUEL_TOTAL, fuels = fuels_total), by = "fuel", relationship = "many-to-many") %>%
      mutate(fuel = coalesce(fuels, fuel)) %>%
      select(-fuels)

    result[["weather_correction_demand"]] <- wc_demand

    if (nrow(wc_demand) > 0) {
      # Join on date, iso2, fuel, sector (demand is daily)
      result_co2 <- result_co2 %>%
        left_join(
          wc_demand %>%
            select(iso2, date, fuel, sector, correction_factor_demand = correction_factor),
          by = c("iso2", "date", "fuel", "sector")
        ) %>%
        mutate(correction_factor_demand = coalesce(correction_factor_demand, 1.0))
    } else {
      warning("No demand correction factors available, skipping")
      result_co2 <- result_co2 %>% mutate(correction_factor_demand = 1.0)
    }
  } else {
    result_co2 <- result_co2 %>% mutate(correction_factor_demand = 1.0)
  }

  # Combine corrections
  message("Applying corrections")
  result_co2 <- result_co2 %>%
    mutate(
      correction_factor_total = correction_factor_powermix * correction_factor_demand,
      value = value * correction_factor_total
    )

  # Recompute total / all
  result_co2 <- result_co2 %>%
    filter(sector != SECTOR_ALL) %>%
    bind_rows(
      result_co2 %>%
        filter(sector != SECTOR_ALL) %>%
        group_by(iso2, date, unit, estimate, region, year) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(sector = SECTOR_ALL, fuel = FUEL_TOTAL)
    ) %>%
    ungroup()

  result[["co2_weather_corrected"]] <- result_co2

  return(result)
}
