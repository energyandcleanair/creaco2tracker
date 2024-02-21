
#' Get daily CO2 emissions from EUROSTAT, ENTSOG and ENTSOE data.
#'
#' @param diagnostics_folder
#' @param use_cache whether to use cache for EUROSTAT, power and gas data.
#' Save time for development. Should be Falsein production.
#' @param iso2s
#'
#' @return
#' @export
#'
#' @examples
get_co2_daily <- function(diagnostics_folder='diagnostics',
                          use_cache=F,
                          iso2s = c("EU")
                          ){

  # Collect necessary data
  gas_demand <- download_gas_demand(region_id=iso2s, use_cache = use_cache)
  pwr_demand <- download_pwr_demand(use_cache = use_cache)
  eurostat_cons <- get_eurostat_cons(diagnostics_folder = diagnostics_folder, use_cache = use_cache)

  # Quick sanity checks
  diagnostic_pwr(pwr_demand, diagnostics_folder = diagnostics_folder)
  diagnostic_eurostat_cons(eurostat_cons, iso2s=iso2s, diagnostics_folder = diagnostics_folder)

  # Compute emissions
  co2 <- get_co2_from_eurostat_cons(eurostat_cons)

  # Filter regions
  # Note: this need to be done after co2 estimates,
  # as all EU countries will be used to estimate weighted average NCVs
  co2 <- co2 %>%
    filter(iso2 %in% iso2s)

  # Project data til now
  co2_filled <- project_until_now(co2, pwr = pwr_demand, gas_demand=gas_demand)

  # Downscale to daily data
  co2_daily <- downscale_daily(co2 = co2_filled, pwr_demand = pwr_demand, gas_demand = gas_demand)

  # Resplit gas
  co2_daily <- split_gas_to_elec_others(co2_daily)

  # Add total
  co2_daily <- add_total_co2(co2_daily)

  # Diagnostics
  diagnostic_co2(co2_daily, diagnostics_folder=diagnostics_folder)

  # Formatting / cleaning for db
  co2_daily_formatted <- format_co2_for_db(co2_daily)

  return(co2_daily_formatted)
}
