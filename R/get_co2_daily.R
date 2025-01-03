
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
                          iso2s = c("EU"),
                          min_year = NULL
                          ){

  # Collect necessary data
  gas_demand <- download_gas_demand(region_id=NULL, use_cache = use_cache)
  pwr_demand <- download_pwr_demand(use_cache = use_cache)
  eurostat_cons <- get_eurostat_cons(diagnostics_folder = diagnostics_folder,
                                     pwr_demand = pwr_demand,
                                     use_cache = use_cache)
  eurostat_indprod <- get_eurostat_indprod(use_cache = use_cache)

  # Quick sanity checks
  diagnostic_pwr(pwr_demand, diagnostics_folder = diagnostics_folder)
  diagnostic_eurostat_cons(eurostat_cons, iso2s=iso2s, diagnostics_folder = diagnostics_folder)

  # Compute emissions from EUROSTAT first
  co2 <- get_co2_from_eurostat_cons(eurostat_cons)

  # Project data til now
  # Need to be after filter since we're using all countries to fill missing EU data
  co2_filled <- project_until_now(co2, pwr_demand = pwr_demand, gas_demand=gas_demand, eurostat_indprod=eurostat_indprod)

  # Filter regions
  # Note: this need to be done after co2 estimates,
  # as all EU countries will be used to estimate weighted average NCVs
  co2_filled <- co2_filled %>%
    filter(iso2 %in% iso2s)


  # co2_filled %>%
  #   filter(iso2=="EU", fuel=="oil", date >= "2020-01-01") %>%
  #   ggplot() +
  #   geom_line(aes(date, value, color=estimate))

  # Downscale to daily data
  co2_daily <- downscale_daily(co2 = co2_filled, pwr_demand = pwr_demand, gas_demand = gas_demand)

  # Resplit gas
  co2_daily <- split_gas_to_elec_others(co2_daily)

  # Add total
  co2_daily <- add_total_co2(co2_daily)

  # Validation
  validate_co2(co2_daily, diagnostics_folder=diagnostics_folder, region=iso2s)

  # Fill missing
  co2_daily <- co2_daily %>%
    rename(region=geo) %>%
    mutate(unit='t/day') %>%
    filter(!is.na(date)) %>%
    {
      if(!is.null(min_year)){
        filter(., year(date) >= min_year)
      } else {
        .
      }
    }

  return(co2_daily)
}
