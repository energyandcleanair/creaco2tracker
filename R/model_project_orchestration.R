#' Project CO2 emissions until today using various proxies
#'
#' @param co2
#' @param pwr_generation
#' @param gas_demand
#' @param eurostat_indprod
#' @param fill_mode one of "overwrite", "missing", or "ratio". Default is "missing".
#'
#' @return
#' @export
#'
#' @examples
project_until_now <- function(
  co2_unprojected,
  pwr_generation,
  gas_demand,
  eurostat_indprod,
  fill_mode=c("missing", "overwrite", "ratio"),
  date_to=NULL){

  fill_mode <- match.arg(fill_mode) # Takes the first one by default i.e. missing

  dts_month <- seq.Date(min(co2_unprojected$date), if(is.null(date_to)) today() %>% 'day<-'(1) else as.Date(date_to), by='month')

  co2_unprojected %>%
    split_gas_to_elec_all() %>%
    project_until_now_elec(pwr_generation=pwr_generation, dts_month=dts_month, fill_mode=fill_mode) %>%
    project_until_now_gas(gas_demand=gas_demand, dts_month=dts_month, fill_mode=fill_mode) %>%
    project_eu_from_countries(dts_month=dts_month, fill_mode=fill_mode) %>%

    # We use industry for coal others
    project_until_now_coal_others(eurostat_indprod=eurostat_indprod, dts_month=dts_month, fill_mode=fill_mode) %>%

    # Then run projections
    project_until_now_forecast(dts_month=dts_month) %>%

    # And detotalise, since data from total and other sectors may now overlap again
    detotalise_co2()
}
