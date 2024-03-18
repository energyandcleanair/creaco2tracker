download_co2_daily <- function(date_from="2015-01-01", use_cache = F, refresh_cache = F, version=NULL, iso2s=NULL){
  creahelpers::api.get("api.energyandcleanair.org/emission/co2",
                       date_from=date_from,
                       use_cache = use_cache,
                       refresh_cache = refresh_cache,
                       cache_folder = "cache",
                       region = iso2s,
                       version = version) %>%
    select(region, date, fuel, sector, unit, frequency, value, version)
}

download_gas_demand <- function(region_id=NULL, use_cache = F, refresh_cache = F){

  params <- list(
    fuel='fossil_gas',
    data_source='crea',
    date_from='2015-01-01',
    format='csv',
    region_id=region_id
  )

  # Remove null elements
  params <- purrr::compact(params)

  creahelpers::api.get("api.energyandcleanair.org/energy/demand",
                       params=params,
                       use_cache = use_cache,
                       refresh_cache = refresh_cache,
                       cache_folder = "cache") %>%
    select(region_id, date, fuel, sector, unit, frequency, value)
}

download_pwr_demand <- function(date_from="2015-01-01", region=NULL, use_cache=T, refresh_cache=F) {

  pwr <- creahelpers::api.get('api.energyandcleanair.org/power/generation',
                              date_from=date_from,
                              aggregate_by='country,source,date',
                              region='EU',
                              split_by = 'year',
                              use_cache = use_cache,
                              refresh_cache = refresh_cache,
                              cache_folder = "cache")

  #add total generation
  pwr <- pwr %>%
    filter(source!='Total') %>%
    group_by(iso2, region, country, date) %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(source='Total') %>%
    bind_rows(pwr %>% filter(source!='Total'))

  #add EU total
  pwr <- pwr %>%
    filter(country!='EU total') %>%
    group_by(date, source) %>%
    filter(region=='EU') %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(country='EU total',
           iso2='EU') %>%
    bind_rows(pwr %>% filter(country!='EU total')) %>%
    ungroup()

  return(pwr)
}


download_corrected_demand <- function(region_id=NULL, sector='total', use_cache = F, refresh_cache = F){

  # https://http://localhost:8080/energy/demand?fuel=electricity_temperature_corrected&region_id=EU&format=csv&aggregate_by=year,date_without_year,fuel,sector,region_id&date_from=2019-01-01&pivot_by=year&rolling_days=7&pivot_fill_value=nan&rolling_fill_with_zero=False&sector=total&columns_order=date_without_year
  params <- list(
    fuel='electricity_temperature_corrected,fossil_gas_temperature_corrected',
    sector=sector,
    date_from='2015-01-01',
    format='csv',
    region_id=region_id
  )

  # Remove null elements
  params <- purrr::compact(params)

  creahelpers::api.get("api.energyandcleanair.org/energy/demand",
                       params=params,
                       use_cache = use_cache,
                       refresh_cache = refresh_cache,
                       cache_folder = "cache") %>%
    select(region_id, date, fuel, sector, unit, frequency, value)
}


#' Get power plant effiency directly from IEA data.
#'
#' Most likely reported on a LHV basis
#'
#' @return
#' @export
#'
#' @examples
download_thermal_efficiency <- function(region_id=NULL){

  if(!is.null(region_id)){
    region_id <- paste(region_id, collapse=',')
  }
  
  params <- list(
    source='NATGAS',
    year=2020,
    format='csv',
    country=region_id,
    flow_raw='EFFELE',
    product_raw='NATGAS',
    unit='TJ',
    api_key=Sys.getenv("API_KEY")
  )

  # Create URL params
  eff <-  creahelpers::api.get('api.energyandcleanair.org/energy/iea_balance', params=params) %>%
    mutate(value=value/100) %>%
    select(region_id=iso2, product_raw, flow_raw, unit, year, value)

  # Fill missing countries with median
  if(!is.null(region_id)){
    eff <- eff %>%
      tidyr::complete(region_id=unique(!!region_id), product_raw, flow_raw, unit, year,
                      fill=list(value=median(eff$value)))
  }

  return(eff)
}
