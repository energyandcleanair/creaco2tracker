select_if_exists <- function(data, ...) {
  if (is.null(data) || nrow(data)==0) {
    return(NULL)
  }

  dplyr::select(data, ...)
}



download_co2 <- function(date_from="2015-01-01", use_cache = F, refresh_cache = F, version=NULL, iso2s=NULL){

  creahelpers::api.get("api.energyandcleanair.org/emission/co2",
                       date_from=date_from,
                       use_cache = use_cache,
                       refresh_cache = refresh_cache,
                       cache_folder = "cache",
                       region = iso2s,
                       version = version) %>%
    select_if_exists(region, date, fuel, sector, unit, frequency, value, version)
}

download_gas_demand <- function(iso2=NULL,
                                use_cache=F,
                                refresh_cache=F,
                                date_from="2015-01-01"){

  params <- list(
    fuel='fossil_gas',
    data_source='crea',
    date_from=date_from,
    format='csv',
    region_id=iso2
  )

  # Remove null elements
  params <- purrr::compact(params)

  creahelpers::api.get("api.energyandcleanair.org/energy/demand",
                       params=params,
                       use_cache = use_cache,
                       refresh_cache = refresh_cache,
                       cache_folder = "cache") %>%
    select_if_exists(region_id, date, fuel, sector, unit, frequency, value) %>%
    rename(iso2=region_id)

}


download_corrected_demand <- function(region_id=NULL,
                                      sector='total',
                                      date_from="2015-01-01",
                                      use_cache = F,
                                      refresh_cache = F){

  params <- list(
    fuel='electricity_temperature_corrected,fossil_gas_temperature_corrected',
    sector=sector,
    date_from=date_from,
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
    select_if_exists(region_id, date, fuel, sector, unit, frequency, value)
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

  api_key <- Sys.getenv("API_KEY")
  if(api_key==""){
    stop("API_KEY not set")
  }

  params <- list(
    source='NATGAS',
    year=2020,
    format='csv',
    country=region_id,
    flow_raw='EFFELE',
    product_raw='NATGAS',
    unit='TJ',
    api_key=api_key
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
