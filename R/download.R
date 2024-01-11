download_co2_daily <- function(date_from="2015-01-01"){
  readr::read_csv(glue('https://api.energyandcleanair.org/co2/emission?date_from={date_from}&format=csv')) %>%
    select(region, date, fuel, sector, unit, frequency, value, version)
}

download_gas_demand <- function(region_id=NULL){

  params <- list(
    fuel='fossil_gas',
    data_source='crea',
    date_from='2015-01-01',
    format='csv',
    region_id=region_id
  )

  # Remove null elements
  params <- purrr::compact(params)

  # Create URL params
  url_params <- paste(names(params), params, sep = "=", collapse = "&")

  readr::read_csv(sprintf('https://api.energyandcleanair.org/energy/demand?%s', url_params)) %>%
    select(region_id, date, fuel, sector, unit, frequency, value)
}


download_electricity <- function(region_id=NULL, data_source=NULL){

  params <- list(
    aggregate_by='country,source,date',
    data_source=data_source,
    date_from='2015-01-01',
    format='csv',
    country=paste(region_id, collapse=',')
  )

  # Remove null elements
  params <- purrr::compact(params)

  # Create URL params
  url_params <- paste(names(params), params, sep = "=", collapse = "&")


  readr::read_csv(sprintf('https://api.energyandcleanair.org/power/generation?%s', url_params)) %>%
    rename(region_id=iso2)
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

  params <- list(
    source='NATGAS',
    year=2020,
    format='csv',
    country=paste(region_id, collapse=','),
    flow_raw='EFFELE',
    product_raw='NATGAS',
    unit='TJ'
  )

  # Remove null elements
  params <- purrr::compact(params)

  # Create URL params
  url_params <- paste(names(params), params, sep = "=", collapse = "&")

  eff <- readr::read_csv(sprintf('https://api.energyandcleanair.org/energy/iea_balance?%s', url_params)) %>%
    mutate(value=value/100) %>%
    select(region_id=iso2, product_raw, flow_raw, unit, year, value)

  # Fill missing countries with median
  if(!is.null(region_id)){
    eff <- eff %>%
      tidyr::complete(region_id=unique(!!region_id), product_raw, flow_raw, unit, year,
                      fill=list(value=median(eff$value)))
  }
}
