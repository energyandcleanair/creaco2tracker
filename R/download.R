download_co2_daily <- function(){
  readr::read_csv('https://api.energyandcleanair.org/co2/emission?date_from=2015-01-01&format=csv') %>%
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