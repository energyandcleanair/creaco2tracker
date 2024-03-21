check_recent_enough <- function(days=10){
  
  date_from <- lubridate::today() - lubridate::days(days)
  
  gas_demand <- download_gas_demand(region_id="EU", date_from=date_from)
  if(is.null(gas_demand) || nrow(gas_demand)==0) stop("Missing recent gas demand")
  
  co2 <- download_co2_daily(date_from=date_from, iso2s="EU")
  if(is.null(co2) || nrow(co2)==0) stop("Missing recent CO2")
  
  corrected <- download_corrected_demand(date_from=date_from, region_id="EU")
  if(is.null(corrected) || nrow(corrected)==0) stop("Missing recent corrected gas demand")
  
}