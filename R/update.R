update_co2_daily <- function(production=T){
  co2_daily <- get_co2_daily()
  upload_co2_daily(co2_daily, production=production)
}

update_corrected_demand <- function(production=T){
  corrected_demand <- get_corrected_demand()
  upload_corrected_demand(corrected_demand, production=production)
}
