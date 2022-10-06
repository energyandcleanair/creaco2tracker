update_co2_daily <- function(production=T){
  co2_daily <- get_co2_daily()
  upload_co2_daily(co2_daily, production=production)
}
