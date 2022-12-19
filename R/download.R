download_co2_daily <- function(){
  readr::read_csv('https://api.energyandcleanair.org/co2/emission?date_from=2015-01-01&format=csv') %>%
    select(region, date, fuel, sector, unit, frequency, value, version)
}