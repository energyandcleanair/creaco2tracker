update_gas_demand <- function(production, diagnostics_folder){
  gas_demand <- get_gas_demand()
  upload_gas_demand(gas_demand, production=production)
}

update_co2_daily <- function(production, diagnostics_folder){
  co2_daily <- get_co2_daily(diagnostics_folder=diagnostics_folder)
  upload_co2_daily(co2_daily, production=production)
}

update_corrected_demand <- function(production, diagnostics_folder){
  corrected_demand <- get_corrected_demand()
  upload_corrected_demand(corrected_demand, production=production, clear_all_first=T)
}

update_all <- function(production=T, diagnostics_folder="diagnostics"){

  library(tidyverse)
  library(lubridate)
  library(tidytext)
  library(countrycode)
  library(plotly)
  library(glue)
  library(logger)
  library(creaco2tracker)
  library(magick)

  update_gas_demand(production=production, diagnostics_folder=diagnostics_folder)
  update_co2_daily(production=production, diagnostics_folder=diagnostics_folder)
  update_corrected_demand(production=production, diagnostics_folder=diagnostics_folder)

  check_recent_enough(days=15)
}
