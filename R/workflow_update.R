update_gas_demand <- function(production, diagnostics_folder, data_masking = NULL) {
  gas_demand <- get_gas_demand(data_masking = data_masking)
  upload_gas_demand(gas_demand, production = production)
}

update_co2_daily <- function(production, diagnostics_folder, data_masking = NULL) {
  co2_daily <- get_co2(
    diagnostics_folder = diagnostics_folder, downscale_daily = TRUE, iso2s =
      "EU", data_masking = data_masking
  )
  upload_co2_daily(co2_daily, production = production)
}

update_corrected_demand <- function(production, diagnostics_folder, data_masking = NULL) {
  corrected_demand <- get_corrected_demand(data_masking = data_masking)
  upload_corrected_demand(corrected_demand, production = production, clear_all_first = TRUE)
}

update_all <- function(production = TRUE, diagnostics_folder = "diagnostics", data_masking = NULL) {
  library(tidyverse)
  library(lubridate)
  library(tidytext)
  library(countrycode)
  library(plotly)
  library(glue)
  library(logger)
  library(creaco2tracker)
  library(magick)
  library(magrittr)

  update_gas_demand(
    production = production, diagnostics_folder = diagnostics_folder,
    data_masking = data_masking
  )
  update_co2_daily(
    production = production, diagnostics_folder = diagnostics_folder, data_masking =
      data_masking
  )
  update_corrected_demand(
    production = production, diagnostics_folder = diagnostics_folder,
    data_masking = data_masking
  )

  check_recent_enough(days = 15)
}
