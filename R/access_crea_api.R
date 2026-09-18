#' Fetch published CO2 data from the CREA API
#' @keywords internal
crea_data_access_get_co2 <- function(...) {
  creahelpers::api.get("api.energyandcleanair.org/emission/co2", ...)
}


#' Fetch published demand data from the CREA API
#' @keywords internal
crea_data_access_get_demand <- function(params, ...) {
  creahelpers::api.get(
    "api.energyandcleanair.org/energy/demand",
    params = params,
    ...
  )
}


#' Fetch IEA balance data from the CREA API
#' @keywords internal
crea_data_access_get_iea_balance <- function(params, ...) {
  creahelpers::api.get(
    "api.energyandcleanair.org/energy/iea_balance",
    params = params,
    ...
  )
}
