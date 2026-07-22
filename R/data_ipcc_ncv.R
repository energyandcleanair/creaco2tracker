#' Get IPCC net calorific values
#'
#' Returns the IPCC net calorific values used by the CO2 pipeline, mapped to
#' Eurostat Standard International Energy Product Classification (SIEC) codes.
#' Values are expressed in kilojoules per kilogram (kJ/kg).
#'
#' The values come from the 2006 IPCC defaults in the IPCC Emission Factor
#' Database (EFDB). The corresponding EFDB extract is included in the package as
#' `inst/extdata/EFDB_output.csv`.
#'
#' @return A tibble with columns `siec`, `fuel`, and `ncv_kjkg`.
#' @export
#'
#' @examples
#' get_ipcc_ncv()
get_ipcc_ncv <- function() {
  tibble::tribble(
    ~siec, ~fuel, ~ncv_kjkg,
    SIEC_HARD_COAL, "Anthracite", 26700,
    SIEC_BROWN_COAL, "Lignite", 11900,
    SIEC_BROWN_COAL_BRIQUETTES, "Brown Coal Briquettes", 20700,
    SIEC_CRUDE_OIL, "Crude Oil", 42300,
    SIEC_NATURAL_GAS, "Natural Gas", 48000,
    SIEC_COKE_OVEN_COKE, "Coke Oven Coke and Lignite Coke", 28200,
    SIEC_OIL_PRODUCTS, "Other Petroleum Products", 40200,
    SIEC_ROAD_DIESEL, "Diesel Oil", 43000,
    SIEC_GASOIL_DIESEL, "Gas Oil", 43000,
    SIEC_AVIATION_GASOLINE, "Aviation Gasoline", 44300,
    SIEC_MOTOR_GASOLINE_XBIO, "Motor Gasoline", 44300,
    SIEC_FUEL_OIL, "Residual Fuel Oil", 40400,
    SIEC_HEATING_GASOIL, "Gas Oil", 43000,
    SIEC_KEROSENE_XBIO, "Jet Kerosene", 44100
  )
}
