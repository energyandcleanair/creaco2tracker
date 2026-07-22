#' Get CO2 emission factors used by the CO2 pipeline
#'
#' Returns fuel emission factors in tonnes of CO2 per terajoule (tCO2/TJ),
#' mapped to Eurostat Standard International Energy Product Classification
#' (SIEC) codes. Unless noted below, values come from the IPCC Emission Factor
#' Database (EFDB):
#' <https://www.ipcc-nggip.iges.or.jp/EFDB/find_ef.php>.
#'
#' The kerosene and aviation gasoline factors come from the European Environment
#' Agency (EEA), rather than the IPCC EFDB. Some petroleum-product factors are
#' calculated from their carbon content, as shown alongside the values in the
#' source file.
#'
#' @return A tibble with columns `siec` and `co2_factor_t_per_TJ`.
#' @export
#'
#' @examples
#' get_ipcc_emission_factors()
get_ipcc_emission_factors <- function() {
  tibble::tribble(
    ~siec, ~co2_factor_t_per_TJ,
    SIEC_HARD_COAL, 92.8, # EFDB ID 110620
    SIEC_BROWN_COAL, 113.1, # EFDB ID 123085
    SIEC_BROWN_COAL_BRIQUETTES, 99, # EFDB ID 123073
    SIEC_PEAT, 117.766, # EFDB ID 122005 (peat)
    SIEC_OIL_SHALE, 108, # IPCC oil shale default
    SIEC_OIL_PRODUCTS, 20 * 44 / 12, # EFDB ID 110669; carbon content conversion
    SIEC_FUEL_OIL, 77.7, # EFDB ID 121579
    SIEC_HEATING_GASOIL, 20 * 44 / 12, # EFDB ID 17174; 73.33 tCO2/TJ
    SIEC_MOTOR_GASOLINE_XBIO, 72.1, # EFDB ID 18667 (motor gasoline)
    SIEC_ROAD_DIESEL, 72.1, # EFDB ID 18919
    SIEC_CRUDE_OIL, 73, # EFDB ID 110603
    SIEC_NATURAL_GAS, 55.74, # Average of EFDB IDs 123092-123095
    SIEC_COKE_OVEN_GAS, 41.2, # EFDB ID 122159
    SIEC_COKE_OVEN_COKE, 113, # EFDB ID 110624
    SIEC_GASOIL_DIESEL, 72.1, # EFDB ID 18919
    SIEC_KEROSENE_XBIO, 72.69, # EEA
    SIEC_AVIATION_GASOLINE, 70.55 # EEA
  )
}
