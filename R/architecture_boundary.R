#' Data-access architecture boundary contract
#'
#' The codebase is split into two layers for CO2-critical pipelines.
#'
#' Behind data-access:
#' - Fetches remote data and reads/writes source caches.
#' - Applies source-level masking rules.
#' - Includes source client modules that talk directly to external APIs.
#'
#' In front of data-access:
#' - Performs modeling, reconciliation, decomposition, and orchestration.
#' - Must not fetch raw sources directly.
#' - Must not call source masking directly.
#'
#' @return Named list with boundary metadata and guard patterns.
#' @keywords internal
architecture_get_boundary_config <- function() {
  list(
    data_access_files = c(
      "access_power.R",
      "access_eurostat.R",
      "access_gas.R",
      "access_weather.R"
    ),
    client_files = c(
      "client_entsoe_api.R",
      "client_ember_api.R",
      "client_agsi_api.R",
      "client_weather_api.R",
      "client_iea_api.R"
    ),
    front_layer_files = c(
      "get_co2.R",
      "get_demand_components.R",
      "get_corrected_demand.R",
      "get_gas_demand.R",
      "get_power_generation.R",
      "update.R"
    ),
    forbidden_front_patterns = c(
      "apply_source_data_mask\\s*\\(",
      "creahelpers::api.get\\s*\\(",
      "entsoe\\.get_power_generation\\s*\\(",
      "ember\\.get_power_generation\\s*\\(",
      "collect_oil\\s*\\(",
      "collect_solid\\s*\\(",
      "collect_gas\\s*\\(",
      "get_weather\\s*\\(",
      "agsi\\.get_storage_change\\s*\\(",
      "eurostat::get_eurostat\\s*\\("
    )
  )
}