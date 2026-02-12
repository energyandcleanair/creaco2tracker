#' Get Weather-Corrected Demand for Gas and Electricity
#'
#' Computes weather-corrected gas demand (excluding power sector) and
#' electricity demand using demand component decomposition. Replaces actual
#' HDD/CDD with day-of-year climatological means to produce corrected values.
#'
#' @param diagnostics_folder Folder for diagnostic outputs.
#' @param use_cache Whether to use cached data.
#' @param model_type Model type for demand decomposition: "lm" or "gam".
#'
#' @return Tibble formatted for DB upload with columns:
#'   date, unit, value, fuel, sector, data_source, frequency, region_id, region_type
#'
#' @export
get_corrected_demand <- function(diagnostics_folder = "diagnostics",
                                 use_cache = TRUE,
                                 model_type = c("gam", "lm")) {

  model_type <- match.arg(model_type)
  create_dir(diagnostics_folder)

  # Get demand components with weather correction
  components <- get_demand_components(
    iso2s = "EU",
    date_from = "2015-01-01",
    date_to = Sys.Date(),
    use_cache = use_cache,
    model_type = model_type,
    diagnostics_folder = file.path(diagnostics_folder, "demand_components")
  )

  # Gas: sum heating + others (exclude "electricity" = gas-to-power component)
  # to get total gas demand excluding power sector
  corrected_gas_demand <- components %>%
    filter(fuel == "fossil_gas", component != "electricity") %>%
    filter(!is.na(value_weather_corrected)) %>%
    group_by(date) %>%
    summarise(value = sum(value_weather_corrected, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      # Convert from m3 to TWh/day
      value = value / 55 / 3.6 / 1000,
      fuel = "fossil_gas_temperature_corrected",
      sector = "except_power",
      data_source = "crea",
      unit = "TWh/day",
      frequency = "daily",
      region_id = "EU",
      region_type = "region"
    )

  # Electricity: sum all components (heating + cooling + others)
  corrected_electricity_demand <- components %>%
    filter(fuel == "electricity") %>%
    filter(!is.na(value_weather_corrected)) %>%
    group_by(date) %>%
    summarise(value = sum(value_weather_corrected, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      # Convert from MW to GW
      value = value / 1e3,
      fuel = "electricity_temperature_corrected",
      sector = "total",
      data_source = "crea",
      unit = "GW",
      frequency = "daily",
      region_id = "EU",
      region_type = "region"
    )

  bind_rows(corrected_gas_demand, corrected_electricity_demand)
}
