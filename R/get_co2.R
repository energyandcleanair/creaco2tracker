
#' Get daily CO2 emissions from EUROSTAT, ENTSOG and ENTSOE data.
#'
#' @param diagnostics_folder
#' @param use_cache whether to use cache for EUROSTAT, power and gas data.
#' Save time for development. Should be Falsein production.
#' @param iso2s
#' @param downscale_daily
#' @param min_year
#' @param ncv_source iea, iea_shared, ipcc
#' @param fill_mode one of "missing", "overwrite", or "ratio". Default is "missing".
#'
#' @return
#' @export
#'
#' @examples
get_co2 <- function(diagnostics_folder='diagnostics',
                    downscale_daily=T,
                    use_cache=F,
                    iso2s = get_eu_iso2s(include_eu = T),
                    min_year = NULL,
                    ncv_source = "iea",
                    fill_mode=c("missing", "overwrite", "ratio")){

  create_dir(diagnostics_folder)

  # Collect necessary data
  gas_demand <- download_gas_demand(iso2 = NULL, use_cache = use_cache)
  pwr_generation <- entsoe.get_power_generation(use_cache = use_cache)

  # Get fossil-fuel consumption based on Eurostat
  eurostat_cons <- get_eurostat_cons(
    diagnostics_folder = file.path(diagnostics_folder, "eurostat"),
    pwr_generation = pwr_generation,
    use_cache = use_cache)

  # Get industrial production data from Eurostat
  eurostat_indprod <- get_eurostat_indprod(
    use_cache = use_cache,
    diagnostics_folder = file.path(diagnostics_folder, 'eurostat')
    )

  # Quick sanity checks on power generation
  if(!is_null_or_empty(diagnostics_folder)){
    diagnostic_pwr(pwr_generation,
                   diagnostics_folder = file.path(diagnostics_folder, "pwr"))
  }


  # Compute CO2 emissions based on Eurostat fossil-fuel consumption/oxydation
  co2_unprojected <- get_co2_from_eurostat_cons(eurostat_cons,
                                                ncv_source = ncv_source,
                                                diagnostics_folder = diagnostics_folder,
                                                use_cache = use_cache
                                                )


  # Project and impute data until now using various methods
  # We need to have all EU countries there as some EU imputation
  # relies on its member states
  co2 <- project_until_now(co2_unprojected,
                           pwr_generation=pwr_generation,
                           gas_demand=gas_demand,
                           eurostat_indprod=eurostat_indprod,
                           fill_mode=fill_mode)


  if(!is_null_or_empty(diagnostics_folder)){
    diagnose_eu_vs_countries(
      co2_unprojected = co2_unprojected,
      co2 = co2,
      pwr_generation = pwr_generation,
      eurostat_cons = eurostat_cons %>% filter(time < "2025-01-01"),
      diagnostics_folder = file.path(diagnostics_folder, 'eu_vs_countries')
      )
  }


  # Filter regions
  # Note: this need to be done after co2 estimates,
  # as all EU countries will be used to estimate weighted average NCVs
  co2 <- co2 %>%
    filter(iso2 %in% iso2s)

  # Downscale to daily data
  if(downscale_daily){
    co2 <- downscale_daily(co2 = co2, pwr_generation = pwr_generation, gas_demand = gas_demand)
  }

  # Resplit gas
  co2 <- split_gas_to_elec_others(co2)

  # Add total
  co2 <- add_total_co2(co2)

  # Validation
  validate_co2(co2, diagnostics_folder=diagnostics_folder)

   # Final tweaks
  co2 <- co2 %>%
    mutate(region = countrycode::countrycode(iso2,
                                             origin = "iso2c",
                                             destination = "country.name",
                                             custom_match = c("EU"="EU")
                                             )) %>%
    mutate(unit="t") %>%
    filter(!is.na(date)) %>%
    {
      if(!is.null(min_year)){
        filter(., year(date) >= min_year)
      } else {
        .
      }
    }

  return(co2)
}
