#' Get daily CO2 emissions from EUROSTAT, ENTSOG and ENTSOE data.
#'
#' @param diagnostics_folder
#' @param use_cache whether to use cache for EUROSTAT, power and gas data.
#' Save time for development. Should be Falsein production.
#' @param iso2s
#' @param downscale_daily
#' @param min_year
#' @param date_to End date for filtering data (YYYY-MM-DD). If NULL, no upper date filter is
#' applied.
#' @param ncv_source iea, iea_shared, ipcc
#' @param fill_mode one of "missing", "overwrite", or "ratio". Default is "missing".
#' @param data_masking Optional named list of masking rules. See
#'   `get_data_masking_config()` for a template.
#'
#' @return
#' @export
#'
#' @examples
get_co2 <- function(
  diagnostics_folder = "diagnostics",
  downscale_daily = TRUE,
  use_cache = FALSE,
  iso2s = get_eu_iso2s(include_eu = TRUE),
  min_year = NULL,
  date_to = today(),
  ncv_source = "iea",
  fill_mode = c("missing", "overwrite", "ratio"),
  downscale_cut_latest_days = 3,
  correct_gas_demand_to_eurostat = TRUE,
  data_masking = NULL
) {
  run_started_at <- Sys.time()
  log_info(
    glue::glue(
      "[{format_log_timestamp(run_started_at)}] START get_co2 (iso2s={length(iso2s)}, downscale_daily={downscale_daily}, use_cache={use_cache})"
    )
  )

  create_dir(diagnostics_folder)

  source_diagnostics_folder <- function(name) {
    if (is_null_or_empty(diagnostics_folder)) {
      NULL
    } else {
      file.path(diagnostics_folder, name)
    }
  }

  # Add buffer to date_to if downscaling daily
  if (downscale_daily && !is.null(date_to)) {
    date_to_cut <- as.Date(date_to) + downscale_cut_latest_days
  } else {
    date_to_cut <- date_to
  }

  # Collect necessary data
  gas_demand <- log_timed_stage("get_gas_demand", {
    get_gas_demand(
      iso2s = iso2s,
      date_to = date_to_cut,
      correct_to_eurostat = correct_gas_demand_to_eurostat,
      use_cache = use_cache,
      diagnostics_folder = source_diagnostics_folder("gas_demand"),
      data_masking = data_masking
    )
  })

  pwr_generation <- log_timed_stage("get_power_generation", {
    get_power_generation(
      iso2s = iso2s,
      date_to = date_to_cut,
      use_cache = use_cache,
      diagnostics_folder = source_diagnostics_folder("power_generation"),
      data_masking = data_masking
    )
  })


  # Get fossil-fuel consumption based on Eurostat
  eurostat_cons <- log_timed_stage("get_eurostat_cons", {
    get_eurostat_cons(
      diagnostics_folder = file.path(diagnostics_folder, "eurostat"),
      pwr_generation = pwr_generation,
      use_cache = use_cache,
      data_masking = data_masking
    )
  })

  # Get industrial production data from Eurostat
  eurostat_indprod <- log_timed_stage("get_eurostat_indprod", {
    get_eurostat_indprod(
      use_cache = use_cache,
      diagnostics_folder = file.path(diagnostics_folder, "eurostat"),
      data_masking = data_masking
    )
  })

  # Compute CO2 emissions based on Eurostat fossil-fuel consumption/oxydation
  co2_unprojected <- log_timed_stage("get_co2_from_eurostat_cons", {
    get_co2_from_eurostat_cons(
      eurostat_cons,
      ncv_source = ncv_source,
      diagnostics_folder = diagnostics_folder,
      use_cache = use_cache
    )
  })

  # Project and impute data until now using various methods
  # We need to have all EU countries there as some EU imputation
  # relies on its member states
  co2 <- log_timed_stage("project_until_now", {
    project_until_now(
      co2_unprojected,
      pwr_generation = pwr_generation,
      gas_demand = gas_demand,
      eurostat_indprod = eurostat_indprod,
      fill_mode = fill_mode,
      date_to = date_to
    )
  })


  if (!is_null_or_empty(diagnostics_folder)) {
    log_timed_stage("diagnose_eu_vs_countries", {
      diagnose_eu_vs_countries(
        co2_unprojected = co2_unprojected,
        co2 = co2,
        pwr_generation = pwr_generation,
        eurostat_cons = eurostat_cons,
        diagnostics_folder = file.path(diagnostics_folder, "eu_vs_countries")
      )
    })
  }

  # Filter regions
  # Note: this need to be done after co2 estimates,
  # as all EU countries will be used to estimate weighted average NCVs
  co2 <- co2 %>%
    filter(iso2 %in% iso2s)

  # Downscale to daily data
  if (downscale_daily) {
    co2 <- log_timed_stage("downscale_daily", {
      downscale_daily(
        co2 = co2,
        pwr_generation = pwr_generation,
        gas_demand = gas_demand,
        cut_latest_days = downscale_cut_latest_days
      )
    })
  }

  # Resplit gas
  co2 <- split_gas_to_elec_others(co2)

  # Re-combine fuels e.g. peat goes to coal
  co2 <- recombine_fuels(co2)

  # Add total
  co2 <- add_total_co2(co2)

  # Validation
  log_timed_stage("validate_co2", {
    validate_co2(co2, diagnostics_folder = diagnostics_folder)
  })

  # Final tweaks
  co2 <- co2 %>%
    mutate(
      region = countrycode::countrycode(
        iso2,
        origin = "iso2c",
        destination = "country.name",
        custom_match = c("EU" = "EU")
      )
    ) %>%
    mutate(unit = "t") %>%
    filter(!is.na(date)) %>%
    {
      if (!is.null(min_year)) {
        filter(., year(date) >= min_year)
      } else {
        .
      }
    }

  run_ended_at <- Sys.time()
  total_elapsed_s <- as.numeric(difftime(run_ended_at, run_started_at, units = "secs"))
  log_info(glue::glue("[{format_log_timestamp(run_ended_at)}] DONE get_co2 ({round(total_elapsed_s, 2)}s)"))

  return(co2)
}
