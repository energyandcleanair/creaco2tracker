#' Fetch masked Eurostat source datasets for fuel consumption processing
#'
#' @param use_cache Whether to use source-level cache.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Named list containing `oil`, `solid`, and `gas`, each with
#'   `monthly` and `yearly` tibbles.
#' @keywords internal
eurostat_data_access_get_cons_sources <- function(
  use_cache = FALSE,
  data_masking = NULL
) {
  list(
    oil = log_timed_stage("eurostat_collect_oil", {
      collect_oil(use_cache = use_cache, data_masking = data_masking)
    }),
    solid = log_timed_stage("eurostat_collect_solid", {
      collect_solid(use_cache = use_cache, data_masking = data_masking)
    }),
    gas = log_timed_stage("eurostat_collect_gas", {
      collect_gas(use_cache = use_cache, data_masking = data_masking)
    })
  )
}


#' Fetch masked Eurostat industrial production dataset
#'
#' @param use_cache Whether to use source-level cache.
#' @param iso2s Optional ISO2 filter.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Tibble with Eurostat industrial production data.
#' @keywords internal
eurostat_data_access_get_indprod <- function(
  use_cache = FALSE,
  iso2s = NULL,
  data_masking = NULL
) {
  indprod_raw <- log_timed_stage("eurostat_indprod_fetch", {
    get_eurostat_from_code(
      code = "sts_inpr_m",
      use_cache = use_cache,
      iso2s = iso2s
    )
  })

  indprod_with_iso2 <- log_timed_stage("eurostat_indprod_add_iso2", {
    indprod_raw %>% add_iso2()
  })

  log_timed_stage("eurostat_indprod_mask", {
    indprod_with_iso2 %>%
      apply_source_data_mask(
        source_name = "eurostat_indprod",
        data_masking = data_masking
      )
  })
}


#' Collect oil consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @param data_masking Optional named list of masking rules.
#' @return Raw oil consumption data from EUROSTAT
#' @export
collect_oil <- function(use_cache = FALSE, data_masking = NULL) {
  oil_siec_codes <- c(
    SIEC_OIL_PRODUCTS,
    SIEC_CRUDE_OIL,
    SIEC_ROAD_DIESEL,
    SIEC_MOTOR_GASOLINE_XBIO,
    SIEC_HEATING_GASOIL,
    SIEC_FUEL_OIL,
    SIEC_GASOIL_DIESEL,
    SIEC_KEROSENE_XBIO,
    SIEC_AVIATION_GASOLINE,
    SIEC_BIOGASOLINE,
    SIEC_BIODIESEL
  )

  # Monthly data
  cons_monthly_raw <- log_timed_stage("collect_oil_fetch_monthly", {
    get_eurostat_from_code(
      code = "nrg_cb_oilm",
      use_cache = use_cache,
      filters = list(siec = oil_siec_codes)
    ) %>%
      filter(siec %in% oil_siec_codes) %>%
      apply_source_data_mask(
        source_name = "eurostat_oil_monthly",
        data_masking = data_masking
      )
  })

  # Yearly data
  cons_yearly_raw <- log_timed_stage("collect_oil_fetch_yearly", {
    get_eurostat_from_code(
      code = "nrg_cb_oil",
      use_cache = use_cache,
      filters = list(siec = oil_siec_codes)
    ) %>%
      filter(siec %in% oil_siec_codes) %>%
      apply_source_data_mask(
        source_name = "eurostat_oil_yearly",
        data_masking = data_masking
      )
  })

  # Add missing GID_NE when it happens
  cons_yearly <- log_timed_stage("collect_oil_fill_non_energy_yearly", {
    fill_oil_non_energy_use_yearly(cons_yearly_raw)
  })

  # Missing GID_OBS -> fill with GID_CAL
  cons_yearly <- log_timed_stage("collect_oil_fill_gid_obs_yearly", {
    fill_gid_obs_with_gid_cal(cons_yearly)
  })

  # And GID_NE
  cons_monthly_filled <- log_timed_stage("collect_oil_fill_non_energy_monthly", {
    fill_oil_non_energy_use_monthly(
      yearly = cons_yearly,
      monthly = cons_monthly_raw
    )
  })

  # Add Oil transport
  cons_monthly_filled <- log_timed_stage("collect_oil_add_transport", {
    add_oil_transport(
      monthly = cons_monthly_filled,
      yearly = cons_yearly
    )
  })

  # Missing GID_OBS -> fill with GID_CAL
  cons_monthly_filled <- log_timed_stage("collect_oil_fill_gid_obs_monthly", {
    fill_gid_obs_with_gid_cal(cons_monthly_filled)
  })

  list(
    monthly = cons_monthly_filled %>% add_iso2() %>% filter(!is.na(iso2)),
    yearly = cons_yearly %>% add_iso2() %>% filter(!is.na(iso2))
  )
}


#' Collect solid fuel (i.e. coal and coke so far for us) consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @param data_masking Optional named list of masking rules.
#' @return Raw solid fuel consumption data from EUROSTAT
#' @export
collect_solid <- function(use_cache = FALSE, data_masking = NULL) {
  # Monthly data
  consumption_codes_monthly <- c("nrg_cb_sffm")
  cons_monthly_raw <- get_eurostat_from_code(
    code = consumption_codes_monthly,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "eurostat_solid_monthly",
      data_masking = data_masking
    )

  # Yearly data
  consumption_codes_yearly <- c("nrg_cb_sff")
  cons_yearly_raw <- get_eurostat_from_code(
    code = consumption_codes_yearly,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "eurostat_solid_yearly",
      data_masking = data_masking
    )

  # Only keep certain SIEC codes
  # to avoid double counting
  # e.g. shouldn't keep both Lignite and Brown coal
  siec_codes <- c(
    SIEC_BROWN_COAL,
    SIEC_HARD_COAL,
    SIEC_COKE_OVEN_COKE,
    SIEC_BROWN_COAL_BRIQUETTES,
    SIEC_OIL_SHALE,
    SIEC_PEAT
  )

  filter_siec <- function(x) filter(x, siec %in% siec_codes)

  list(
    monthly = filter_siec(cons_monthly_raw) %>% add_iso2() %>% filter(!is.na(iso2)),
    yearly = filter_siec(cons_yearly_raw) %>% add_iso2() %>% filter(!is.na(iso2))
  )
}


#' Collect gas consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @param data_masking Optional named list of masking rules.
#' @return Raw gas consumption data from EUROSTAT
#' @export
collect_gas <- function(use_cache = FALSE, data_masking = NULL) {
  # Monthly data
  consumption_codes_monthly <- c("nrg_cb_gasm")
  cons_monthly_raw <- get_eurostat_from_code(
    code = consumption_codes_monthly,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "eurostat_gas_monthly",
      data_masking = data_masking
    )

  # Yearly data - with specific filters for gas
  gas_nrg_bal_yearly <- c(
    NRG_BAL_IC_OBS, # Inland consumption - observed
    NRG_BAL_FC_NE, # Final consumption - non-energy use
    # Transformation input - electricity and heat generation - main activity producers
    NRG_BAL_TI_EHG_MAPE_E,
    # Monthly data doesn't have distinction between elec and heat only. We then include it to
    # ensuire continuity
    # even though this is technically innacurate
    # Transformation input - electricity and heat generation - HEAT ONLY
    NRG_BAL_TI_EHG_MAPH_E,
    # Transformation input - electricity and heat generation - main activity producer
    # combined heat and power - energy use
    NRG_BAL_TI_EHG_MAPCHP_E
  )

  cons_yearly_raw <- get_eurostat_from_code(
    code = "nrg_cb_gas",
    use_cache = use_cache,
    filters = list(nrg_bal = gas_nrg_bal_yearly)
  ) %>%
    apply_source_data_mask(
      source_name = "eurostat_gas_yearly",
      data_masking = data_masking
    )

  # Add Final consumption - non energy use to gas monthly data
  cons_monthly_raw <- add_gas_non_energy(
    cons_monthly_raw = cons_monthly_raw,
    cons_yearly_raw = cons_yearly_raw
  )

  # Add missing 2019 elec data for gas
  cons_monthly_raw <- fill_ng_elec_eu27(cons_monthly_raw = cons_monthly_raw)

  list(
    monthly = cons_monthly_raw %>% add_iso2() %>% filter(!is.na(iso2)),
    yearly = cons_yearly_raw %>% add_iso2() %>% filter(!is.na(iso2))
  )
}


