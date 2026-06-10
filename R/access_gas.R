#' Fetch masked ENTSOG flow source data
#'
#' @param years Integer vector of years to fetch.
#' @param use_cache Whether to use source-level cache.
#' @param verbose Whether to print verbose output.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Tibble with ENTSOG flow data.
#' @keywords internal
gas_data_access_get_entsog_flow <- function(
  years,
  use_cache = TRUE,
  verbose = FALSE,
  data_masking = NULL
) {
  creahelpers::api.get(
    "https://api.russiafossiltracker.com/v0/entsogflow",
    date_from = glue("{min(years)}-01-01}"),
    date_to = glue("{max(years)}-12-31}"),
    type = "consumption,distribution,storage,crossborder,production",
    split_by = "year",
    verbose = verbose,
    use_cache = TRUE,
    refresh_cache = !use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "entsog_flow_raw",
      data_masking = data_masking
    )
}


#' Fetch masked AGSI storage-change source data
#'
#' @param date_from Start date.
#' @param date_to End date.
#' @param iso2 Character vector of ISO2 country codes.
#' @param verbose Whether to print verbose output.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Tibble with AGSI storage change data.
#' @keywords internal
gas_data_access_get_agsi_storage <- function(
  date_from,
  date_to,
  iso2,
  use_cache = TRUE,
  verbose = FALSE,
  data_masking = NULL
) {
  agsi.get_storage_change(
    date_from = date_from,
    date_to = date_to,
    iso2 = iso2,
    use_cache = use_cache,
    verbose = verbose
  ) %>%
    apply_source_data_mask(
      source_name = "agsi_storage_daily",
      data_masking = data_masking
    )
}


#' Fetch masked Eurostat gas data used for correction
#'
#' @param data_masking Optional named list of masking rules.
#'
#' @return Tibble with Eurostat gas monthly data.
#' @keywords internal
gas_data_access_get_eurostat_monthly_for_correction <- function(
  use_cache = TRUE,
  data_masking = NULL
) {
  gas_consumption <- get_eurostat_from_code(
    code = "nrg_cb_gasm",
    use_cache = use_cache,
    filters = list(
      unit = "MIO_M3",
      siec = "G3000",
      nrg_bal = c("IC_OBS", "IPRD", "IMP", "EXP", "STK_CHG_MG")
    )
  )

  gas_consumption %>%
    filter(
      unit == "MIO_M3",
      siec == "G3000"
    ) %>%
    mutate(
      type = recode(
        nrg_bal,
        IC_OBS = "consumption",
        IPRD = "production",
        IMP = "imports",
        EXP = "minus_exports",
        STK_CHG_MG = "storage",
        .default = NA_character_
      )
    ) %>%
    filter(!is.na(type)) %>%
    mutate(values = ifelse(type %in% c("minus_exports", "storage"), -values, values)) %>%
    select(iso2 = geo, date = time, value_m3 = values, type) %>%
    mutate(
      value_m3 = value_m3 * 1e6,
      unit = "m3",
      iso2 = recode(iso2, "UK" = "GB", "EU27_2020" = "EU"),
      method = "eurostat"
    ) %>%
    apply_source_data_mask(
      source_name = "eurostat_gas_monthly_for_correction",
      data_masking = data_masking
    )
}
