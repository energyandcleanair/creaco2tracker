#' Fetch masked Eurostat source datasets for fuel consumption processing
#'
#' @param use_cache Whether to use source-level cache.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Named list containing `oil`, `solid`, and `gas`, each with
#'   `monthly` and `yearly` tibbles.
#' @keywords internal
eurostat_data_access_get_cons_sources <- function(use_cache = FALSE,
                                                  data_masking = NULL) {
  cons_raw_oil <- collect_oil(use_cache = use_cache)
  cons_raw_solid <- collect_solid(use_cache = use_cache)
  cons_raw_gas <- collect_gas(use_cache = use_cache)

  list(
    oil = list(
      monthly = apply_source_data_mask(
        cons_raw_oil$monthly,
        source_name = "eurostat_oil_monthly",
        data_masking = data_masking
      ),
      yearly = apply_source_data_mask(
        cons_raw_oil$yearly,
        source_name = "eurostat_oil_yearly",
        data_masking = data_masking
      )
    ),
    solid = list(
      monthly = apply_source_data_mask(
        cons_raw_solid$monthly,
        source_name = "eurostat_solid_monthly",
        data_masking = data_masking
      ),
      yearly = apply_source_data_mask(
        cons_raw_solid$yearly,
        source_name = "eurostat_solid_yearly",
        data_masking = data_masking
      )
    ),
    gas = list(
      monthly = apply_source_data_mask(
        cons_raw_gas$monthly,
        source_name = "eurostat_gas_monthly",
        data_masking = data_masking
      ),
      yearly = apply_source_data_mask(
        cons_raw_gas$yearly,
        source_name = "eurostat_gas_yearly",
        data_masking = data_masking
      )
    )
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
eurostat_data_access_get_indprod <- function(use_cache = FALSE,
                                             iso2s = NULL,
                                             data_masking = NULL) {
  get_eurostat_from_code(code = "sts_inpr_m", use_cache = use_cache, iso2s = iso2s) %>%
    add_iso2() %>%
    apply_source_data_mask(
      source_name = "eurostat_indprod",
      data_masking = data_masking
    )
}


get_eurostat_from_code <- function(code, iso2s=NULL, use_cache = T, filters = NULL) {

  # Create a digest of iso2s and filters
  create_dir("cache")
  digest <- digest::digest(list(iso2s, filters))
  filepath <- file.path("cache", glue("eurostat_{code}_{digest}.rds"))

  if (use_cache & file.exists(filepath)) {
    return(readRDS(filepath))
  }

  if(!is.null(iso2s)){
    filters$geo <- iso2s
  }

  raw <- eurostat::get_eurostat(code, filters = filters, keepFlags=T)
  keep_code <- intersect(names(raw), c("nrg_bal", "siec", "nace_r2", "prod_nrg"))
  if (length(keep_code) == 0) keep_code <- NULL
  raw %>%
    eurostat::label_eurostat(code = keep_code, fix_duplicated = T) %>% # Keep nrg_bal code as well
    dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD"))) %T>% # Column changed with new EUROSTAT version
    saveRDS(filepath)
}