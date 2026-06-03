#' Fetch masked source datasets for power generation processing
#'
#' This data-access entry point owns source retrieval and source-level masking
#' for ENTSOE and EMBER datasets used in power generation reconciliation.
#'
#' @param iso2s Character vector of ISO2 country codes.
#' @param date_from Start date for data retrieval.
#' @param date_to End date for data retrieval.
#' @param use_cache Whether to use source-level caches.
#' @param data_masking Optional named list of masking rules.
#'
#' @return Named list with `entsoe_daily`, `ember_monthly`, and `ember_yearly`.
#' @keywords internal
power_data_access_get_sources <- function(
  iso2s,
  date_from,
  date_to,
  use_cache = TRUE,
  data_masking = NULL
) {
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  # For tier calculation, power reconciliation needs data since 2020.
  tier_calc_start <- as.Date("2020-01-01")
  fetch_from <- min(date_from, tier_calc_start)

  message("Fetching ENTSOE daily data...")
  entsoe_daily <- entsoe.get_power_generation(
    date_from = fetch_from,
    date_to = date_to,
    iso2s = iso2s,
    use_cache = use_cache
  ) %>%
    filter(iso2 %in% iso2s) %>%
    apply_source_data_mask(
      source_name = "entsoe_power_daily",
      data_masking = data_masking
    )

  message("Fetching EMBER monthly data...")
  ember_monthly <- ember.get_power_generation(
    frequency = "monthly",
    iso2s = iso2s,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "ember_power_monthly",
      data_masking = data_masking
    )

  message("Fetching EMBER yearly data...")
  ember_yearly <- ember.get_power_generation(
    frequency = "yearly",
    iso2s = iso2s,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "ember_power_yearly",
      data_masking = data_masking
    )

  list(
    entsoe_daily = entsoe_daily,
    ember_monthly = ember_monthly,
    ember_yearly = ember_yearly
  )
}
