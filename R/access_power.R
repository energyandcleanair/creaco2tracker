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

  # The chained scaling model needs enough history for monthly ETS ratios even
  # when the caller asks for a narrower output window.
  ets_history_start <- as.Date("2018-01-01")
  fetch_from <- min(date_from, ets_history_start)

  log_info("Fetching ENTSOE daily data...")
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

  log_info("Fetching EMBER monthly data...")
  ember_monthly <- ember.get_power_generation(
    frequency = "monthly",
    iso2s = iso2s,
    use_cache = use_cache
  ) %>%
    apply_source_data_mask(
      source_name = "ember_power_monthly",
      data_masking = data_masking
    )

  log_info("Fetching EMBER yearly data...")
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


#' Fetch installed power-generation capacity
#'
#' @param source One of `"entsoe"` or `"ember"`.
#' @param iso2s Character vector of ISO2 country codes.
#' @param date_from,date_to Optional date bounds used by ENTSOE.
#' @param use_cache Whether to use source-level caches.
#'
#' @return Installed-capacity observations from the selected source.
#' @keywords internal
power_data_access_get_installed_capacity <- function(
  source = c("entsoe", "ember"),
  iso2s = "EU",
  date_from = "2015-01-01",
  date_to = lubridate::today(),
  use_cache = TRUE
) {
  source <- match.arg(source)
  if (source == "ember") {
    return(ember.get_installed_capacity(iso2s = iso2s, use_cache = use_cache))
  }

  do.call(entsoe.get_installed_capacity, list(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache
  ))
}


#' Fetch power-generation sources used by validation
#'
#' @param iso2s Character vector of ISO2 country codes.
#' @param use_cache Whether to use source-level caches.
#' @param include_entsoe Whether to retrieve ENTSOE generation data.
#'
#' @return A list containing ENTSOE and EMBER generation data.
#' @keywords internal
power_data_access_get_validation_sources <- function(
  iso2s = get_eu_iso2s(),
  use_cache = TRUE,
  include_entsoe = TRUE
) {
  list(
    entsoe = if (include_entsoe) {
      entsoe.get_power_generation(iso2s = iso2s, use_cache = use_cache)
    } else {
      NULL
    },
    ember = ember.get_power_generation(iso2s = iso2s, use_cache = use_cache)
  )
}
