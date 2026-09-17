#' Project CO2 emissions until today using various proxies
#'
#' @param co2
#' @param pwr_generation
#' @param gas_demand
#' @param eurostat_indprod
#' @param fill_mode one of "overwrite", "missing", or "ratio". Default is "missing".
#'
#' @return
#' @export
#'
#' @examples
project_until_now <- function(
  co2_unprojected,
  pwr_generation,
  gas_demand,
  eurostat_indprod,
  fill_mode = c("missing", "overwrite", "ratio"),
  date_to = NULL
) {
  fill_mode <- match.arg(fill_mode) # Takes the first one by default i.e. missing
  separate <- attr(co2_unprojected, "coal_separate_projection")
  if (!is.null(separate) && nrow(separate) > 0 && !"estimate" %in% names(separate)) {
    separate <- separate %>% tidyr::crossing(estimate = c("central", "lower", "upper"))
  }
  if (nrow(co2_unprojected) == 0 && !is.null(separate)) return(separate)

  dts_month <- seq.Date(min(co2_unprojected$date), if (is.null(date_to)) {
    today() %>%
      "day<-"(1)
  } else {
    as.Date(date_to)
  }, by = "month")

  # Preserve a known total whose sector split is unavailable, but do not train
  # or forecast it as if `unknown` were an economic sector. These rows rejoin
  # after the projection steps and contribute only in their observed periods.
  unallocated <- co2_unprojected %>%
    filter(sector == SECTOR_UNKNOWN)
  if (!"estimate" %in% names(unallocated)) {
    unallocated <- unallocated %>%
      tidyr::crossing(estimate = c("central", "lower", "upper"))
  }
  if (all(co2_unprojected$sector == SECTOR_UNKNOWN)) {
    return(bind_rows(unallocated, separate))
  }

  co2_unprojected %>%
    filter(sector != SECTOR_UNKNOWN) %>%
    split_gas_to_elec_all() %>%
    project_until_now_elec(
      pwr_generation = pwr_generation, dts_month = dts_month, fill_mode =
        fill_mode
    ) %>%
    project_until_now_gas(gas_demand = gas_demand, dts_month = dts_month, fill_mode = fill_mode) %>%
    project_eu_from_countries(dts_month = dts_month, fill_mode = fill_mode) %>%
    # We use industry for coal others
    project_until_now_coal_others(
      eurostat_indprod = eurostat_indprod, dts_month = dts_month,
      fill_mode = fill_mode
    ) %>%
    fill_eu_internal_gaps() %>%
    # Then run projections
    project_until_now_forecast(dts_month = dts_month) %>%
    # Forecast expansion can introduce internal NA rows that did not exist when
    # the pre-forecast EU gap fill ran.
    fill_eu_internal_gaps() %>%
    # And detotalise, since data from total and other sectors may now overlap again
    bind_rows(unallocated) %>%
    bind_rows(separate) %>%
    group_by(iso2, date, fuel, sector, unit, estimate) %>%
    summarise(value = if (anyNA(value)) NA_real_ else sum(value), .groups = "drop") %>%
    detotalise_co2()
}
