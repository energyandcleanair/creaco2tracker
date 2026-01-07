#' Collect solid fuel (i.e. coal and coke so far for us) consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @return Raw solid fuel consumption data from EUROSTAT
#' @export
collect_solid <- function(use_cache = FALSE) {
  # Monthly data
  consumption_codes_monthly <- c("nrg_cb_sffm")
  cons_monthly_raw <- get_eurostat_from_code(
        code = consumption_codes_monthly,
        use_cache = use_cache
    )

  # Yearly data
  consumption_codes_yearly <- c("nrg_cb_sff")
  cons_yearly_raw <- get_eurostat_from_code(
        code = consumption_codes_yearly,
        use_cache = use_cache
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

  filter_siec <- function(x) filter(x, siec_code %in% siec_codes)

  # # Remove weird data points
  # cons_monthly_raw <- cons_monthly_raw %>%
  #     filter(geo!='Slovenia' | time != '2023-12-01' | siec_code != SIEC_HARD_COAL)

  list(
        monthly = filter_siec(cons_monthly_raw) %>% add_iso2() %>% filter(!is.na(iso2)),
        yearly = filter_siec(cons_yearly_raw) %>% add_iso2() %>% filter(!is.na(iso2))
    )
}

process_solid_monthly <- function(x, pwr_generation) {
  # This one is a bit tricky: for certain months/regions,
  # EUROSTAT has gross inland deliveries data but no transformation/consumption data
  # We should make sure to filter out these months so that it's not considered months without coal
  # consumption
  NRG_TRANS_ELEC <- "TI_EHG_MAP"
  NRG_GID_CALCULATED <- "GID_CAL"
  NRG_TRANS_COKING <- "TI_CO"

  by_sector <- x %>%
    filter(nrg_bal_code %in% c(NRG_GID_CALCULATED, NRG_TRANS_ELEC, NRG_TRANS_COKING)) %>%
    mutate(sector = if_else(nrg_bal_code == NRG_TRANS_ELEC, SECTOR_ELEC, SECTOR_ALL)) %>%
  # Date valid only from 2014
  filter(time >= "2014-01-01") %>%
    select(iso2, time, siec_code, nrg_bal_code, sector, unit, values)


  view_gaps <- function(x, sector, siec_code) {
    times <- unique(x$time)
    x %>%
      filter(sector == !!sector, siec_code == !!siec_code) %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
      ungroup() %>%
      arrange(desc(time)) %>%
      tidyr::complete(
        iso2,
        nesting(nrg_bal_code, siec_code, unit, sector),
        time = times,
        fill = list(values = NA)
      ) %>%
      select(iso2, time, values) %>%
      spread(iso2, values) %>%
      arrange(desc(time))
  }

  #############################
  # Apply manual fixes
  #############################
  # Greece has started declaring 0 brown coal values for elec starting from 2015-09-01,
  # but apparently 100% of brown coal was used for elec before that
  to_add_greece <- by_sector %>%
    filter((iso2 == "GR" & siec_code == SIEC_BROWN_COAL & time >= "2015-09-01" & nrg_bal_code == NRG_GID_CALCULATED)) %>%
    mutate(sector = SECTOR_ELEC,
           nrg_bal_code = NRG_TRANS_ELEC
           )

  by_sector_fixed <- bind_rows(
      by_sector %>%
        filter(!(iso2 == "GR" & siec_code == SIEC_BROWN_COAL & time >= "2015-09-01" & sector == SECTOR_ELEC)),
      to_add_greece
    )

  # Add the difference to EU
  to_add_to_eu <- to_add_greece %>%
    mutate(iso2 = "EU") %>%
    select(iso2, time, siec_code, sector, unit, value_to_add = values)

  by_sector_fixed <- by_sector_fixed %>%
    left_join(to_add_to_eu, relationship = "one-to-one") %>%
    mutate(value_to_add = tidyr::replace_na(value_to_add, 0)) %>%
    mutate(values = values + value_to_add) %>%
    select(-value_to_add)

  #########################
  # Fill hard coal electricity
  # Slovenia stopped declaring hard coal elec since 2023-12-01
  # yet it seems to be 0 (see investigate function above)
  # As a result, EU has no data either since that date
  # We rebuild EU Hard coal elec as the sum of all countries (when there are enough that is)
  # By the same token, we also update coking as it will be used later on
  ##########################
  # We take hard coal from all countries.

  # Manual France related fixes
  by_sector_fixed <- by_sector_fixed %>%
    mutate(values = case_when(
      # France has some gaps in Hard coal electricity that are annoying to get EU27 total
      # We fill them with 0 after checking some dates in ENTSOE and confirming there was no coal gen
      sector == SECTOR_ELEC & iso2 == "FR" & siec_code == SIEC_HARD_COAL & is.na(values) ~ 0,
      # For coking input, it stopped publishing data in around 2020-09
      # we ignore for now, but would be good to implement a better way to guess values
      # TODO Improve this
      nrg_bal_code == NRG_TRANS_COKING & iso2 == "FR" & siec_code == SIEC_HARD_COAL & is.na(values) ~ 0,
      TRUE ~ values
    ))

  by_sector_fixed <- fill_gaps_in_time_series(
    data = by_sector_fixed,
    group_cols = c("iso2", "siec_code", "nrg_bal_code", "sector", "unit"),
    exclude_iso2s = "EU"
  )

  # Fill missing EU values using sum of countries
  by_sector_fixed <- fill_eu_from_countries_sum(
    data = by_sector_fixed,
    group_cols = c("sector", "siec_code", "nrg_bal_code", "unit", "time"),
    min_countries = 25,
    max_rel_diff = 0.05
  ) %>%
    mutate(fuel = ifelse(siec_code == SIEC_COKE_OVEN_COKE, FUEL_COKE, FUEL_COAL))


  # Remove part of the coal that is used to produced coke to avoid double counting
  # Though keeping a share to represent coke oven gas emissions (see in process_solid_yearly)
  result <- by_sector_fixed %>%
    mutate(factor = case_when(nrg_bal_code == NRG_TRANS_COKING ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
                              TRUE ~ 1)) %>%
    group_by(iso2, siec_code, sector, fuel, unit, time) %>%
    summarise(values = sum(values * factor, na.rm = F))


  # Remove recent dates where fewer siec and sector per date are available
  keep_siec <- result %>%
    ungroup() %>%
    filter(!is.na(values)) %>%
    group_by(iso2, sector, time) %>%
    summarise(n = n_distinct(siec_code)) %>%
    mutate(keep = n == max(n) | time < max(time) - months(36))

  result <- result %>%
    add_iso2() %>%
    left_join(keep_siec) %>%
    filter(keep) %>%
    select(-keep, - n)

  keep_sector <- result %>%
    ungroup() %>%
    filter(!is.na(values)) %>%
    group_by(iso2, fuel, time) %>%
    summarise(n = n_distinct(sector)) %>%
    mutate(keep = n == max(n) | time < max(time) - months(36))

  result <- result %>%
    add_iso2() %>%
    left_join(keep_sector) %>%
    filter(keep) %>%
    select(-keep, - n)

  # Add siec
  # stopifnot(!"siec" %in% colnames(result))
  # result <- result %>%
  #   ungroup() %>%
  #   left_join(x %>% distinct(siec_code, siec))


  return(result)

}

process_solid_yearly <- function(x) {

  NRG_FINAL_ENERGY <- "FC_E"
  NRG_TRANS_ENERGY <- "TI_E"
  NRG_ELEC_CHP <- "TI_EHG_MAPCHP_E"
  NRG_ELEC_ONLY <- "TI_EHG_MAPE_E"
  NRG_TRANS_COKING <- "TI_CO_E"
  NRG_FINAL_IRON_STEEL <- "FC_IND"

  result <- x %>%
        filter(nrg_bal_code %in% c(
          NRG_FINAL_ENERGY,
          NRG_TRANS_ENERGY,
          NRG_ELEC_CHP,
          NRG_ELEC_ONLY,
          NRG_TRANS_COKING) |
            (nrg_bal_code == NRG_FINAL_IRON_STEEL & siec_code == SIEC_COKE_OVEN_COKE)) %>%
        mutate(
            sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL),
            fuel = ifelse(siec_code == SIEC_COKE_OVEN_COKE, FUEL_COKE, FUEL_COAL)
        ) %>%

  # Remove MOST OF coal used to produce coke to avoid double counting
  # We find in investigate_coking_emissions that we can approximate that
  # coke oven gas emissions represent roughly 8% of the equivalent
  # of hard coal emissions. So we keep some of it to account for it
  # (coke oven gas will then be under Coal category)
  mutate(factor = case_when(
      nrg_bal_code == NRG_TRANS_COKING ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
      TRUE ~ 1)
      ) %>%
    group_by(iso2, time, siec_code, sector, fuel, unit) %>%
    summarise(values = sum(values * factor, na.rm = T), .groups = "drop")

  return(result)
}
