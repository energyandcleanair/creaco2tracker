process_solid_monthly <- function(x, pwr_generation) {
  # This one is a bit tricky: for certain months/regions,
  # EUROSTAT has gross inland deliveries data but no transformation/consumption data
  # We should make sure to filter out these months so that it's not considered months without coal
  # consumption
  NRG_TRANS_ELEC <- "TI_EHG_MAP"
  NRG_GID_CALCULATED <- "GID_CAL"
  NRG_TRANS_COKING <- "TI_CO"

  by_sector <- x %>%
    filter(nrg_bal %in% c(NRG_GID_CALCULATED, NRG_TRANS_ELEC, NRG_TRANS_COKING)) %>%
    mutate(sector = if_else(nrg_bal == NRG_TRANS_ELEC, SECTOR_ELEC, SECTOR_ALL)) %>%
    # Date valid only from 2014
    filter(time >= "2014-01-01") %>%
    select(iso2, time, siec, nrg_bal, sector, unit, values)


  view_gaps <- function(x, sector, siec) {
    times <- unique(x$time)
    x %>%
      filter(sector == !!sector, siec == !!siec) %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = TRUE)) %>%
      ungroup() %>%
      arrange(desc(time)) %>%
      tidyr::complete(
        iso2,
        nesting(nrg_bal, siec, unit, sector),
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
  # Greece has started declaring 0 brown coal values for elec starting from
  # 2015-09-01, but apparently 100% of brown coal was used for elec before that.
  to_add_greece <- by_sector %>%
    filter(
      (
        iso2 == "GR" & siec == SIEC_BROWN_COAL & time >= "2015-09-01" &
          nrg_bal == NRG_GID_CALCULATED
      )
    ) %>%
    mutate(
      sector = SECTOR_ELEC,
      nrg_bal = NRG_TRANS_ELEC
    )

  by_sector_fixed <- bind_rows(
    by_sector %>%
      filter(
        !(
          iso2 == "GR" & siec == SIEC_BROWN_COAL & time >= "2015-09-01" &
            sector == SECTOR_ELEC
        )
      ),
    to_add_greece
  )

  # Add the difference to EU
  to_add_to_eu <- to_add_greece %>%
    mutate(iso2 = "EU") %>%
    select(iso2, time, siec, sector, unit, value_to_add = values)

  by_sector_fixed <- by_sector_fixed %>%
    left_join(to_add_to_eu, relationship = "one-to-one") %>%
    mutate(value_to_add = tidyr::replace_na(value_to_add, 0)) %>%
    mutate(values = values + value_to_add) %>%
    select(-value_to_add)

  # Fill missing EU values using sum of countries
  by_sector_fixed <- fill_eu_from_countries_sum(
    data = by_sector_fixed,
    group_cols = c("sector", "siec", "nrg_bal", "unit", "time"),
    min_countries = 25,
    max_rel_diff = 0.05
  ) %>%
    mutate(fuel = siec_to_fuel(siec))


  # Remove part of the coal that is used to produced coke to avoid double counting
  # Though keeping a share to represent coke oven gas emissions (see in process_solid_yearly)
  result <- by_sector_fixed %>%
    mutate(
      factor = case_when(
        nrg_bal == NRG_TRANS_COKING ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
        TRUE ~ 1
      )
    ) %>%
    group_by(iso2, siec, sector, fuel, unit, time) %>%
    summarise(values = sum(values * factor, na.rm = FALSE), .groups = "drop")


  return(result)
}


#' Split solid fuel totals only when the required monthly inputs exist
#'
#' Electricity can be retained without a total. The residual "others" sector
#' requires both total consumption and electricity input. Missing dependencies
#' remain absent so the annual series can provide the existing downstream
#' fallback without converting missing components to zero.
#'
#' @keywords internal
eurostat_split_solid_elec_others <- function(x) {
  coal <- x %>% filter(siec %in% COAL_MONTHLY_GAP_FUELS)
  other_solid <- x %>%
    filter(!siec %in% COAL_MONTHLY_GAP_FUELS) %>%
    eurostat_split_elec_others()
  group_cols <- intersect(names(coal), c("iso2", "time", "unit", "siec", "fuel"))
  wide <- coal %>%
    ungroup() %>%
    filter(sector %in% c(SECTOR_ALL, SECTOR_ELEC)) %>%
    pivot_wider(
      id_cols = all_of(group_cols),
      names_from = sector,
      values_from = values,
      values_fill = NA
    ) %>%
    add_missing_cols(c("all", "electricity"))

  coal_split <- bind_rows(
    wide %>%
      filter(!is.na(electricity)) %>%
      transmute(across(all_of(group_cols)), sector = SECTOR_ELEC, values = electricity),
    wide %>%
      filter(!is.na(all), !is.na(electricity)) %>%
      transmute(across(all_of(group_cols)), sector = SECTOR_OTHERS, values = all - electricity)
  )

  bind_rows(coal_split, other_solid)
}

process_solid_yearly <- function(x) {
  NRG_FINAL_ENERGY <- "FC_E"
  NRG_TRANS_ENERGY <- "TI_E"
  NRG_ELEC_CHP <- "TI_EHG_MAPCHP_E"
  NRG_ELEC_ONLY <- "TI_EHG_MAPE_E"
  NRG_TRANS_COKING <- "TI_CO_E"
  NRG_FINAL_IRON_STEEL <- "FC_IND"

  result <- x %>%
    filter(
      nrg_bal %in% c(
        NRG_FINAL_ENERGY,
        NRG_TRANS_ENERGY,
        NRG_ELEC_CHP,
        NRG_ELEC_ONLY,
        NRG_TRANS_COKING
      ) |
        (nrg_bal == NRG_FINAL_IRON_STEEL & siec == SIEC_COKE_OVEN_COKE)
    ) %>%
    mutate(
      sector = ifelse(nrg_bal %in% c(NRG_ELEC_CHP, NRG_ELEC_ONLY), SECTOR_ELEC, SECTOR_ALL),
      fuel = siec_to_fuel(siec)
    ) %>%
    # Remove MOST OF coal used to produce coke to avoid double counting
    # We find in investigate_coking_emissions that we can approximate that
    # coke oven gas emissions represent roughly 8% of the equivalent
    # of hard coal emissions. So we keep some of it to account for it
    # (coke oven gas will then be under Coal category)
    mutate(
      factor = case_when(
        nrg_bal == NRG_TRANS_COKING ~ -1 + HARDCOAL_COKING_RATE_FACTOR,
        TRUE ~ 1
      )
    ) %>%
    group_by(iso2, time, siec, sector, fuel, unit) %>%
    summarise(values = sum_or_na(values * factor), .groups = "drop")

  return(result)
}

siec_to_fuel <- function(siec) {
  dplyr::case_when(
    siec %in% c(SIEC_BROWN_COAL, SIEC_HARD_COAL, SIEC_BROWN_COAL_BRIQUETTES, SIEC_OIL_SHALE) ~
      FUEL_COAL,
    siec == SIEC_COKE_OVEN_COKE ~ FUEL_COKE,
    siec == SIEC_PEAT ~ FUEL_PEAT,
    TRUE ~ NA_character_
  ) %>%
    # Raise error if unknown siec
    {
      if (any(is.na(.))) stop("Unknown SIEC code in siec_to_fuel") else .
    }
}
