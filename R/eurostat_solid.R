SIEC_BROWN_COAL <- "C0200"
SIEC_HARD_COAL <- "C0100"
SIEC_COKE_OVEN_COKE <- "C0311"
SIEC_BROWN_COAL_BRIQUETTES <- "C0330"
SIEC_OIL_SHALE <- "S2000"
SIEC_PEAT <- "P1100"

HARDCOAL_COKING_RATE_FACTOR <- 0.08 # See investigate_coking_emissions

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

investigate_coking_france <- function(monthly, indprod){


  monthly %>%
    add_iso2() %>%
    select(iso2, time, values, siec, nrg_bal_code) %>%
    filter(iso2=='FR', nrg_bal_code%in% c('TI_CO', 'GID_OBS', 'TI_EHG_MAP')) %>%
    spread(nrg_bal_code, values) %>%
    mutate(RATIO=TI_CO/(GID_OBS-TI_EHG_MAP)) %>%
    ggplot() +
    geom_line(aes(time, RATIO, col=siec)) +
    rcrea::scale_y_zero()

  View()
    gather(nrg_bal_code, values, -time, -iso2, -siec) %>%
    ggplot() + geom_line(aes(time, values, col=nrg_bal_code)) +
    rcrea::scale_y_zero() +
    facet_wrap(~siec)


  coke_fr <- get_eurostat_indprod(iso2s='FR')
  coke_fr %>% filter(nace_r2_code=='191')

  indprod %>%
    filter(geo=='France') %>%
    filter(grepl('coke', nace_r2, ignore.case=T)) %>%
    distinct(nace_r2_code)


}

investigate_solid <- function(monthly, yearly){

  solid <- collect_solid(T)

  result_monthly <- process_solid_monthly(solid$monthly, pwr_generation)
  result_yearly <- process_solid_yearly(solid$yearly)

  bind_rows(
    result_yearly  %>% split_elec_others() %>% mutate(freq="A"),
    result_monthly %>% split_elec_others()  %>% mutate(freq="M")
  ) %>%
    filter(grepl("Hard coal", siec),
           grepl("Europ", geo),
           ) %>%
    group_by(geo, time=floor_date(time,"year"), siec, sector, fuel, freq) %>%
    summarise(values = sum(values, na.rm=T)) %>%
    ggplot() +
    geom_line(aes(time, values, color=sector, linetype=freq)) +
    facet_wrap(~siec) +
    rcrea::scale_y_crea_zero()


   # Compare sum of countries vs EU
  result_monthly %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
    mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
    group_by(
      siec,
      sector,
      time,
      is_eu) %>%
    summarise(value=sum(values, na.rm=T)) %>%
    ggplot() +
    geom_line(aes(time, value, color=is_eu, linetype=is_eu)) +
    facet_wrap(siec~sector, scales="free_y")


  # Data availability: hard coal stops very early for EU
  result_monthly %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
    filter(!is.na(values)) %>%
    group_by(
      siec,
      sector,
      iso2) %>%
    summarise(max_date=max(time)) %>%
    ggplot() +
    geom_bar(aes(x=max_date, y=iso2, fill=iso2=="EU"), stat='identity') +
    scale_x_date(limits=c(as.Date("2023-01-01"), NA), oob = scales::squish) +
    facet_wrap(~siec+sector)

  # Slovenia is blocking hard coal: can safely be replaced with 0
  # and consider EU is sum of all countries
  # as the power is closely matching
  bind_rows(
    pwr_generation %>%
      filter(source=="Coal", iso2=="SI") %>%
      group_by(time=floor_date(date, "month"), type="coal power") %>%
      summarise(values=sum(value_mwh, na.rm=T)),
    result_monthly %>%
      add_iso2() %>%
      filter(iso2=="SI", sector==SECTOR_ELEC) %>%
      group_by(time, type="solid fuel") %>%
      summarise(values=sum(values, na.rm=T))
  ) %>%
    group_by(type) %>%
    mutate(values=values/values[time=="2020-01-01"]) %>%
    ggplot() +
    geom_line(aes(time, values, color=type))
    # facet_wrap(~type, scales='free_y')

}

investigate_coking_emissions <- function(yearly){

  # Investigating coking gas
  hardcoal_coking <- yearly %>%
    filter(nrg_bal_code=="TI_CO_E") %>%
    add_iso2() %>%
    filter(siec=="Hard coal")


  ggplot(hardcoal_coking) + geom_line(aes(time, values)) + rcrea::scale_y_zero()


  yearly_gas <- get_eurostat_from_code(
    code = "nrg_cb_gas",
    use_cache = F,
    filters = list(nrg_bal = c("IPRD", "IC_CAL", "NRG_CO_E", "NRG_E", "FC", "FC_E"))
  )

  yearly_gas %>%
    filter(siec_code==SIEC_COKE_OVEN_GAS) %>%
    add_iso2() %>%
    filter(iso2=="EU", !is.na(values)) %>%
    filter(grepl("Terajoule", unit)) %>%
    ggplot() + geom_line(aes(time, values, col=nrg_bal_code)) +
    rcrea::scale_y_zero()

  coke_gas <- yearly_gas %>% filter(siec_code==SIEC_COKE_OVEN_GAS, nrg_bal_code=="IPRD", grepl("Terajoule", unit))

  # Compute the ratio of hard coal emissions that should be accounted for
  # to account for coke oven gas (which is not available in monthly data)
  hardcoal_coking_emissions <- get_co2_from_eurostat_cons(hardcoal_coking %>% mutate(fuel=FUEL_COAL, sector=SECTOR_ALL))
  coke_gas_emissions <- get_co2_from_eurostat_cons(coke_gas %>% mutate(fuel=FUEL_GAS, sector=SECTOR_ALL))

  bind_rows(
    hardcoal_coking_emissions,
    coke_gas_emissions
  ) %>%
    filter(iso2 %in% c("DE", "IT", "FR", "EU")) %>%
    select(fuel, iso2, date, value) %>%
    spread(fuel, value) %>%
    mutate(ratio=gas/coal) %>%
    filter(ratio != 0) %>%
    ggplot() + geom_line(aes(date, ratio, col=iso2)) + rcrea::scale_y_zero()

  bind_rows(
    hardcoal_coking_emissions,
    coke_gas_emissions
  ) %>%
    filter(iso2 %in% c("DE", "IT", "FR", "EU")) %>%
    select(fuel, iso2, date, value) %>%
    spread(fuel, value) %>%
    mutate(ratio=gas/coal) %>%
    filter(ratio != 0) %>%
    group_by(iso2) %>%
    summarise(ratio=mean(ratio, na.rm=T))


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
    mutate(sector = if_else(nrg_bal_code==NRG_TRANS_ELEC, SECTOR_ELEC, SECTOR_ALL)) %>%
    # Date valid only from 2014
    filter(time >= "2014-01-01") %>%
    select(iso2, time, siec_code, nrg_bal_code, sector, unit, values)


  view_gaps <- function(x, sector, siec_code){
    times <- unique(x$time)
    x %>%
      filter(sector==!!sector, siec_code==!!siec_code) %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
      ungroup() %>%
      arrange(desc(time)) %>%
      tidyr::complete(
        iso2,
        nesting(nrg_bal_code, siec_code, unit, sector),
        time=times,
        fill=list(values=NA)
      )   %>%
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
    filter((iso2 == "GR" & siec_code == SIEC_BROWN_COAL & time >= "2015-09-01" & nrg_bal_code==NRG_GID_CALCULATED)) %>%
    mutate(sector = SECTOR_ELEC,
           nrg_bal_code = NRG_TRANS_ELEC
           )

  by_sector_fixed <- bind_rows(
      by_sector %>%
        filter(!(iso2 == "GR" & siec_code == SIEC_BROWN_COAL & time >= "2015-09-01" & sector == SECTOR_ELEC)),
      to_add_greece
    )

  # Add the difference to EU
  to_add_to_eu <-  to_add_greece %>%
    mutate(iso2="EU") %>%
    select(iso2, time, siec_code, sector, unit, value_to_add=values)

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
    tolerance = 0.05
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
    summarise(n=n_distinct(siec_code)) %>%
    mutate(keep = n==max(n) | time < max(time) - months(36))

  result <- result %>%
    add_iso2() %>%
    left_join(keep_siec) %>%
    filter(keep) %>%
    select(-keep, -n)

  keep_sector <- result %>%
    ungroup() %>%
    filter(!is.na(values)) %>%
    group_by(iso2, fuel, time) %>%
    summarise(n=n_distinct(sector)) %>%
    mutate(keep = n==max(n) | time < max(time) - months(36))

  result <- result %>%
    add_iso2() %>%
    left_join(keep_sector) %>%
    filter(keep) %>%
    select(-keep, -n)

  # Add siec
  # stopifnot(!"siec" %in% colnames(result))
  # result <- result %>%
  #   ungroup() %>%
  #   left_join(x %>% distinct(siec_code, siec))


  return(result)

}


# process_solid_monthly_old <- function(x, pwr_generation) {
#
#
#     # This one is a bit tricky: for certain months/regions,
#     # EUROSTAT has gross inland deliveries data but no transformation/consumption data
#     # We should make sure to filter out these months so that it's not considered months without coal
#     # consumption
#
#   x %>%
#     distinct(nrg_bal_code, nrg_bal)
#
#     NRG_TRANS_ELEC <- "TI_EHG_MAP"
#     # NRG_FINAL_INDUSTRY <- "FC_IND"
#     # NRG_FINAL_OTHERS <- "FC_OTH"
#     NRG_GID_CALCULATED <- "GID_CAL"
#
#     by_sector <- x %>%
#         filter(
#             (grepl("Transformation input|Final consumption.*(industry sector$|other sectors$)", nrg_bal)) |
#                 # (nrg_bal == "Final consumption - industry sector - iron and steel" & siec == "Coke oven coke")
#                 # Monthly data is much closer to yearly industry consumption when using GID
#                 # See diagnostic plot
#                 (nrg_bal == "Gross inland deliveries - calculated" & siec == "Coke oven coke")
#         ) %>%
#         mutate(
#             sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_OTHERS),
#         ) %>%
#         # Date valid only from 2014
#         filter(time >= "2014-01-01")
#
#     gross_inland_deliv <- x %>%
#         filter(
#             grepl("Gross inland deliveries", nrg_bal),
#             time >= "2014-01-01"
#         ) %>%
#         group_by(nrg_bal = "Gross inland deliveries", geo, time, siec, unit) %>%
#         # I had a look at data. Observed is a better option. Otherwise the ratio_siec varies widely
#         # and excludes data points that shouldn't be excluded
#         arrange(desc(nrg_bal_code)) %>% # observed > calculated
#         summarise(value_siec_gid = first(na.omit(values)), .groups = "drop")
#
#
#     #############################
#     # Apply manual fixes
#     #############################
#     # Greece has started declaring 0 brown coal values for elec starting from 2015-09-01,
#     # but apparently 100% of brown coal was used for elec before that
#     by_sector_fixed <-
#         bind_rows(
#             by_sector %>%
#                 filter(!(geo == "Greece" & siec == "Brown coal" & time >= "2015-09-01" & sector == SECTOR_ELEC)),
#             gross_inland_deliv %>%
#                 filter((geo == "Greece" & siec == "Brown coal" & time >= "2015-09-01")) %>%
#                 mutate(sector = SECTOR_ELEC) %>%
#                 rename(values = value_siec_gid)
#         )
#
#     # Add the difference to EU
#     to_add_to_eu <-  gross_inland_deliv %>%
#       filter((geo == "Greece" & siec == "Brown coal" & time >= "2015-09-01")) %>%
#       mutate(sector = SECTOR_ELEC) %>%
#       rename(values = value_siec_gid) %>%
#       mutate(geo = unique(grep("European Union", by_sector$geo, value=T))) %>%
#       select(geo, time, siec, sector, value_to_add=values)
#
#     by_sector_fixed <- by_sector_fixed %>%
#       left_join(to_add_to_eu) %>%
#       mutate(values = ifelse(is.na(value_to_add), values, values + value_to_add)) %>%
#       select(-value_to_add)
#
#     stopifnot(nrow(by_sector) == nrow(by_sector_fixed))
#
#
#     # Get months without coal power generation to prevent excluding false positive
#     has_coal_generation <- pwr_generation %>%
#         filter(source == "Coal") %>%
#         group_by(iso2, time = floor_date(date, "month")) %>%
#         summarise(value_mwh = sum(value_mwh, na.rm = T)) %>%
#         mutate(
#             value_mwh_threshold = quantile(value_mwh[value_mwh > 0], 0.1), # Could be due to stock changes
#             has_coal_generation = value_mwh > value_mwh_threshold
#         ) %>%
#         ungroup() %>%
#         tidyr::complete(iso2, time = unique(x$time), fill = list(has_coal_generation = FALSE, value_mwh = NA))
#
#
#     threshold_thousand_tonnes <- 100
#
#     valid <- by_sector_fixed %>%
#         group_by(geo, time, siec, sector) %>%
#         summarise(
#             value_siec_sector = sum(values, na.rm = T),
#             has_any_confidential = "c" %in% flags
#         ) %>%
#         mutate(value_siec = sum(value_siec_sector)) %>%
#         left_join(gross_inland_deliv, by = c("geo", "time", "siec")) %>%
#         mutate(ratio_siec = value_siec / value_siec_gid) %>%
#         ungroup() %>%
#         # Attach power generation
#         add_iso2() %>%
#         left_join(has_coal_generation, by = c("iso2", "time")) %>%
#         #############################################################
#         # This is the key criteria: Need value, and not too far from
#         #############################################################
#         group_by(geo, siec, sector) %>%
#         mutate(
#             ratio_siec_threshold_loose = min(0.6, quantile(ratio_siec[ratio_siec > 0], 0.5, na.rm = T)),
#             ratio_siec_threshold_strict = min(0.8, quantile(ratio_siec[ratio_siec > 0], 0.5, na.rm = T))
#         ) %>%
#         mutate(
#             valid =
#             # If no coal power generation, we don't expect any coal consumption in elec sector
#                 (sector == SECTOR_ELEC & !has_coal_generation & value_siec_sector == 0) |
#                     # If elec sector value >0, we assume it is correct (this is generally the others that is wrong)
#                     (sector == SECTOR_ELEC & value_siec_sector > 0) |
#                     # If no coal GID or almost, then fine to have zero as well
#                     (value_siec_gid <= threshold_thousand_tonnes) |
#                     # When has confidential, needs to be quite strict
#                     (has_any_confidential & (ratio_siec >= ratio_siec_threshold_strict)) |
#                     # If not, can be a bit more relax
#                     (!has_any_confidential & (ratio_siec >= ratio_siec_threshold_loose))
#         )
#
#     # Then ignore dates that are only invalid for a single month (unless has_confidentials changed to False)
#     valid <- valid %>%
#         group_by(geo, siec, sector) %>%
#         arrange(time) %>%
#         mutate(valid = case_when(
#             valid ~ valid,
#             has_any_confidential & lag(has_any_confidential, default = FALSE) ~ valid,
#             T ~ valid | (lag(valid, default = FALSE) & lead(valid, default = FALSE))
#         )) %>%
#         ungroup()
#
#     # Print what we're removing
#     removed <- valid %>%
#         filter(!valid) %>%
#         group_by(siec, sector) %>%
#         summarise(removed = n())
#
#     message(glue("Removed invalid coal records from EUROSTAT"))
#     print(as.data.frame(removed))
#
#     # Everytime we remove a record from a EU country, we should also remove the corresponding one
#     # at the EU level
#     valid <- valid %>%
#       add_iso2() %>%
#       group_by(time, siec, sector) %>%
#       mutate(valid = case_when(grepl("European", geo) ~ all(valid[iso2 %in% get_eu_iso2s(include_eu=T)]),
#                                T ~ valid)) %>%
#       ungroup()
#
#     valid_by_sector <- by_sector_fixed %>%
#         inner_join(
#             valid %>%
#                 filter(valid) %>%
#                 select(geo, time, siec, sector)
#         ) %>%
#       mutate(fuel = ifelse(siec == "Coke oven coke", FUEL_COKE, FUEL_COAL))
#
#     # We add total coal so that we can deduct coal - others after projecting coal - elec using ENTSOE
#     # WARNING: It means we'll have the three sectors for coal: SECTOR_ALL, SECTOR_ELEC, SECTOR_OTHERS
#     # i.e. there'll be some double counting we need to be aware of
#     total <- gross_inland_deliv %>%
#         filter(siec != "Coke oven coke") %>%
#         mutate(sector = SECTOR_ALL, fuel = FUEL_COAL) %>%
#         rename(values = value_siec_gid) %>%
#         ungroup()
#
#     # Plot heat map of validity
#     # ggplot(valid_by_sector) +
#     #   geom_tile(aes(time, geo, fill = valid), color=NA) +
#     #   facet_wrap(sector~siec) +
#     #   scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "grey", "NA" = "black")) +
#     #   theme_minimal() +
#     #   theme(
#     #     panel.grid = element_blank(),
#     #     panel.background = element_blank(),
#     #     axis.ticks = element_blank(),
#     #     # axis.text = element_blank(),
#     #     axis.title = element_blank()
#     #   )
#
#     # Return only valid records
#     result <- bind_rows(
#         valid_by_sector,
#         total
#     )
#
#     # Debug plot
#     plt_data <- result %>%
#       add_iso2() %>%
#       filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
#       {
#         n_countries <- n_distinct(.$iso2)
#         print(n_countries)
#         stopifnot('Expected 27 EU countries + EU'= n_countries == 28)
#         .
#       } %>%
#       mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
#       filter(time < "2025-01-01") %>%
#       filter(time >= "2015-01-01") %>%
#       group_by(
#         siec,
#         sector,
#         time,
#         is_eu) %>%
#       summarise(value=sum(values, na.rm=T)) %>%
#       arrange(desc(time))
#
#     plt_data %>%
#       ggplot() +
#       geom_line(aes(time, value, color=is_eu, linetype=is_eu)) +
#       facet_wrap(siec~sector, scales="free_y") +
#       theme_minimal() +
#       theme(
#         legend.position = "none"
#       )
#
#     return(result)
# }

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

  # Debug plot
  # x %>%
  #   filter(nrg_bal_code %in% c(
  #     NRG_FINAL_ENERGY,
  #     NRG_TRANS_ENERGY,
  #     NRG_ELEC_CHP,
  #     NRG_ELEC_ONLY,
  #     NRG_TRANS_COKING) |
  #       (nrg_bal_code == NRG_FINAL_IRON_STEEL & siec_code == SIEC_COKE_OVEN_COKE)) %>%
  #   add_iso2() %>%
  #   # filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
  #   filter(iso2=="IT") %>%
  #   filter(time < "2025-01-01") %>%
  #   filter(time >= "2015-01-01") %>%
  #   ggplot() +
  #   geom_line(aes(time, values, color=nrg_bal_code)) +
  #   facet_wrap(~siec)
  #
  # plt_data %>%
  #   ggplot() +
  #   geom_line(aes(time, value, color=is_eu, linetype=is_eu)) +
  #   facet_wrap(siec~sector, scales="free_y") +
  #   theme_minimal() +
  #   theme(
  #     legend.position = "none"
  #   )
  #
  # plt_data <- result %>%
  #   add_iso2() %>%
  #   # filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
  #   filter(iso2=="IT") %>%
  #   filter(time < "2025-01-01") %>%
  #   filter(time >= "2015-01-01") %>%
  #   group_by(
  #     siec,
  #     sector,
  #     time,
  #     is_eu) %>%
  #   summarise(value=sum(values, na.rm=T)) %>%
  #   arrange(desc(time))
  #
  # plt_data %>%
  #   ggplot() +
  #   geom_line(aes(time, value, color=is_eu, linetype=is_eu)) +
  #   facet_wrap(siec~sector, scales="free_y") +
  #   theme_minimal() +
  #   theme(
  #     legend.position = "none"
  #   )

  return(result)

}


