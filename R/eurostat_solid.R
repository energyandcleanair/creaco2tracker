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
      "C0200", # Brown coal
      # "C0220", # Lignite
      "C0100", # Hard coal
      # "C0129", # Other bituminous coal
      # "C0121", # Coking coal
      "C0311", # Coke oven coke
      "C0330" # Brown coal briquettes
      # "C0110"  # Anthracite
    )

    filter_siec <- function(x) filter(x, siec_code %in% siec_codes)

    list(
        monthly = filter_siec(cons_monthly_raw),
        yearly = filter_siec(cons_yearly_raw)
    )
}



investigate_solid <- function(monthly, yearly){


  NRG_TRANS_ELEC <- "TI_EHG_MAP"
  NRG_FINAL_INDUSTRY <- "FC_IND"
  NRG_FINAL_OTHERS <- "FC_OTH"
  NRG_GID_CALCULATED <- "GID_CAL"

  result_yearly <- yearly %>%
    filter(nrg_bal %in% c(
      "Final consumption - energy use",
      "Transformation input - energy use",
      "Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use",
      "Transformation input - electricity and heat generation - main activity producer electricity only - energy use"
    ) |
      (nrg_bal == "Final consumption - industry sector - iron and steel" & siec == "Coke oven coke")) %>%
    mutate(
      sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL),
      fuel = ifelse(siec == "Coke oven coke", FUEL_COKE, FUEL_COAL)
    )


  # result_monthly <- monthly %>%
  #   filter(
  #     # (siec=="Hard coal" &nrg_bal_code %in% c(NRG_TRANS_ELEC, NRG_FINAL_INDUSTRY, NRG_FINAL_OTHERS))
  #     (siec=="Hard coal" &nrg_bal_code %in% c(NRG_GID_CALCULATED, NRG_TRANS_ELEC))
  #     | (siec=="Coke oven coke" & nrg_bal_code==NRG_GID_CALCULATED)
  #     | (siec=="Brown coal" & nrg_bal_code %in% c(NRG_GID_CALCULATED, NRG_TRANS_ELEC))
  #     # nrg_bal_code==NRG_GID_CALCULATED
  #     ) %>%
  #   mutate( sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL))
  #
  # # result_monthly <- bind_rows(
  # #   result_monthly %>% mutate(sector="all"),
  # #   result_monthly%>% filter(nrg_bal_code==NRG_TRANS_ELEC) %>% mutate(sector="electricity")
  # # )
  result_monthly <- process_solid_monthly(monthly, pwr_demand)

  bind_rows(
    result_yearly %>% mutate(freq="A"),
    result_monthly %>% mutate(freq="M")
  ) %>%
    filter(grepl("Poland", geo)) %>%
    group_by(geo, time=floor_date(time,"year"), siec, sector, fuel, freq) %>%
    summarise(values = sum(values, na.rm=T)) %>%
    ggplot() +
    geom_line(aes(time, values, color=sector, linetype=freq)) +
    facet_wrap(~siec)


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
    pwr_demand %>%
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

process_solid_monthly <- function(x, pwr_demand) {


  # This one is a bit tricky: for certain months/regions,
  # EUROSTAT has gross inland deliveries data but no transformation/consumption data
  # We should make sure to filter out these months so that it's not considered months without coal
  # consumption
  NRG_TRANS_ELEC <- "TI_EHG_MAP"
  NRG_GID_CALCULATED <- "GID_CAL"

  by_sector <- x %>%
    filter(nrg_bal_code %in% c(NRG_GID_CALCULATED, NRG_TRANS_ELEC)) %>%
    mutate(sector = if_else(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL)) %>%
    # Date valid only from 2014
    filter(time >= "2014-01-01")


  #############################
  # Apply manual fixes
  #############################
  # Greece has started declaring 0 brown coal values for elec starting from 2015-09-01,
  # but apparently 100% of brown coal was used for elec before that
  to_add_greece <- by_sector %>%
    filter((geo == "Greece" & siec == "Brown coal" & time >= "2015-09-01" & nrg_bal_code==NRG_GID_CALCULATED)) %>%
    mutate(sector = SECTOR_ELEC)

  by_sector_fixed <-
    bind_rows(
      by_sector %>%
        filter(!(geo == "Greece" & siec == "Brown coal" & time >= "2015-09-01" & sector == SECTOR_ELEC)),
      to_add_greece
    )

  # Add the difference to EU
  to_add_to_eu <-  to_add_greece %>%
    mutate(geo = unique(grep("European Union", by_sector$geo, value=T))) %>%
    select(geo, time, siec, sector, value_to_add=values)

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
  ##########################
  # max_date_before <- by_sector_fixed %>%
  #   filter(sector == SECTOR_ELEC, siec == "Hard coal", grepl("European", geo)) %>%
  #   summarise(max_date=max(time)) %>%
  #   pull(max_date)
  # message(glue("[BEFORE] EU last hard coal elec: {max_date_before}"))

  by_sector_fixed <- by_sector_fixed %>%
    tidyr::complete(geo="Slovenia",
                    time,
                    siec="Hard coal",
                    unit,
                    sector="electricity",
                    fill=list(values=0))

  hard_coal_elec_eu_from_countries <- by_sector_fixed %>%
    filter(
      sector == SECTOR_ELEC,
      siec == "Hard coal",
    ) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = F)) %>%
    group_by(sector, siec, time, unit) %>%
    summarise(values_sum_countries=sum(values, na.rm=T),
              n=n()) %>%
    filter(n==27)

  hard_coal_elec_eu <- by_sector_fixed  %>%
    filter(grepl("European", geo),
           sector == SECTOR_ELEC,
           siec == "Hard coal") %>%
    tidyr::complete(geo, sector, siec, time=hard_coal_elec_eu_from_countries$time, unit) %>%
    left_join(hard_coal_elec_eu_from_countries) %>%
    {
      # Check that when both values exist they're similar
      comparison <- filter(., !is.na(values), !is.na(values_sum_countries))
      stopifnot(all(abs(comparison$values - comparison$values_sum_countries) < 1))
      .
    } %>%
    mutate(values = coalesce(values, values_sum_countries))


    # Replace
  by_sector_fixed <- bind_rows(
    by_sector_fixed %>% filter(!(grepl("European", geo) & siec == "Hard coal" & sector == SECTOR_ELEC)),
    hard_coal_elec_eu
  ) %>%
    mutate(fuel = ifelse(siec == "Coke oven coke", FUEL_COKE, FUEL_COAL))

  # max_date_after <- by_sector_fixed %>%
  #   filter(sector == SECTOR_ELEC, siec == "Hard coal", grepl("European", geo)) %>%
  #   summarise(max_date=max(time)) %>%
  #   pull(max_date)
  # message(glue("[AFTER] EU last hard coal elec: {max_date_after}"))

    return(by_sector_fixed)

}


# process_solid_monthly_old <- function(x, pwr_demand) {
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
#     has_coal_generation <- pwr_demand %>%
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

  result <- x %>%
        filter(nrg_bal %in% c(
          "Final consumption - energy use",
          "Transformation input - energy use",
          "Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use",
          "Transformation input - electricity and heat generation - main activity producer electricity only - energy use"
        ) |
            (nrg_bal == "Final consumption - industry sector - iron and steel" & siec == "Coke oven coke")) %>%
        mutate(
            sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL),
            fuel = ifelse(siec == "Coke oven coke", FUEL_COKE, FUEL_COAL)
        )

  # # Debug plot
  # plt_data <- result %>%
  #   add_iso2() %>%
  #   filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
  #   {
  #     n_countries <- n_distinct(.$iso2)
  #     print(n_countries)
  #     stopifnot('Expected 27 EU countries + EU'= n_countries == 28)
  #     .
  #   } %>%
  #   mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
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


