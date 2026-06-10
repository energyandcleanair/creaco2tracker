# These functions are used to investigate Eurostat data gaps
# and devise heuristics to fill them.
# These are not used in the main code

investigate_oil <- function(monthly_oil, monthly_filled_oil, yearly_oil, yearly_filled_oil) {
  # International shipping EU vs countries
  monthly_oil %>%
    filter(nrg_bal %in% c("INTMARB") &
      siec %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_DIESEL_XBIO)) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = TRUE)) %>%
    mutate(is_eu = iso2 == "EU") %>%
    group_by(is_eu, time, siec) %>%
    summarise(values = sum(values, na.rm = TRUE), n = n()) %>%
    arrange(desc(time)) %>%
    ggplot() +
    geom_line(aes(time, values, col = is_eu)) +
    facet_wrap(~siec)

  monthly_oil %>%
    filter(nrg_bal %in% c("INTMARB") &
      siec %in% c(SIEC_FUEL_OIL)) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = TRUE)) %>%
    select(iso2, time, values) %>%
    spread(iso2, values) %>%
    arrange(desc(time)) %>%
    View()


  # Discrepancy monthly - yearly
  bind_rows(
    # monthly_oil,
    fill_oil_non_energy_use_monthly(yearly_filled_oil, monthly_oil) %>% mutate(
      freq =
        "Monthly filled"
    ),
    yearly_filled_oil
  ) %>%
    add_iso2() %>%
    filter(
      iso2 == "AT",
      nrg_bal %in% c(
        "GID_OBS",
        # "GD_PI",
        "GID_NE"
      )
    ) %>%
    group_by(
      siec,
      siec,
      nrg_bal,
      freq,
      year = year(time)
    ) %>%
    summarise(
      values = sum(values, na.rm = TRUE),
      n = n()
    ) %>%
    filter(n == 12 | freq == "Annual") %>%
    # Reproduce same as process
    filter(
      # Oil products: SECTOR_TOTAL
      (nrg_bal %in% c("GID_OBS", "GID_NE") &
        siec %in% c(
          SIEC_OIL_PRODUCTS, SIEC_FUEL_OIL, SIEC_HEATING_GASOIL,
          SIEC_BIOGASOLINE, SIEC_BIODIESEL
        )) |

        # SECTOR_TRANSPORT: Diesel and Gasoline
        (nrg_bal %in% c("FC_TRA_E") # Added in add_oil_transport function
        & siec %in% c(SIEC_ROAD_DIESEL, SIEC_MOTOR_GASOLINE_XBIO)) |

        # SECTOR_TRANSPORT: Kerosene
        (
          nrg_bal %in% c(
            "FC_TRA_DAVI_E", "INTAVI_E",
            "INTAVI_E+FC_TRA_DAVI_E"
          ) # Added in add_oil_transport function
          & siec %in% c(SIEC_KEROSENE_XBIO, SIEC_AVIATION_GASOLINE))

      # SECTOR_TRANSPORT: International maritime bunkers
      # (nrg_bal %in% c("INTMARB")
      #  & siec %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_DIESEL_XBIO))
    ) %>%
    ggplot() +
    geom_line(aes(year, values, col = freq, linetype = nrg_bal)) +
    facet_wrap(
      ~siec,
    ) +
    rcrea::scale_y_zero()


  # Investigate Netherlands oil products
  plt <- bind_rows(
    yearly_filled_oil %>% mutate(freq = "Annual filled"),
    yearly_oil,
    monthly_oil
  ) %>%
    add_iso2() %>%
    filter(
      iso2 == "NL",
      siec == SIEC_OIL_PRODUCTS
    ) %>%
    group_by(freq, year = year(time), siec, nrg_bal) %>%
    summarise(
      values = sum(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot() +
    geom_line(aes(year, values, col = freq)) +
    facet_wrap(~nrg_bal) +
    theme(
      # Hide yaxis labels
      axis.text.y = element_blank()
    )


  ggsave(
    plot = plt,
    filename = "investigate/investigate_netherlands.png",
    height = 12, width = 18
  )
}

investigate_coking_france <- function(monthly_solid, indprod) {
  monthly_solid %>%
    add_iso2() %>%
    select(iso2, time, values, siec, nrg_bal) %>%
    filter(iso2 == "FR", nrg_bal %in% c("TI_CO", "GID_OBS", "TI_EHG_MAP")) %>%
    spread(nrg_bal, values) %>%
    mutate(RATIO = TI_CO / (GID_OBS - TI_EHG_MAP)) %>%
    ggplot() +
    geom_line(aes(time, RATIO, col = siec)) +
    rcrea::scale_y_zero()

  monthly_solid %>%
    add_iso2() %>%
    select(iso2, time, values, siec, nrg_bal) %>%
    filter(iso2 == "FR", nrg_bal %in% c("TI_CO", "GID_OBS", "TI_EHG_MAP")) %>%
    ggplot() +
    geom_line(aes(time, values, col = nrg_bal)) +
    facet_wrap(~siec)


  coke_fr <- get_eurostat_indprod(iso2s = "FR")
  coke_fr %>% filter(nace_r2 == "191")

  indprod %>%
    filter(geo == "France") %>%
    filter(grepl("coke", nace_r2, ignore.case = TRUE)) %>%
    distinct(nace_r2)
}

investigate_solid <- function(monthly_solid, yearly_solid) {
  solid <- collect_solid(TRUE)

  result_monthly <- process_solid_monthly(solid$monthly, pwr_generation)
  result_yearly <- process_solid_yearly(solid$yearly)

  bind_rows(
    result_yearly %>% split_elec_others() %>% mutate(freq = "A"),
    result_monthly %>% split_elec_others() %>% mutate(freq = "M")
  ) %>%
    filter(
      grepl("Hard coal", siec),
      grepl("Europ", geo),
    ) %>%
    group_by(geo, time = floor_date(time, "year"), siec, sector, fuel, freq) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    ggplot() +
    geom_line(aes(time, values, color = sector, linetype = freq)) +
    facet_wrap(~siec) +
    rcrea::scale_y_crea_zero()


  # Compare sum of countries vs EU
  result_monthly %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = TRUE)) %>%
    mutate(is_eu = case_when(iso2 == "EU" ~ "EU", TRUE ~ "EU member states")) %>%
    group_by(
      siec,
      sector,
      time,
      is_eu
    ) %>%
    summarise(value = sum(values, na.rm = TRUE)) %>%
    ggplot() +
    geom_line(aes(time, value, color = is_eu, linetype = is_eu)) +
    facet_wrap(siec ~ sector, scales = "free_y")


  # Data availability: hard coal stops very early for EU
  result_monthly %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = TRUE)) %>%
    filter(!is.na(values)) %>%
    group_by(
      siec,
      sector,
      iso2
    ) %>%
    summarise(max_date = max(time)) %>%
    ggplot() +
    geom_bar(aes(x = max_date, y = iso2, fill = iso2 == "EU"), stat = "identity") +
    scale_x_date(limits = c(as.Date("2023-01-01"), NA), oob = scales::squish) +
    facet_wrap(~ siec + sector)

  # Slovenia is blocking hard coal: can safely be replaced with 0
  # and consider EU is sum of all countries
  # as the power is closely matching
  bind_rows(
    pwr_generation %>%
      filter(source == "Coal", iso2 == "SI") %>%
      group_by(time = floor_date(date, "month"), type = "coal power") %>%
      summarise(values = sum(value_mwh, na.rm = TRUE)),
    result_monthly %>%
      add_iso2() %>%
      filter(iso2 == "SI", sector == SECTOR_ELEC) %>%
      group_by(time, type = "solid fuel") %>%
      summarise(values = sum(values, na.rm = TRUE))
  ) %>%
    group_by(type) %>%
    mutate(values = values / values[time == "2020-01-01"]) %>%
    ggplot() +
    geom_line(aes(time, values, color = type))
}

investigate_coking_emissions <- function(yearly_solid) {
  # Investigating coking gas
  hardcoal_coking <- yearly_solid %>%
    filter(nrg_bal == "TI_CO_E") %>%
    add_iso2() %>%
    filter(siec == "Hard coal")


  ggplot(hardcoal_coking) +
    geom_line(aes(time, values)) +
    rcrea::scale_y_zero()


  yearly_gas <- get_eurostat_from_code(
    code = "nrg_cb_gas",
    use_cache = TRUE,
    filters = list(siec = "Coke oven gas")
  )

  coke_gas <- yearly_gas %>%
    filter(nrg_bal == "GID_OBS") %>%
    add_iso2()

  # Compute the ratio of hard coal emissions that should be accounted for
  # to account for coke oven gas (which is not available in monthly data)
  hardcoal_coking_emissions <- get_co2_from_eurostat_cons(
    hardcoal_coking %>% mutate(
      fuel =
        FUEL_COAL, sector = SECTOR_ALL
    )
  )
  coke_gas_emissions <- get_co2_from_eurostat_cons(
    coke_gas %>% mutate(
      fuel = FUEL_GAS, sector =
        SECTOR_ALL
    )
  )

  bind_rows(
    hardcoal_coking_emissions,
    coke_gas_emissions
  ) %>%
    filter(iso2 %in% c("DE", "IT", "FR", "EU")) %>%
    select(fuel, iso2, date, value) %>%
    spread(fuel, value) %>%
    mutate(ratio = gas / coal) %>%
    filter(ratio != 0) %>%
    ggplot() +
    geom_line(aes(date, ratio, col = iso2)) +
    rcrea::scale_y_zero()

  bind_rows(
    hardcoal_coking_emissions,
    coke_gas_emissions
  ) %>%
    filter(iso2 %in% c("DE", "IT", "FR", "EU")) %>%
    select(fuel, iso2, date, value) %>%
    spread(fuel, value) %>%
    mutate(ratio = gas / coal) %>%
    filter(ratio != 0) %>%
    group_by(iso2) %>%
    summarise(ratio = mean(ratio, na.rm = TRUE))
}
