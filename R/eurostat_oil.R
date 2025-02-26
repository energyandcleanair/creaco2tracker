# SIEC codes constants
SIEC_OIL_PRODUCTS <- "O4600"
SIEC_CRUDE_OIL <- "O4100_TOT_4200-4500" # Crude oil, NGL, refinery feedstocks
SIEC_ROAD_DIESEL <- "O46711"
SIEC_MOTOR_GASOLINE <- "O4652"
SIEC_GASOIL_DIESEL_XBIO <- "O4671XR5220B"
SIEC_HEATING_GASOIL <- "O46712"
SIEC_FUEL_OIL <- "O4680"
SIEC_KEROSENE_XBIO <- "O4661XR5230B"
SIEC_AVIATION_GASOLINE <- "O4651"
SIEC_BIOGASOLINE <- "R5210B"
SIEC_BIODIESEL <- "R5220B"


#' Collect oil consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @return Raw oil consumption data from EUROSTAT
#' @export
collect_oil <- function(use_cache = FALSE) {

  # Monthly data
  cons_monthly_raw <- get_eurostat_from_code("nrg_cb_oilm", use_cache = use_cache)

  # Yearly data
  cons_yearly_raw <- lapply(
    c(SIEC_OIL_PRODUCTS,
      SIEC_CRUDE_OIL,
      SIEC_ROAD_DIESEL,
      SIEC_MOTOR_GASOLINE,
      SIEC_HEATING_GASOIL,
      SIEC_FUEL_OIL,
      SIEC_GASOIL_DIESEL_XBIO,
      SIEC_KEROSENE_XBIO,
      SIEC_AVIATION_GASOLINE,
      SIEC_BIOGASOLINE,
      SIEC_BIODIESEL
    ),
    function(x) {
      get_eurostat_from_code(
        code = "nrg_cb_oil",
        use_cache = use_cache,
        filters = list(siec = x)
      )
    }
  ) %>%
    bind_rows()

  # Add missing GID_NE when it happens
  cons_yearly <- fill_oil_non_energy_use_yearly(cons_yearly_raw)

  # And GID_NE
  cons_monthly_filled <- fill_oil_non_energy_use_monthly(
    yearly = cons_yearly,
    monthly = cons_monthly_raw
  )

  # Add Oil transport
  cons_monthly_filled <- add_oil_transport(
    monthly = cons_monthly_filled,
    yearly = cons_yearly
  )

  list(
    monthly = cons_monthly_filled,
    yearly = cons_yearly
  )
}

investigate_oil <- function(cons_monthly_raw, cons_monthly_filled, cons_yearly){


  # International shipping EU vs countries
  cons_monthly_raw %>%
    filter(nrg_bal_code %in% c("INTMARB")
           & siec_code %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_DIESEL_XBIO)) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
    mutate(is_eu=iso2=="EU") %>%
    group_by(is_eu, time, siec_code) %>%
    summarise(values=sum(values, na.rm=T), n=n())  %>%
    arrange(desc(time)) %>%
    ggplot() +
    geom_line(aes(time, values, col=is_eu)) +
    facet_wrap(~siec_code)

  cons_monthly_raw %>%
    filter(nrg_bal_code %in% c("INTMARB")
           & siec_code %in% c(SIEC_FUEL_OIL)) %>%
    add_iso2() %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
    select(iso2, time, values) %>%
    spread(iso2, values) %>%
    arrange(desc(time)) %>%
    View()



  # Discrepancy monthly - yearly
  bind_rows(
    # cons_monthly_raw,
    fill_oil_non_energy_use_monthly(cons_yearly, cons_monthly_raw) %>% mutate(freq="Monthly filled"),
    cons_yearly) %>%
    add_iso2() %>%
    filter(
           iso2=="AT",
           nrg_bal_code %in% c("GID_OBS",
                               # "GD_PI",
                               "GID_NE")
           ) %>%
    group_by(
      siec_code,
      siec,
      nrg_bal_code,
      freq,
      year=year(time)
    ) %>%
    summarise(values=sum(values, na.rm=T),
              n=n()
    ) %>%
    filter(n==12 | freq=='Annual') %>%
    # Reproduce same as process
    filter(

      # Oil products: SECTOR_TOTAL
      (nrg_bal_code %in% c("GID_OBS", "GID_NE")
       & siec_code %in% c(SIEC_OIL_PRODUCTS, SIEC_FUEL_OIL, SIEC_HEATING_GASOIL,
                          SIEC_BIOGASOLINE, SIEC_BIODIESEL)) |

        # SECTOR_TRANSPORT: Diesel and Gasoline
        (nrg_bal_code %in% c("FC_TRA_E") # Added in add_oil_transport function
         & siec_code %in% c(SIEC_ROAD_DIESEL, SIEC_MOTOR_GASOLINE)) |

        # SECTOR_TRANSPORT: Kerosene
        (nrg_bal_code %in% c("FC_TRA_DAVI_E", "INTAVI_E", "INTAVI_E+FC_TRA_DAVI_E") # Added in add_oil_transport function
         & siec_code %in% c(SIEC_KEROSENE_XBIO, SIEC_AVIATION_GASOLINE)) |

        # SECTOR_TRANSPORT: International maritime bunkers
        (nrg_bal_code %in% c("INTMARB")
         & siec_code %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_DIESEL_XBIO))
    ) %>%

    ggplot() +
    geom_line(aes(year, values, col=freq, linetype=nrg_bal_code)) +
    facet_wrap(~siec,
               # scales='free_y'
               ) +
    rcrea::scale_y_zero()

}



#' Process oil data from EUROSTAT
#'
#' @param x Raw EUROSTAT data
#' @return Processed oil consumption data
#' @export
process_oil <- function(x) {

  # Only keep what interests us
  x <- x %>% filter(

    # Oil products: SECTOR_TOTAL
    (nrg_bal_code %in% c("GID_OBS", "GID_NE")
    & siec_code %in% c(SIEC_OIL_PRODUCTS, SIEC_FUEL_OIL, SIEC_HEATING_GASOIL,
                    SIEC_BIOGASOLINE, SIEC_BIODIESEL)) |

    # SECTOR_TRANSPORT: Diesel and Gasoline
    (nrg_bal_code %in% c("FC_TRA_E") # Added in add_oil_transport function
     & siec_code %in% c(SIEC_ROAD_DIESEL, SIEC_MOTOR_GASOLINE)) |

    # SECTOR_TRANSPORT: Kerosene
    (nrg_bal_code %in% c("FC_TRA_DAVI_E", "INTAVI_E", "INTAVI_E+FC_TRA_DAVI_E") # Added in add_oil_transport function
     & siec_code %in% c(SIEC_KEROSENE_XBIO, SIEC_AVIATION_GASOLINE)) |

    # SECTOR_TRANSPORT: International maritime bunkers
    (nrg_bal_code %in% c("INTMARB")
     & siec_code %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_DIESEL_XBIO))
  )



  # We have separated fuel oil from oil products as it has a significantly higher
  # emission factor. To avoid double counting, we substract fuel oil from oil products.

  # We also remove biofuels from oil products (assuming co2=0) and remove them afterwards
  x <- x %>%
    group_by(nrg_bal, nrg_bal_code, freq, unit, geo, time) %>%
    mutate(values = dplyr::if_else(siec_code==SIEC_OIL_PRODUCTS,
                                   values - sum(values[siec_code %in% c(SIEC_FUEL_OIL,
                                                                        SIEC_HEATING_GASOIL,
                                                                        SIEC_BIOGASOLINE,
                                                                        SIEC_BIODIESEL)]),
                                   values)) %>%
    ungroup() %>%
    filter(!siec_code %in% c(SIEC_BIOGASOLINE, SIEC_BIODIESEL))


  mult <- x %>%
    ungroup() %>%
    distinct(siec_code, nrg_bal, nrg_bal_code) %>%
    mutate(factor = case_when(
      nrg_bal_code == "GID_NE" ~ -1,
      T ~ 1
    ),
    sector = case_when(
      grepl("INTMARB|AVI_E|FC_TRA_E", nrg_bal_code) ~ SECTOR_TRANSPORT,
      T ~ SECTOR_ALL
    )
    ) %>%
    select(-c(nrg_bal))

  # Need three for all and one or two for transport
  stopifnot("Fix oil"=nrow(mult[mult$sector==SECTOR_ALL,]) == 6,
            "Fix oil"=nrow(mult[mult$sector==SECTOR_TRANSPORT,]) == 8,
            "Fix oil"=base::setequal(mult$factor, c(1,-1)))

  x %>%
    inner_join(mult) %>%
    mutate(
      values = values * factor,
      fuel = FUEL_OIL
    ) %>%
    select(-c(factor)) %>%
    arrange(desc(time)) %>%
    filter(!is.na(values)) %>%
    # After validation, we find that transport data is only correct from ~2010
    filter(
      sector == SECTOR_ALL | time >= "2010-01-01"
    )
}

#' Process monthly oil data
#' @param x Raw monthly oil data
#' @return Processed monthly oil consumption
#' @export
process_oil_monthly <- function(x) {
  process_oil(x)
}

#' Process yearly oil data
#' @param x Raw yearly oil data
#' @return Processed yearly oil consumption
#' @export
process_oil_yearly <- function(x) {
  process_oil(x)
}


#' Monthly data is not as detailed as yearly one.
#' We make and test some assumptions to fill the gaps (see in code)
#'
#'
#'
#' @param cons_yearly_raw
#' @param cons_monthly_raw
#'
#' @return
#' @export
#'
#' @examples
add_oil_transport <- function(monthly, yearly) {

  eu_iso2s <- get_eu_iso2s(include_eu = T)
  filter_plot <- function(x) {
    x %>% add_iso2() %>% filter(iso2 %in% eu_iso2s) %>% filter(time >= "2010-01-01")
  }
  stopifnot(all(yearly$unit == "Thousand tonnes"))
  stopifnot(all(monthly$unit == "Thousand tonnes"))

  project_shares <- function(x){
    x %>%
      ungroup() %>%
      tidyr::complete(time = unique(monthly$time), geo, siec_code) %>%
      group_by(geo, siec_code) %>%
      arrange(time) %>%
      tidyr::fill(share, .direction = c("updown")) %>%
      ungroup()
  }

  # Visually validate computation
  validate_computation <- function(computed_monthly){
    computed_monthly %>%
      group_by(time=floor_date(time, "year"), geo, nrg_bal_code, siec_code) %>%
      summarise(values_monthly = sum(values, na.rm=T),
                n=n()
      ) %>%
      filter(n==12) %>%
      # split and unnest nrg_bal_code
      mutate(nrg_bal_code = strsplit(nrg_bal_code, "\\+")) %>%
      unnest(nrg_bal_code) %>%
      # Prevent double counting
      group_by(time, geo, siec_code) %>%
      mutate(values_monthly = values_monthly / n()) %>%
      left_join(
        yearly %>%
          rename(values_yearly=values)
      ) %>%
      ungroup() %>%
      group_by(time, geo) %>%
      summarise(values_monthly = sum(values_monthly, na.rm=T),
                values_yearly = sum(values_yearly, na.rm=T)
      ) %>%
      gather(key, values, -time, -geo) %>%
      ungroup() %>%
      filter_plot %>%
      ggplot() +
      geom_line(aes(time, values, col=key)) +
      facet_wrap(~geo, scales='free_y') +
      rcrea::scale_y_zero()
  }


  # Assumption 1: Constant share of motor gasoline (that doesn't go to petro industry) goes to road energy use

  # Compute share
  share_motor_gasoline_road <- yearly %>%
    filter(siec_code == SIEC_MOTOR_GASOLINE,
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_ROAD_E", "GID_NE")) %>%
    group_by(geo, time, siec_code, nrg_bal_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS - GID_NE))

  # Visually validate assumption
  ggplot(share_motor_gasoline_road %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_road_e_gasoline <- share_motor_gasoline_road %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_MOTOR_GASOLINE,
               nrg_bal_code %in% c("GID_OBS", "GID_NE")) %>%
        mutate(factor = case_when(
          nrg_bal_code == "GID_NE" ~ -1,
          T ~ 1
        )) %>%
        group_by(geo, time, siec_code) %>%
        summarise(values=sum(values * factor, na.rm=T)),
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  validate_computation(monthly_tra_road_e_gasoline)


  # Assumption 2: Constant share of road diesel goes to road energy use
  share_road_diesel_road <- yearly %>%
    filter(siec_code == SIEC_ROAD_DIESEL,
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_ROAD_E")) %>%
    group_by(geo, time, nrg_bal_code, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS))

  ggplot(share_road_diesel_road %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_road_e_diesel <- share_road_diesel_road %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_ROAD_DIESEL,
               nrg_bal_code %in% c("GID_OBS")) %>%
        group_by(geo, time, siec_code) %>%
        summarise(values=sum(values, na.rm=T)),
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_road_e_diesel)


  # # TEMP
  # bind_rows(
  #   monthly %>% mutate(source='monthly'),
  #   yearly %>% mutate(source='yearly')
  # ) %>%
  #   filter(siec=="Road diesel",
  #          nrg_bal_code=="GID_OBS",
  #          grepl("Europe", geo)
  #          ) %>%
  #   group_by(geo, time=floor_date(time, "year"), source) %>%
  #   summarise(values=sum(values, na.rm=T),
  #             n=n()) %>% arrange(time) %>%
  #   filter_plot %>%
  #   ggplot() +
  #   geom_line(aes(time, values, col=source)) +
  #   rcrea::scale_y_zero()


  # Assumption 2bis: Constant share of road diesel goes to transportation energy use
  share_road_diesel_transport <- yearly %>%
    filter(siec_code == SIEC_ROAD_DIESEL,
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_E")) %>%
    group_by(geo, time, nrg_bal_code, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = FC_TRA_E / (GID_OBS))

  ggplot(share_road_diesel_transport %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_e_diesel <- share_road_diesel_transport %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_ROAD_DIESEL,
               nrg_bal_code %in% c("GID_OBS")) %>%
        select(geo, time, siec_code, values),
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_e_diesel)


  # Assumption 2ter: Constant share of motor gasoline goes to transport energy use
  share_motor_gasoline_tra <- yearly %>%
    filter(siec_code == SIEC_MOTOR_GASOLINE,
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_E", "GID_NE")) %>%
    group_by(geo, time, siec_code, nrg_bal_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = FC_TRA_E / (GID_OBS - GID_NE))

  # Visually validate assumption
  ggplot(share_motor_gasoline_tra %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_e_gasoline <- share_motor_gasoline_tra %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_MOTOR_GASOLINE,
               nrg_bal_code %in% c("GID_OBS", "GID_NE")) %>%
        mutate(factor = case_when(
          nrg_bal_code == "GID_NE" ~ -1,
          T ~ 1
        )) %>%
        group_by(geo, time, siec_code) %>%
        summarise(values=sum(values * factor, na.rm=T)),
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  # Visually validate computation
  validate_computation(monthly_tra_e_gasoline)


  # Assumption 3: Road diesel and motor gasoline are the only fuels used in road transportation
  share_gasoline_diesel_road <- yearly %>%
    filter(siec_code %in% c(SIEC_OIL_PRODUCTS, SIEC_ROAD_DIESEL, SIEC_MOTOR_GASOLINE),
           nrg_bal_code =="FC_TRA_ROAD_E") %>%
    group_by(geo, time, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(siec_code, values) %>%
    group_by(geo, time) %>%
    summarise(share_road = (!!sym(SIEC_ROAD_DIESEL) + !!sym(SIEC_MOTOR_GASOLINE)) / !!sym(SIEC_OIL_PRODUCTS))

  ggplot(share_gasoline_diesel_road %>% filter_plot) +
    geom_line(aes(time, share_road)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()


  # This one doesn't need any computation
  # This is more to validate the assumption for computation later on


  # Assumption 4: Share of kerosene going to domestic aviation vs international aviation
  # is constant
  # Note: Monthly data has international aviation but not domestic
  share_kerosene_aviation <- yearly %>%
    filter(siec_code == SIEC_KEROSENE_XBIO,
           nrg_bal_code %in% c("INTAVI_E", "FC_TRA_DAVI_E")) %>%
    group_by(geo, time, nrg_bal_code, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = FC_TRA_DAVI_E / INTAVI_E) %>%
    ungroup()

  ggplot(share_kerosene_aviation %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_aviation_kerosene <- share_kerosene_aviation %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_KEROSENE_XBIO,
               nrg_bal_code %in% c("INTAVI_E")) %>%
        select(geo, time, siec_code, values)
        ,
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = share * values,
           nrg_bal_code = "FC_TRA_DAVI_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  # Visually validate computation
  validate_computation(monthly_tra_aviation_kerosene)


  # Assumption 5: Share of aviation gasoline going to aviation constant (and close to 1)
  share_aviation_aviation_gasoline <- yearly %>%
    filter(siec_code == SIEC_AVIATION_GASOLINE,
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_DAVI_E")) %>%
    group_by(geo, time, nrg_bal_code, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec_code) %>%
    summarise(share = (FC_TRA_DAVI_E) / GID_OBS)

  ggplot(share_aviation_aviation_gasoline %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_aviation_gasoline <- share_aviation_aviation_gasoline %>%
    project_shares %>%
    right_join(
      monthly %>%
        filter(siec_code == SIEC_AVIATION_GASOLINE,
               nrg_bal_code %in% c("GID_OBS")) %>%
        select(geo, time, siec_code, values)
      ,
      by=c("geo", "time", "siec_code")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_DAVI_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_aviation_gasoline)


  # Assumption 6: Motor gasoline, road diesel and kerosene represent most of the transport sector
  share_transport <- yearly %>%
    filter(siec_code %in% c(SIEC_MOTOR_GASOLINE, SIEC_ROAD_DIESEL, SIEC_KEROSENE_XBIO, SIEC_OIL_PRODUCTS),
           nrg_bal_code =="FC_TRA_E") %>%
    group_by(geo, time, siec_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(siec_code, values) %>%
    group_by(geo, time) %>%
    summarise(share_transport = (!!sym(SIEC_MOTOR_GASOLINE) + !!sym(SIEC_ROAD_DIESEL) + !!sym(SIEC_KEROSENE_XBIO)) / !!sym(SIEC_OIL_PRODUCTS))

  ggplot(share_transport %>% filter_plot) +
    geom_line(aes(time, share_transport)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()


  # Conclusion:
  # Transport will be split in Road, Aviation (domestic + international),
  # International Maritime and Others (domestic navigation, rail, pipeline etc.)
  # We will use the following shares:
  # - Road: Motor gasoline + Road diesel
  # - Aviation: all Kerosene
  # - International Maritime: using International Bunkers which is also available in monthly data
  # - Others: the rest based on FC_TRA_E - Road - Aviation - International Maritime
  #
  # If we don't want to divide inso sub sectors
  # We can do TRANSPORT = FC_TRA_E (Gasoline + Diesel) + INTAVI_E+FC_TRA_DAVI_E (Kerosene) + All bunkers


  added <- bind_rows(
    monthly_tra_road_e_gasoline,
    monthly_tra_road_e_diesel,
    monthly_tra_aviation_kerosene,
    monthly_tra_aviation_gasoline,
    monthly_tra_e_diesel,
    monthly_tra_e_gasoline
  ) %>%
    select(time, geo, siec_code, values, nrg_bal_code) %>%
    mutate(unit = "Thousand tonnes")

  # Re-add siec for NCV/emission factor matching
  # Though ultimately we should switch everything to siec_code
  added <- added %>%
    left_join(distinct(monthly, siec, siec_code))

  # There are very few common records (though the original ones are wrong)
  # cons_monthly_raw %>%
  #   inner_join(added %>% rename(values_added=values)) %>%
  #   View()
  monthly_filled <- bind_rows(
    monthly %>% anti_join(added %>% distinct(geo, time, siec_code, siec, nrg_bal_code)),
    added
  )

  return(monthly_filled)
}



fill_oil_non_energy_use_yearly <- function(yearly){

  # Austria, and maybe other countries have GD_PI but not GID_NE in yearly data.
  # We'll assume GID_NE=GD_PI for those
  yearly %>%
    group_by(siec_code, geo) %>%
    mutate(
      should_use = case_when(
        any(values[nrg_bal_code=="GID_NE"] > 0) ~ "GID_NE",
        any(values[nrg_bal_code=="GD_PI"] > 0) ~ "GD_PI",
        T ~ "GID_NE"
      )) %>%
    filter(
      (nrg_bal_code %in% c("GID_NE", "GD_PI") & should_use==nrg_bal_code) |
        (!nrg_bal_code %in% c("GID_NE", "GD_PI"))
    ) %>%
    mutate(
      nrg_bal_code = recode(nrg_bal_code, "GD_PI"="GID_NE")
    ) %>%
    ungroup() %>%
    # restore nrg_bal
    select(-nrg_bal) %>%
    left_join(
      yearly %>% ungroup() %>% distinct(nrg_bal_code, nrg_bal)
    )
}

#' Non energy use is not available in monthly data. We derive it from industrial production data
#' instead
#'
#' @return
#' @export
#'
#' @examples
fill_oil_non_energy_use_monthly <- function(yearly, monthly){


  gd_pi_to_gid_ne <- bind_rows(
    yearly,
    monthly
  ) %>%
    inner_join(
      tibble(
        freq=c("Annual", "Monthly"),
        nrg_bal_code=c("GID_NE", "GD_PI")
      )
    ) %>%
    group_by(geo, siec_code, year=year(time), nrg_bal_code, freq) %>%
    summarise(values=sum(values, na.rm=T),
              n=n(),
              .groups='drop'
              ) %>%
    filter(n==12 | freq=='Annual') %>%
    filter(!is.na(values)) %>%
    select(-c(freq, n)) %>%
    spread(nrg_bal_code, values) %>%
    mutate(ratio = GID_NE / GD_PI) %>%
    filter(!is.na(ratio), !is.infinite(ratio), ratio >0) %>%
    complete(
      nesting(geo, siec_code),
      year=seq(min(year(yearly$time)), max(year(monthly$time))),
      fill=list(ratio=NA)
    ) %>%
    # Fill up and down
    arrange(year) %>%
    group_by(geo, siec_code) %>%
    fill(ratio, .direction="updown") %>%
    ungroup() %>%
    select(geo, siec_code, year, ratio)


  # Add GID_NE where not existing
  monthly_gid_ne <- monthly %>%
    filter(nrg_bal_code=="GD_PI") %>%
    mutate(year = year(time)) %>%
    left_join(
      gd_pi_to_gid_ne,
      by=c("geo", "siec_code", "year")
    ) %>%
    mutate(ratio = replace_na(ratio, 1)) %>%
    mutate(values = values * ratio,
           nrg_bal_code = "GID_NE") %>%
    select(nrg_bal_code, siec_code, freq, unit, geo, time, values) %>%
    # Add nrg_bal and siec columns
    left_join(
      bind_rows(monthly, yearly) %>% ungroup() %>% distinct(nrg_bal_code, nrg_bal)
    ) %>%
    left_join(
      bind_rows(monthly, yearly) %>% ungroup() %>% distinct(siec_code, siec)
    )

  # Just ensure there were no GID_NE in monthly
  stopifnot(0==nrow(monthly %>% filter(nrg_bal_code=="GID_NE")))

  bind_rows(
    monthly,
    monthly_gid_ne
  )
}


#' NOT USED ANYMORE AS WE'RE RELYING ON GD_PI INSTEAD
#' Non energy use is not available in monthly data. We derive it from industrial production data
#' instead
#'
#' @return
#' @export
#'
#' @examples
fill_oil_non_energy_use_old <- function(cons_yearly_raw, cons_monthly_raw, eurostat_indprod){


  selected_nace_r2 <-  c(
    "C20", # Manufacture of chemicals and chemical products,
    "C21", # Manufacture of pharmaceuticals,
    "C22", # Manufacture of rubber and plastic products,
    "C19", # Manufacture of coke and refined petroleum products,
    # "C192", #Manufacture of refined petroleum products
    "C203", # Manufacture of paints, varnishes, and similar coatings,
    # "C206", # Manufacture of man-made fibers,
    "C205", # Manufacture of explosives, glues, and other chemical products,
    # "C2222", # Manufacture of plastic packing goods,
    # "C2017", # Manufacture of synthetic rubber in primary forms,
    "C23", #Manufacture of other non-metallic mineral products
    # "C1396", # Manufacture of other technical and industrial textiles,
    # "B0891", # Mining of chemical and fertilizer minerals,
    "B061", # Extraction of crude petroleum,
    # "C204", # Manufacture of soap and detergents, cleaning and polishing preparations, perfumes and toilet preparations,
    # "C211", # Manufacture of basic pharmaceutical products,
    # "C212", # Manufacture of pharmaceutical preparations,
    "C222", # Manufacture of plastics products,
    "C231", # Manufacture of glass and glass products,
    # "C232",
    "C233",
    "C234"
    # "C235",
    # "C236"
    # "C2331", # Manufacture of ceramic tiles and flags,
    # # "C2399", # Manufacture of other non-metallic mineral products n.e.c.,
    # "C241", # Manufacture of basic iron and steel and of ferro-alloys,
    # "C2442", # Aluminium production,
    # # "C2651", # Manufacture of instruments and appliances for measuring, testing and navigation,
    # # "C2711", # Manufacture of electric motors, generators and transformers,
    # # "C291" # Manufacture of motor vehicles
  )

  # Train model
  training_predicted <- cons_yearly_raw %>%
    filter(nrg_bal_code=="GID_NE", siec_code=="O4600") %>%
    # filter(geo=="European Union - 27 countries (from 2020)")
    filter(grepl("European Union", geo))

  training_predictors <- eurostat_indprod %>%
    filter(grepl("European Union", geo),
           unit=="Index, 2021=100",
           grepl("Calendar adjusted ", s_adj)) %>%
    filter(nace_r2_code %in% selected_nace_r2) %>%
    select(geo, time, nace_r2=nace_r2_code, values) %>%
    group_by(nace_r2) %>%
    filter(any(!is.na(values))) %>%
    group_by(time=floor_date(time, "year"), geo, nace_r2) %>%
    summarise(values=sum(values, na.rm=T)) %>%
    spread(nace_r2, values) %>%
    ungroup()

  training_data <- left_join(training_predicted, training_predictors)

  model <- lm(values ~   ., data=training_data %>% select(-c(geo, time, nrg_bal_code, siec_code, nrg_bal, siec, unit, freq)))
  summary(model)

  plt_data <- training_data
  plt_data$predicted <- predict(model, newdata=training_data %>% select(-c(values)))
  ggplot() +
    geom_point(data=plt_data, aes(time, values)) +
    geom_line(data=plt_data, aes(time, predicted)) +
    rcrea::scale_y_crea_zero()

  # Add to monthly
  # !!! Model is commensurate to yearly data (i.e. the intercept)

  # training_predictors_monthly <-
#
#   cons_monthly_raw %>%
#     filter(nrg_bal_code=="GID_NE", siec_code=="O4600") %>%
#     mutate(values = predict(model, newdata=cons_monthly_raw %>% select(-c(values, geo, time, nrg_bal_code, siec_code, nrg_bal, siec, unit, freq)))) %>%
#     select(-c(nrg_bal_code, siec_code)) %>%
#     bind_rows(cons_monthly_raw)

}
