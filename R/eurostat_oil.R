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
    c("O4600", # Oil products
      "O4100_TOT_4200-4500", # Crude oil, NGL, refinery feedstocks
      "O46711", #Road diesel
      "O4652", #Motor gasoline
      "O4671XR5220B", #Gas oil and diesel oil (excluding biofuel portion)
      "O46712", # Heating and other gasoil,
      "O4680", #Fuel oil
      "O4671XR5220B", #Gas oil and diesel oil (excluding biofuel portion)
      "O4661XR5230B", #Kerosene-type jet fuel (excluding biofuel portion)
      # Biofuels
      "R5210B", # Biogasoline
      "R5220B" # Biodiesel
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


  # Add Oil transport
  cons_monthly_raw <- add_oil_transport(
    cons_monthly_raw = cons_monthly_raw,
    cons_yearly_raw = cons_yearly_raw
  )

  list(
    monthly = cons_monthly_raw,
    yearly = cons_yearly_raw
  )
}

#' Process oil data from EUROSTAT
#'
#' @param x Raw EUROSTAT data
#' @return Processed oil consumption data
#' @export
process_oil <- function(x) {

  # Gross inland deliveries - energy use' is not available anymore starting from 2023
  # This is the one we need though...
  # we estimate GID_ENERGY = GID - Gross delivery to refinery, it worked pretty well
  # SIEC_CRUDE_OIL <- "O4100_TOT_4200-4500XBIO"
  SIEC_OIL_PRODUCTS <- "Oil products"
  SIEC_FUEL_OIL <- "Fuel oil"
  SIEC_HEATING_OIL <- "Heating and other gasoil"
  SIEC_DIESEL <- "Road diesel"
  SIEC_GASOLINE <- "Motor gasoline"
  SIEC_BIOGASOLINE <- "Blended biogasoline"
  SIEC_BIODIESEL <- "Blended biodiesels"
  SIEC_KEROSENE_XBIO <- "Kerosene-type jet fuel (excluding biofuel portion)"
  SIEC_GASOIL_AND_DIESEL_OIL <- "Gas oil and diesel oil (excluding biofuel portion)"


  # Only keep what interests us
  x <- x %>% filter(

    # Oil products: SECTOR_TOTAL
    (nrg_bal_code %in% c("GID_OBS", "GD_PI")
    & siec %in% c(SIEC_OIL_PRODUCTS, SIEC_FUEL_OIL, SIEC_HEATING_OIL,
                    SIEC_BIOGASOLINE, SIEC_BIODIESEL)) |


      # Oil products: SECTOR_TRANSPORT
      (nrg_bal_code %in% c("FC_TRA_E") # Added in add_oil_transport function
       & siec %in% c(SIEC_DIESEL, SIEC_GASOLINE)) |
      # Kerosene aviation -> SECTOR_TRANSPORT
      (nrg_bal_code %in% c("INTAVI_E")
       & siec %in% c(SIEC_KEROSENE_XBIO)) |
      # International maritime bunkers -> SECTOR_TRANSPORT
      (nrg_bal_code %in% c("INTMARB")
       & siec %in% c(SIEC_FUEL_OIL, SIEC_GASOIL_AND_DIESEL_OIL))
  )

  # We have separated fuel oil from oil products as it has a significantly higher
  # emission factor. To avoid double counting, we substract fuel oil from oil products.

  # We also remove biofuels from oil products (assuming co2=0) and remove them afterwards
  x <- x %>%
    group_by(nrg_bal, nrg_bal_code, freq, unit, geo, time) %>%
    mutate(values = dplyr::if_else(siec==SIEC_OIL_PRODUCTS,
                                   values - sum(values[siec %in% c(SIEC_FUEL_OIL, SIEC_HEATING_OIL,
                                                          SIEC_BIOGASOLINE, SIEC_BIODIESEL)]),
                                   values)) %>%
    ungroup() %>%
    filter(!siec %in% c(SIEC_BIOGASOLINE, SIEC_BIODIESEL))


  mult <- x %>%
    ungroup() %>%
    distinct(siec_code, siec, nrg_bal, nrg_bal_code) %>%
    mutate(factor = case_when(
      nrg_bal_code == "GD_PI" ~ -1,
      T ~ 1
    ),
    sector = case_when(
      grepl("transport|aviation|maritime", nrg_bal) ~ SECTOR_TRANSPORT,
      T ~ SECTOR_ALL
    )
    ) %>%
    select(-c(nrg_bal))

  # Need three for all and one or two for transport
  stopifnot("Fix oil"=nrow(mult[mult$sector==SECTOR_ALL,]) == 6,
            "Fix oil"=nrow(mult[mult$sector==SECTOR_TRANSPORT,]) == 5,
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
add_oil_transport <- function(cons_monthly_raw, cons_yearly_raw) {

  eu_iso2s <- get_eu_iso2s(include_eu = T)
  filter_plot <- function(x) {
    x %>% add_iso2() %>% filter(iso2 %in% eu_iso2s) %>% filter(time >= "2010-01-01")
  }
  stopifnot(all(cons_yearly_raw$unit == "Thousand tonnes"))
  stopifnot(all(cons_monthly_raw$unit == "Thousand tonnes"))

  project_shares <- function(x){
    x %>%
      ungroup() %>%
      tidyr::complete(time = unique(cons_monthly_raw$time), geo, siec) %>%
      group_by(geo, siec) %>%
      arrange(time) %>%
      tidyr::fill(share, .direction = c("updown")) %>%
      ungroup()
  }

  # Visually validate computation
  validate_computation <- function(computed_monthly){
    computed_monthly %>%
      group_by(time=floor_date(time, "year"), geo, nrg_bal_code, siec) %>%
      summarise(values_monthly = sum(values, na.rm=T),
                n=n()
      ) %>%
      filter(n==12) %>%
      # split and unnest nrg_bal_code
      mutate(nrg_bal_code = strsplit(nrg_bal_code, "\\+")) %>%
      unnest(nrg_bal_code) %>%
      # Prevent double counting
      group_by(time, geo, siec) %>%
      mutate(values_monthly = values_monthly / n()) %>%
      left_join(
        cons_yearly_raw %>%
          rename(values_yearly=values)
      ) %>%
      ungroup() %>%
      group_by(time, geo) %>%
      summarise(values_monthly = sum(values_monthly, na.rm=T),
                values_yearly = sum(values_yearly, na.rm=T)
      ) %>%
      gather(key, values, -time, -geo) %>%
      filter_plot %>%
      ggplot() +
      geom_line(aes(time, values, col=key)) +
      facet_wrap(~geo, scales='free_y') +
      rcrea::scale_y_zero()
  }


  # Assumption 1: Constant share of motor gasoline (that doesn't go to petro industry) goes to road energy use

  # Compute share
  share_motor_gasoline_road <- cons_yearly_raw %>%
    filter(siec %in% c("Motor gasoline"),
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_ROAD_E", "GD_PI")) %>%
    group_by(geo, time, siec, nrg_bal_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS - GD_PI))

  # Visually validate assumption
  ggplot(share_motor_gasoline_road %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_road_e_gasoline <- share_motor_gasoline_road %>%
    project_shares %>%
    right_join(
      cons_monthly_raw %>%
        filter(siec %in% c("Motor gasoline"),
               nrg_bal_code %in% c("GID_OBS", "GD_PI")) %>%
        mutate(factor = case_when(
          nrg_bal_code == "GD_PI" ~ -1,
          T ~ 1
        )) %>%
        group_by(geo, time, siec) %>%
        summarise(values=sum(values * factor, na.rm=T)),
      by=c("geo", "time", "siec")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  validate_computation(monthly_tra_road_e_gasoline)


  # Assumption 2: Constant share of road diesel goes to road energy use
  share_road_diesel_road <- cons_yearly_raw %>%
    filter_unit %>%
    filter(siec %in% c("Road diesel"),
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_ROAD_E")) %>%
    group_by(geo, time, nrg_bal_code, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_ROAD_E / (GID_OBS))

  ggplot(share_road_diesel_road %>% filter_plot) +
    geom_line(aes(time, share_road)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_road_e_diesel <- share_road_diesel_road %>%
    project_shares %>%
    right_join(
      cons_monthly_raw %>%
        filter(siec %in% c("Road diesel"),
               nrg_bal_code %in% c("GID_OBS")) %>%
        group_by(geo, time, siec) %>%
        summarise(values=sum(values, na.rm=T)),
      by=c("geo", "time", "siec")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_ROAD_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_road_e_diesel)


  # TEMP
  bind_rows(
    cons_monthly_raw %>% mutate(source='monthly'),
    cons_yearly_raw %>% mutate(source='yearly')
  ) %>%
    filter(siec=="Road diesel",
           nrg_bal_code=="GID_OBS",
           grepl("Europe", geo)
           ) %>%
    group_by(geo, time=floor_date(time, "year"), source) %>%
    summarise(values=sum(values, na.rm=T),
              n=n()) %>% arrange(time) %>%
    filter_plot %>%
    ggplot() +
    geom_line(aes(time, values, col=source)) +
    rcrea::scale_y_zero()


  # Assumption 2bis: Constant share of road diesel goes to transportation energy use
  share_road_diesel_transport <- cons_yearly_raw %>%
    filter_unit %>%
    filter(siec %in% c("Road diesel"),
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_E")) %>%
    group_by(geo, time, nrg_bal_code, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_E / (GID_OBS))

  ggplot(share_road_diesel_transport %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_e_diesel <- share_road_diesel_transport %>%
    project_shares %>%
    right_join(
      cons_monthly_raw %>%
        filter(siec %in% c("Road diesel"),
               nrg_bal_code %in% c("GID_OBS")) %>%
        select(geo, time, siec, values),
      by=c("geo", "time", "siec")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_e_diesel)


  # Assumption 3 ter: Constant share of motor gasoline (that doesn't go to petro industry) goes to transport energy use

  # Compute share
  share_motor_gasoline_tra <- cons_yearly_raw %>%
    filter(siec %in% c("Motor gasoline"),
           nrg_bal_code %in% c("GID_OBS", "FC_TRA_E", "GD_PI")) %>%
    group_by(geo, time, siec, nrg_bal_code) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec) %>%
    summarise(share = FC_TRA_E / (GID_OBS - GD_PI))

  # Visually validate assumption
  ggplot(share_motor_gasoline_tra %>% filter_plot) +
    geom_line(aes(time, share)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_e_gasoline <- share_motor_gasoline_tra %>%
    project_shares %>%
    right_join(
      cons_monthly_raw %>%
        filter(siec %in% c("Motor gasoline"),
               nrg_bal_code %in% c("GID_OBS", "GD_PI")) %>%
        mutate(factor = case_when(
          nrg_bal_code == "GD_PI" ~ -1,
          T ~ 1
        )) %>%
        group_by(geo, time, siec) %>%
        summarise(values=sum(values * factor, na.rm=T)),
      by=c("geo", "time", "siec")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "FC_TRA_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()


  # Visually validate computation
  validate_computation(monthly_tra_e_gasoline)





  # Assumption 3: Road diesel and motor gasoline are the only fuels used in road transportation
  share_gasoline_diesel_road <- cons_yearly_raw %>%
    filter(siec %in% c("Oil products", "Road diesel", "Motor gasoline"),
           nrg_bal_code =="FC_TRA_ROAD_E") %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(siec, values) %>%
    group_by(geo, time) %>%
    summarise(share_road = (`Road diesel` + `Motor gasoline`) / `Oil products`)

  ggplot(share_gasoline_diesel_road %>% filter_plot) +
    geom_line(aes(time, share_road)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()


  # This one doesn't need any computation
  # This is more to validate the assumption for computation later on

  # Assumption 4: Share of kerosene going to aviation constant (and close to 1)
  share_kerosene_aviation <- cons_yearly_raw %>%
    filter(siec %in% c("Kerosene-type jet fuel (excluding biofuel portion)"),
           nrg_bal_code %in% c("GID_OBS", "INTAVI_E", "FC_TRA_DAVI_E")) %>%
    group_by(geo, time, nrg_bal_code, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(nrg_bal_code, values) %>%
    group_by(geo, time, siec) %>%
    summarise(share = (INTAVI_E + FC_TRA_DAVI_E) / GID_OBS)

  ggplot(share_kerosene_aviation %>% filter_plot) +
    geom_line(aes(time, share_aviation)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # Compute new data
  monthly_tra_aviation <- share_kerosene_aviation %>%
    project_shares %>%
    right_join(
      cons_monthly_raw %>%
        filter(siec %in% c("Kerosene-type jet fuel (excluding biofuel portion)"),
               nrg_bal_code %in% c("GID_OBS")) %>%
        select(geo, time, siec, values)
        ,
      by=c("geo", "time", "siec")
    ) %>%
    mutate(values = values * share,
           nrg_bal_code = "INTAVI_E+FC_TRA_DAVI_E"
    ) %>%
    select(-c(share)) %>%
    ungroup()

  # Visually validate computation
  validate_computation(monthly_tra_aviation)


  # Assumption 5: Aviation is only kerosene
  share_aviation_kerosene <- cons_yearly_raw %>%
    filter(siec %in% c("Kerosene-type jet fuel (excluding biofuel portion)", "Oil products"),
           nrg_bal_code =="INTAVI_E") %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(siec, values) %>%
    group_by(geo, time) %>%
    summarise(share_kerosene = `Kerosene-type jet fuel (excluding biofuel portion)`/ `Oil products`)

  ggplot(share_aviation_kerosene %>% filter_plot) +
    geom_line(aes(time, share_kerosene)) +
    facet_wrap(~geo) +
    rcrea::scale_y_zero()

  # This one doesn't need any computation
  # This is more to validate the assumption for computation later on


  # Assumption 6: Motor gasoline, road diesel and kerosene represent most of the transport sector
  share_transport <- cons_yearly_raw %>%
    filter(siec %in% c("Motor gasoline", "Road diesel", "Kerosene-type jet fuel (excluding biofuel portion)", "Oil products"),
           nrg_bal_code =="FC_TRA_E") %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm=T))  %>%
    spread(siec, values) %>%
    group_by(geo, time) %>%
    summarise(share_transport = (`Motor gasoline` + `Road diesel` + `Kerosene-type jet fuel (excluding biofuel portion)`) / `Oil products`)

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

  added <- bind_rows(
    monthly_tra_road_e_gasoline,
    monthly_tra_road_e_diesel,
    monthly_tra_aviation,
    monthly_tra_e_diesel,
    monthly_tra_e_gasoline
  )

  # There are very few common records (though the original ones are wrong)
  # cons_monthly_raw %>%
  #   inner_join(added %>% rename(values_added=values)) %>%
  #   View()


  cons_monthly_raw_filled <- bind_rows(
    cons_monthly_raw %>% anti_join(added %>% distinct(geo, time, siec, nrg_bal_code)),
    added
  )

  return(cons_monthly_raw_filled)
}




#' NOT USED ANYMORE AS WE'RE RELYING ON GD_PI INSTEAD
#' Non energy use is not available in monthly data. We derive it from industrial production data
#' instead
#'
#' @return
#' @export
#'
#' @examples
fill_oil_non_energy_use <- function(cons_yearly_raw, cons_monthly_raw, eurostat_indprod){


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
