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

  # Debug
  # x %>%
  #   filter(nrg_bal_code %in% c("GID_OBS", "GD_PI"),
  #          time=="2010-01-01",
  #          grepl("European", geo)) %>%
  #   filter(siec %in% c(SIEC_OIL_PRODUCTS, SIEC_FUEL_OIL, SIEC_HEATING_OIL,
  #                      SIEC_DIESEL, SIEC_GASOLINE, SIEC_BIOGASOLINE, SIEC_BIODIESEL)) %>%
  #   group_by(nrg_bal, siec) %>%
  #   summarise(values = sum(values, na.rm=T)) %>%
  #   arrange(desc(values))

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
#' Most importantly, non-energy use
#' is available in yearly but not in monthly data.
#'
#' We use yearly ratio 'Gross inland deliveries - energy use' / 'Gross inland deliveries - observed'
#' to update monthly data
#'
#' A more accurate way would be to predict this ratio using industrial production index (e.g. fertiliser, petrochemicals)
#' For a later version.
#'
#' @param cons_yearly_raw
#' @param cons_monthly_raw
#'
#' @return
#' @export
#'
#' @examples
add_oil_transport <- function(cons_monthly_raw, cons_yearly_raw) {


  # Get share of diesel and gasoline used in transportation
  shares <- cons_yearly_raw %>%
    filter(time >= "1990-01-01") %>%
    filter(siec %in% c("Motor gasoline",
                       "Road diesel"
                       )) %>%
    filter(grepl("Thousand tonnes", unit)) %>%
    filter(nrg_bal %in% c("Gross inland deliveries - observed",
                          "Final consumption - transport sector - energy use")) %>%
    mutate(year = year(time)) %>%
    select(nrg_bal_code, siec, geo, year, values) %>%
    tidyr::spread(nrg_bal_code, values) %>%
    mutate(share_transport = FC_TRA_E / GID_OBS) %>%
    select(-c(FC_TRA_E, GID_OBS))

  ggplot(shares) +
    geom_line(aes(year, share_transport, col = siec)) +
    facet_wrap(~geo)

  # Project til now
  years <- unique(year(cons_monthly_raw$time))
  shares_filled <- shares %>%
    tidyr::complete(year = years, geo, siec) %>%
    group_by(geo, siec) %>%
    arrange(year) %>%
    tidyr::fill(share_transport,
                .direction = c("updown")
                ) %>%
    ungroup()


  # Fill missing with average
  # shares_filled <- shares_filled %>%
  #   group_by(siec, year) %>%
  #   mutate(share_transport = ifelse(is.na(share_transport), mean(share_transport, na.rm=T), share_transport)) %>%
  #   ungroup()

  ggplot(shares_filled) +
    geom_line(aes(year, share_transport, col = siec)) +
    facet_wrap(~geo)

  cons_monthly_raw_transport <- cons_monthly_raw %>%
    filter(nrg_bal %in% c("Gross inland deliveries - observed")) %>%
    mutate(year = year(time)) %>%
    inner_join(shares_filled) %>%
    mutate(
      nrg_bal = "Final consumption - transport sector - energy use",
      nrg_bal_code = "FC_TRA_E",
      values = values * share_transport
    ) %>%
    select(-c(share_transport, year))

  return(bind_rows(
    cons_monthly_raw,
    cons_monthly_raw_transport
  ))
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
