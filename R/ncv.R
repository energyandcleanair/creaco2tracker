add_ncv_iea <- function(x, diagnostics_folder=NULL){

  conversion_raw <- iea.get_conversion_factors(iso2=c(get_eu_iso2s(), "EU"))

  #' Add EUROSTAT Siec code to IEA
  add_siec_code <- function(y){

    # conversion_raw %>%
    #     distinct(product) %>%
    #     pull(product) %>%
    #     paste(collapse=", ")

    # "Other hydrocarbons, Petroleum coke, Kerosene type jet fuel excl. biofuels, BKB/peat briquette plants, Other bituminous coal, Coke oven coke, Peat and peat products, Lubricants, Bitumen, Gas/diesel oil excl. biofuels,
    # White spirit & industrial spirit (SBP), Other oil products, Liquefied petroleum gases (LPG), Motor gasoline excl. biofuels,
    # Biogasoline, Bio jet kerosene, Biodiesels, Aviation gasoline, Natural Gas, Naphtha, Fuel oil, Paraffin waxes, Other kerosene,
    # Refinery gas, Natural gas liquids, Crude oil, Other liquid biofuels, Lignite, Anthracite, Refinery feedstocks, Additives/blending components, Charcoal, Gasoline type jet fuel, Coal tar, Ethane, Coking coal, Sub-bituminous coal, Peat products, Patent fuel, Oil shale and oil sands"
    siec_fuel <- tribble(
      ~siec_code, ~product,
      SIEC_HARD_COAL, "Anthracite",
      SIEC_BROWN_COAL, "Lignite",
      SIEC_BROWN_COAL_BRIQUETTES, "BKB/peat briquette plants",
      SIEC_PEAT, "Peat and peat products",
      SIEC_OIL_SHALE, "Oil shale and oil sands",
      SIEC_CRUDE_OIL, "Crude oil",
      SIEC_NATURAL_GAS, "Natural Gas",
      SIEC_COKE_OVEN_COKE, "Coke oven coke",
      SIEC_OIL_PRODUCTS, "Other oil products",
      SIEC_ROAD_DIESEL, "Gas/diesel oil excl. biofuels",
      SIEC_GASOIL_DIESEL, "Gas/diesel oil excl. biofuels",
      SIEC_AVIATION_GASOLINE, "Aviation gasoline",
      SIEC_MOTOR_GASOLINE_XBIO, "Motor gasoline excl. biofuels",
      SIEC_FUEL_OIL, "Fuel oil",
      SIEC_HEATING_GASOIL, "Gas/diesel oil excl. biofuels",
      SIEC_KEROSENE_XBIO, "Kerosene type jet fuel excl. biofuels"
    )

    y %>%
      right_join(siec_fuel, by="product", relationship='many-to-many')
  }




  # Brown coal consists of the addition of lignite and sub-bituminous coal.
  # In 2021, lignite made up 99.6 % of the brown coal consumed in the EU,and sub-bituminous coal 0.4 %.
  # https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Production_of_lignite_in_the_Western_Balkans_-_statistics&oldid=627763

  conversion <- conversion_raw %>%
    filter(flow_raw=="NAVERAGE", unit=="KJKG", iso2 %in% get_eu_iso2s()) %>%
    add_siec_code() %>%
    mutate(source="IEA") %>%
    group_by(iso2, siec_code, year) %>%
    summarise(ncv_kjkg=mean(value),
              # max=max(value),
              # min=min(value),
              source="IEA",
              .groups="drop"
    ) %>%
    filter(!is.na(siec_code))

  # There are weird outliers values (e.g. Brown coal for NL and FR)
  # Removing them
  conversion <- conversion %>%
    group_by(siec_code, iso2) %>%
    summarise(ncv_kjkg=mean(ncv_kjkg),
              .groups="drop") %>%
    group_by(siec_code) %>%
    mutate(zscore=
             case_when(
               sd(ncv_kjkg)==0 ~ 0,
               T ~ (ncv_kjkg-mean(ncv_kjkg))/sd(ncv_kjkg))
    ) %>%
    arrange(desc(zscore)) %>%
    filter(abs(zscore)<2) %>%
    select(-zscore, -ncv_kjkg) %>%
    left_join(conversion) %>%
    ungroup()


  # Fill for missing iso2s and EU using weighted mean
  conversion_wmean <- conversion %>%
    left_join(
      x %>%
        group_by(iso2, year=year(time), siec_code) %>%
        summarise(qty=sum(values),
                  .groups="drop")
    ) %>%
    mutate(qty=tidyr::replace_na(qty, 0)) %>%
    group_by(siec_code, year) %>%
    summarise(ncv_kjkg_wmean=weighted.mean(ncv_kjkg, qty),
              .groups="drop") %>%
    filter(!is.na(ncv_kjkg_wmean))

  conversion_filled <- conversion %>%
    ungroup() %>%
    tidyr::complete(
      iso2=unique(add_iso2(x)$iso2),
      siec_code,
      year=seq(min(year(x$time)), max(year(x$time)), by=1)
    ) %>%
    # Fill time wise
    group_by(iso2, siec_code) %>%
    fill(ncv_kjkg, .direction = "downup") %>%
    left_join(conversion_wmean) %>%
    mutate(ncv_kjkg=coalesce(ncv_kjkg, ncv_kjkg_wmean)) %>%
    fill(ncv_kjkg, .direction = "downup") %>%
    select(-c(ncv_kjkg_wmean)) %>%
    ungroup() %>%
    mutate(source=coalesce(source, "IEA (weighted averaged)"))

  # Check dispersion (NCVs shouldn't be too far apart, otherwise need to manually check ratios)
  if(!is_null_or_empty(diagnostics_folder)){
    conversion_filled %>%
      group_by(iso2, siec_code) %>%
      summarise(
        ncv_kjkg=mean(ncv_kjkg),
        .groups="drop"
      ) %>%
      ungroup() %>%
      tidyr::spread(siec_code, ncv_kjkg) %>%
      write_csv(file.path(diagnostics_folder, "ncv_iea.csv"))
  }

  # Add ncv
  x_with_ncv <- x %>%
    mutate(year=year(time)) %>%
    add_iso2() %>%
    left_join(conversion_filled, by=c("iso2", "siec_code", "year")) %>%
    group_by(iso2, siec_code) %>%
    arrange(time) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup")

  stopifnot(all(!is.na(x_with_ncv %>% filter(iso2!='ME', !grepl("Terajoule", unit)) %>% pull(ncv_kjkg))))

  # NEW: Make it time insensitive. It otherwise introduces weird patterns
  x_with_ncv <- x_with_ncv %>%
    group_by(iso2, siec_code) %>%
    mutate(ncv_kjkg=mean(ncv_kjkg, na.rm=T))

  x_with_ncv
}

# New function to get SIEC fuel mapping
get_siec_ipcc_fuel_mapping <- function() {
  tribble(
    ~siec_code, ~fuel,
    SIEC_HARD_COAL, "Anthracite",
    SIEC_BROWN_COAL, "Lignite",
    SIEC_BROWN_COAL_BRIQUETTES, "Brown Coal Briquettes",
    SIEC_CRUDE_OIL, "Crude Oil",
    SIEC_NATURAL_GAS, "Natural Gas",
    SIEC_COKE_OVEN_COKE, "Coke Oven Coke and Lignite Coke",
    SIEC_OIL_PRODUCTS, "Other Petroleum Products",
    SIEC_ROAD_DIESEL, "Diesel Oil",
    SIEC_GASOIL_DIESEL, "Gas Oil",
    SIEC_AVIATION_GASOLINE, "Aviation Gasoline",
    SIEC_MOTOR_GASOLINE_XBIO, "Motor Gasoline",
    SIEC_FUEL_OIL, "Residual Fuel Oil",
    SIEC_HEATING_GASOIL, "Gas Oil",
    SIEC_KEROSENE_XBIO, "Jet Kerosene"
  )
}


get_ipcc_data <- function() {
  read_csv(get_data_filepath('EFDB_output.csv'))
}

get_ipcc_ncv <- function() {
  get_ipcc_data() %>%
    filter(grepl('2006', `Type of parameter`)) %>%
    filter(Unit %in% ("TJ/Gg"),
           Description=="Net Calorific Value (NCV)") %>%
    mutate(ncv_kjkg = as.numeric(Value) * 1e3) %>%
    select(fuel = `Fuel 2006`, ncv_kjkg)
}


add_ncv_ipcc <- function(x, diagnostics_folder=NULL){

  # Get IPCC data
  ipcc <- get_ipcc_ncv()

  # Get SIEC fuel mapping
  siec_fuel <- get_siec_ipcc_fuel_mapping()

  # Map SIEC codes to IPCC fuels
  ncvs <- siec_fuel %>%
    left_join(ipcc, by="fuel") %>%
    select(-c(fuel))

  # Write diagnostics if folder is provided
  if(!is_null_or_empty(diagnostics_folder)){
    ncvs %>%
      write_csv(file.path(diagnostics_folder, "ncv_ipcc.csv"))
  }

  # Add NCVs to the input data
  x_with_ncv <- x %>%
    left_join(ncvs, by = "siec_code")

  # Check for missing NCVs
  missing_ncvs <- x_with_ncv %>%
    filter(is.na(ncv_kjkg)) %>%
    distinct(siec_code)

  if(nrow(missing_ncvs) > 0) {
    warning("Missing NCVs for the following SIEC codes: ",
            paste(missing_ncvs$siec_code, collapse=", "))
  }

  return(x_with_ncv)
}
