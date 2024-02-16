get_co2_from_eurostat_cons <- function(eurostat_cons, diagnostic_folder="diagnostics"){
  eurostat_cons %>%
    # filter(fuel_type=='coal') %>%
    add_ncv(diagnostic_folder = diagnostic_folder) %>%
    add_emission_factor() %>%
    mutate(CO2_emissions=
             case_when(unit=='Thousand tonnes' ~ values * ncv_kjkg / 1000 * co2_factor,
                       unit=='Terajoule (gross calorific value - GCV)' & siec == "Natural gas" ~ values * ncv_gcv_gas * co2_factor
             )) %>%
    group_by(iso2, geo, time, fuel_type, sector) %>%
    summarise_at('CO2_emissions', sum, na.rm=T) %>%
    ungroup()
}


add_ncv <- function(x, diagnostic_folder=NULL){

  conversion_raw <- iea.get_conversion_factors(iso2=c(get_eu_iso2s(),"EU"))

  add_siec <- function(x){
    x %>%
      mutate(siec = case_when(
        grepl("anthracite", product, ignore.case = TRUE) ~ "Hard coal",
        grepl("lignite", product, ignore.case = TRUE) ~ "Brown coal",
        grepl("peat and peat products", product, ignore.case = TRUE) ~ "Peat",
        grepl("oil shale and oil sands", product, ignore.case = TRUE) ~ "Oil shale",
        grepl("gas/diesel oil excl. biofuels|motor gasoline excl. biofuels", product, ignore.case = TRUE) ~ "Oil products",
        grepl("crude oil", product, ignore.case = TRUE) ~ "Crude oil",
        grepl("natural gas", product, ignore.case = TRUE) ~ "Natural gas",
        grepl("coke oven coke", product, ignore.case = TRUE) ~ "Coke oven coke",
        TRUE ~ NA_character_
      ))
  }

  # Brown coal consists of the addition of lignite and sub-bituminous coal.
  # In 2021, lignite made up 99.6 % of the brown coal consumed in the EU,and sub-bituminous coal 0.4 %.
  # https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Production_of_lignite_in_the_Western_Balkans_-_statistics&oldid=627763

  conversion <- conversion_raw %>%
    filter(flow_raw=="NAVERAGE", unit=="KJKG", iso2 %in% get_eu_iso2s()) %>%
    add_siec() %>%
    group_by(iso2, siec, year) %>%
    summarise(ncv_kjkg=mean(value),
              max=max(value),
              min=min(value),
              source="IEA"
              ) %>%
    filter(!is.na(siec)) %>%
    ungroup()

  # There are weird outliers values (e.g. Brown coal for NL and FR)
  # Removing them
  conversion %>%
    group_by(siec, iso2) %>%
    summarise(ncv_kjkg=mean(ncv_kjkg)) %>%
    group_by(siec) %>%
    mutate(zscore=(ncv_kjkg-mean(ncv_kjkg))/sd(ncv_kjkg)) %>%
    arrange(desc(zscore)) %>%
    filter(abs(zscore)<2) %>%
    select(-zscore, -ncv_kjkg) %>%
    left_join(conversion) %>%
    ungroup() -> conversion


  # Fill for missing iso2s and EU using weighted mean
  conversion_wmean <- conversion %>%
    left_join(
      x %>%
        group_by(iso2, year=year(time), siec) %>%
        summarise(qty=sum(values))
    ) %>%
    mutate(qty=tidyr::replace_na(qty, 0)) %>%
    group_by(siec, year) %>%
    summarise(ncv_kjkg_wmean=weighted.mean(ncv_kjkg, qty)) %>%
    filter(!is.na(ncv_kjkg_wmean))

  conversion_filled <- conversion %>%
    select(-c(min, max)) %>%
    ungroup() %>%
    tidyr::complete(
      iso2=unique(add_iso2(x)$iso2),
      siec,
      year
    ) %>%
    left_join(conversion_wmean) %>%
    mutate(ncv_kjkg=coalesce(ncv_kjkg, ncv_kjkg_wmean)) %>%
    select(-c(ncv_kjkg_wmean)) %>%
    ungroup() %>%
    mutate(source=coalesce(source, "IEA (weighted averaged)"))

  # Check dispersion (NCVs shouldn't be too far apart, otherwise need to manually check ratios)
  if(!is.null(diagnostic_folder)){
    conversion_filled %>%
      group_by(iso2, siec) %>%
      summarise(
        ncv_kjkg=mean(ncv_kjkg)
      ) %>%
      ungroup() %>%
      # arrange(iso2, siec, source) %>%
      # select(-c(source)) %>%
      tidyr::spread(siec, ncv_kjkg) %>%
    write_csv(file.path(diagnostic_folder, "ncv.csv"))
  }

  # Add ncv
  x %>%
    mutate(year=year(time)) %>%
    add_iso2() %>%
    left_join(conversion_filled) %>%
    group_by(geo, siec) %>%
    arrange(time) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup")


  # filter(grepl("lignite|sub-bituminous", product, ignore.case=T))
  # siec=='Hard coal'~29.3*5000/7000*94.6,
  # siec=='Brown coal'~10*102,
  # siec=='Peat'~9.7*106,
  # grepl('Oil shale', siec)~6.4*108,
  # grepl('Oil products', siec)~46*72,
  # grepl('Crude oil', siec)~44*73,
  # siec=='Natural gas'~.9*42*55),


}

add_emission_factor <- function(x){


  x %>%
    mutate(co2_factor = case_when(
      siec=='Hard coal'~92.8, #EFID=110620
      siec=='Brown coal'~113.1, #EFID=123085
      siec=='Peat'~117.766, #EFID=122005
      grepl('Oil shale', siec)~108,
      grepl('Oil products', siec)~72.3, #EFID=113617
      grepl('Crude oil', siec)~73, #EFID=110603
      siec=='Natural gas'~55.74, #Average of EFID123092-123095
      siec=='Coke oven coke' ~ 113) #EFID=110624
    )

  # mutate(co2_factor = case_when(
  #   siec=='Hard coal'~29.3*5000/7000*94.6,
  #   siec=='Brown coal'~10*102,
  #   siec=='Peat'~9.7*106,
  #   grepl('Oil shale', siec)~6.4*108,
  #   grepl('Oil products', siec)~46*72,
  #   grepl('Crude oil', siec)~44*73,
  #   siec=='Natural gas'~.9*42*55)
  # )
}
