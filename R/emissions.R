get_co2_from_eurostat_cons <- function(eurostat_cons, diagnostics_folder="diagnostics",
                                       keep_siec=F){

  group_by_cols <- c("iso2", "geo", "date"="time", "fuel", "sector", "unit")
  if(keep_siec){
    group_by_cols <- c(group_by_cols, "siec")
  }

  eurostat_cons %>%
    add_ncv(diagnostics_folder = diagnostics_folder) %>%
    add_emission_factor() %>%
    mutate(value_co2_tonne=
             case_when(unit=='Thousand tonnes' ~ values * ncv_kjkg / 1000 * co2_factor_t_per_TJ,
                       unit=='Terajoule (gross calorific value - GCV)' & siec == "Natural gas" ~ values * ncv_gcv_gas * co2_factor_t_per_TJ
             )) %>%
    group_by_at(group_by_cols) %>%
    summarise(value = sum(value_co2_tonne, na.rm=T),
              unit='t',
              .groups="drop"
    ) %>%
    ungroup()
}


add_ncv <- function(x, diagnostics_folder=NULL){

  conversion_raw <- iea.get_conversion_factors(iso2=c(get_eu_iso2s(),"EU"))

  #' Add EUROSTAT Siec name to IEA. Some products have two SIEC because they are used differently in the transport sector for instance.
  add_siec <- function(y){
    separator <- "###"
    products <- y %>%
      distinct(product)

    products %>%
      mutate(siec = case_when(
        grepl("anthracite", product, ignore.case = TRUE) ~ "Hard coal",
        grepl("lignite", product, ignore.case = TRUE) ~ "Brown coal",
        grepl("BKB", product, ignore.case = TRUE) ~ "Brown coal briquettes",
        grepl("peat and peat products", product, ignore.case = TRUE) ~ "Peat",
        grepl("oil shale and oil sands", product, ignore.case = TRUE) ~ "Oil shale",
        # grepl("gas/diesel oil excl. biofuels|motor gasoline excl. biofuels", product, ignore.case = TRUE) ~ "Oil products",
        grepl("crude oil", product, ignore.case = TRUE) ~ "Crude oil",
        grepl("natural gas", product, ignore.case = TRUE) ~ "Natural gas",
        grepl("coke oven coke", product, ignore.case = TRUE) ~ "Coke oven coke",
        # Oil products both used as Oil products in SECTOR_ALL and more granularily in transport
        grepl("Gas/diesel oil", product, ignore.case = TRUE) ~ paste(c("Oil products",
                                                                       "Road diesel",
                                                                       "Gas oil and diesel oil (excluding biofuel portion)"),
                                                                     collapse=separator),
        grepl("Motor gasoline", product, ignore.case = TRUE) ~ paste(c("Oil products", "Motor gasoline", "Aviation gasoline"), collapse=separator),
        grepl("^Fuel oil$", product, ignore.case = TRUE) ~ paste(c("Fuel oil", "Heating and other gasoil"), collapse=separator),
        grepl("^Kerosene", product, ignore.case = TRUE) ~ paste(c("Kerosene",
                                                                  "Jet kerosene",
                                                                  "Kerosene-type jet fuel (excluding biofuel portion)"
                                                                  ), collapse=separator),
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(siec)) %>%
      rowwise() %>%
      mutate(siec=list(stringr::str_split(siec, separator)[[1]])) %>%
      ungroup() %>%
      unnest(siec) %>%
      right_join(y,
                 relationship = "many-to-many",
      )
  }

  # Brown coal consists of the addition of lignite and sub-bituminous coal.
  # In 2021, lignite made up 99.6 % of the brown coal consumed in the EU,and sub-bituminous coal 0.4 %.
  # https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Production_of_lignite_in_the_Western_Balkans_-_statistics&oldid=627763

  conversion <- conversion_raw %>%
    filter(flow_raw=="NAVERAGE", unit=="KJKG", iso2 %in% get_eu_iso2s()) %>%
    add_siec() %>%
    mutate(source="IEA") %>%
    group_by(iso2, siec, year) %>%
    summarise(ncv_kjkg=mean(value),
              max=max(value),
              min=min(value),
              source="IEA",
              .groups="drop"
    ) %>%
    filter(!is.na(siec))

  # There are weird outliers values (e.g. Brown coal for NL and FR)
  # Removing them
  conversion %>%
    group_by(siec, iso2) %>%
    summarise(ncv_kjkg=mean(ncv_kjkg),
              .groups="drop") %>%
    group_by(siec) %>%
    mutate(zscore=
             case_when(
               sd(ncv_kjkg)==0 ~ 0,
              T ~ (ncv_kjkg-mean(ncv_kjkg))/sd(ncv_kjkg))
    ) %>%
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
        summarise(qty=sum(values),
                  .groups="drop")
    ) %>%
    mutate(qty=tidyr::replace_na(qty, 0)) %>%
    group_by(siec, year) %>%
    summarise(ncv_kjkg_wmean=weighted.mean(ncv_kjkg, qty),
              .groups="drop") %>%
    filter(!is.na(ncv_kjkg_wmean))

  conversion_filled <- conversion %>%
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
  if(!is.null(diagnostics_folder)){
    conversion_filled %>%
      group_by(iso2, siec) %>%
      summarise(
        ncv_kjkg=mean(ncv_kjkg),
        .groups="drop"
      ) %>%
      ungroup() %>%
      # arrange(iso2, siec, source) %>%
      # select(-c(source)) %>%
      tidyr::spread(siec, ncv_kjkg) %>%
    write_csv(file.path(diagnostics_folder, "ncv.csv"))
  }

  # Add ncv
  x_with_ncv <- x %>%
    mutate(year=year(time)) %>%
    add_iso2() %>%
    left_join(conversion_filled) %>%
    group_by(geo, siec) %>%
    arrange(time) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup")

  stopifnot(all(!is.na(x_with_ncv[x_with_ncv$iso2!='ME',]$ncv_kjkg)))

  x_with_ncv
}

add_emission_factor <- function(x){
  # Taken from https://www.ipcc-nggip.iges.or.jp/EFDB/find_ef.php
  x %>%
    mutate(co2_factor_t_per_TJ = case_when(
      siec=='Hard coal'~92.8, #EFID=110620
      siec=='Brown coal'~113.1, #EFID=123085
      siec=="Brown coal briquettes"~99, #EFID=123073
      siec=='Peat'~117.766, #EFID=122005
      grepl('Oil shale', siec)~108,
      grepl('Oil products', siec)~72.3, #EFID=113617
      grepl('Fuel oil', siec)~77.7, #EFID=121579
      grepl('Heating and other gasoil', siec)~77.7, #EFID=121579
      grepl('Motor gasoline', siec)~72.1 	, #EFID=18667
      grepl('Road diesel', siec)~72.1, #EFID=18919
      grepl('Crude oil', siec)~73, #EFID=110603
      siec=='Natural gas'~55.74, #Average of EFID123092-123095
      siec=='Coke oven coke' ~ 113, #EFID=110624,
      grepl('Gas oil and diesel oil', siec) ~ 72.1, #EFID=18919
      grepl('Kerosene-type jet fuel', siec) ~ 72.69, # Taken from EEA: https://sdi.eea.europa.eu/catalogue/srv/eng/catalog.search#/metadata/f6e68f73-b494-4f8c-8c73-8a153a53f64a
      grepl('Aviation gasoline', siec) ~ 70.55, # Taken from EEA: https://sdi.eea.europa.eu/catalogue/srv/eng/catalog.search#/metadata/f6e68f73-b494-4f8c-8c73-8a153a53f64a
    )) %>%
    {
      stopifnot(all(!is.na(.$co2_factor_t_per_TJ)))
      .
    }
}
