get_co2_from_eurostat_cons <- function(eurostat_cons, diagnostics_folder="diagnostics"){
  eurostat_cons %>%
    add_ncv(diagnostics_folder = diagnostics_folder) %>%
    add_emission_factor() %>%
    mutate(value_co2_tonne=
             case_when(unit=='Thousand tonnes' ~ values * ncv_kjkg / 1000 * co2_factor_t_per_TJ,
                       unit=='Terajoule (gross calorific value - GCV)' & siec == "Natural gas" ~ values * ncv_gcv_gas * co2_factor_t_per_TJ
             )) %>%
    group_by(iso2, geo, date=time, fuel, sector) %>%
    summarise(value = sum(value_co2_tonne),
              unit='t',
              .groups="drop"
    )
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
        grepl("peat and peat products", product, ignore.case = TRUE) ~ "Peat",
        grepl("oil shale and oil sands", product, ignore.case = TRUE) ~ "Oil shale",
        # grepl("gas/diesel oil excl. biofuels|motor gasoline excl. biofuels", product, ignore.case = TRUE) ~ "Oil products",
        grepl("crude oil", product, ignore.case = TRUE) ~ "Crude oil",
        grepl("natural gas", product, ignore.case = TRUE) ~ "Natural gas",
        grepl("coke oven coke", product, ignore.case = TRUE) ~ "Coke oven coke",
        # Oil products both used as Oil products in SECTOR_ALL and more granularily in transport
        grepl("Gas/diesel oil", product, ignore.case = TRUE) ~ paste(c("Oil products", "Road diesel"), collapse=separator),
        grepl("Motor gasoline", product, ignore.case = TRUE) ~ paste(c("Oil products", "Motor gasoline"), collapse=separator),
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
  x %>%
    mutate(year=year(time)) %>%
    add_iso2() %>%
    left_join(conversion_filled) %>%
    group_by(geo, siec) %>%
    arrange(time) %>%
    tidyr::fill(ncv_kjkg, .direction = "downup")
}

add_emission_factor <- function(x){

  x %>%
    mutate(co2_factor_t_per_TJ = case_when(
      siec=='Hard coal'~92.8, #EFID=110620
      siec=='Brown coal'~113.1, #EFID=123085
      siec=='Peat'~117.766, #EFID=122005
      grepl('Oil shale', siec)~108,
      grepl('Oil products', siec)~72.3, #EFID=113617
      grepl('Motor gasoline', siec)~72.1 	, #EFID=18667
      grepl('Road diesel', siec)~72.1, #EFID=18919
      grepl('Crude oil', siec)~73, #EFID=110603
      siec=='Natural gas'~55.74, #Average of EFID123092-123095
      siec=='Coke oven coke' ~ 113) #EFID=110624
    )
}
