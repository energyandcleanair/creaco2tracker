get_eu_iso2s <- function(eurostat=F){
  cl <- countrycode::codelist
  iso2s <- cl$iso2c[which(cl$eu28=="EU" & cl$iso2c != "GB")]
  if(eurostat) iso2s[iso2s=="GR"]="EL"
  iso2s
}

recode_siec <- function(x){
  x %>%
    mutate(siec = recode(
      siec,
      "Crude oil, NGL, refinery feedstocks, additives and oxygenates and other hydrocarbons" = "Crude oil",
      "Oil shale and oil sands" = "Oil shale"))
}

add_iso2 <- function(x, country_col="geo"){
  x %>%
    mutate(iso2=countrycode::countrycode(!!sym(country_col), "country.name", "iso2c",
                                         custom_match = c("European Union - 27 countries (from 2020)"="EU",
                                                          "EU27 & UK"="EU28",
                                                          "Kosovo*"="XK")))
}

split_gas_to_elec_others <- function(co2){

   group_by_cols <- intersect(names(co2), c("iso2", "geo", "date", "fuel", "sector", "estimate"))

    gas_sectors <- co2 %>% filter(fuel=='gas') %>% pull(sector) %>% unique()
  if(length(gas_sectors)>2){stop("More than 2 sectors for gas. Shouldn't happen")}
  if(sets_are_equal(gas_sectors, c(SECTOR_ALL, SECTOR_ELEC))){
    co2 %>%
      filter(fuel=='gas') %>%
      group_by_at(group_by_cols) %>%
      summarise(value = sum(value * case_when(sector==SECTOR_ALL ~ 1,
                                              sector==SECTOR_ELEC ~ -1)),
                .groups = "drop") %>%
      mutate(sector='others') %>%
      bind_rows(
        co2 %>% filter(fuel!='gas' | (!sector %in% c(SECTOR_OTHERS, SECTOR_ALL)))) %>%
      ungroup()
  }else{
    log_info("Gas already split between electricity and others. Skipping.")
    co2
  }
}

split_gas_to_elec_all <- function(co2){

  gas_sectors <- co2 %>% filter(fuel=='gas') %>% pull(sector) %>% unique()
  group_cols <- intersect(names(co2), c("iso2", "geo", "date", "fuel", "estimate"))

  if(length(gas_sectors)>2){stop("More than 2 sectors for gas. Shouldn't happen")}

  if(sets_are_equal(gas_sectors, c(SECTOR_ELEC, SECTOR_OTHERS))){
    co2 %>%
      filter(fuel=='gas', sector != SECTOR_ALL) %>%
      group_by_at(group_cols) %>%
      summarise(
        # coverage=weighted.mean(coverage, value),
        across(value, sum), .groups = "drop") %>%
      mutate(sector=SECTOR_ALL) %>%
      bind_rows(co2 %>% filter(fuel!='gas' | (!sector %in% c(SECTOR_OTHERS, SECTOR_ALL))))
  }else{
    log_info("Gas already split between electricity and all Skipping.")
    co2
  }
}

add_total_co2 <- function(co2){
  co2 %>%
    filter(fuel!='total') %>%
    group_by(iso2, geo, date, estimate) %>%
    summarise(across(value, sum), .groups="drop") %>%
    mutate(fuel='total', sector=SECTOR_ALL) %>%
    bind_rows(co2 %>% filter(fuel!='total')) %>%
    ungroup()
}

format_co2_for_db <- function(co2_daily, cut_tail_days=3){

  co2_daily %>%
    pivot_wider(names_from = estimate, values_from = value, names_prefic = "value_") %>%
    rename(value=value_central) %>%
    # Combine coal and coke
    mutate(fuel=case_when(
      fuel %in% c(FUEL_COAL, FUEL_COKE) ~ FUEL_COAL,
      T ~ fuel
    )) %>%
    group_by(iso2, date, fuel, sector, unit) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(across(c(fuel, sector), stringr::str_to_title),
           frequency='daily',
           region=iso2,
           version=as.character(packageVersion("creaco2tracker"))) %>%
    select(region, date, fuel, sector, unit, frequency, version, value)
}
