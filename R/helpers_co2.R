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

add_iso2 <- function(x){
  x %>%
    mutate(iso2=countrycode::countrycode(geo, "country.name", "iso2c",
                                         custom_match = c("European Union - 27 countries (from 2020)"="EU",
                                                          "Kosovo*"="XK")))
}

split_gas_to_elec_others <- function(co2){
  gas_sectors <- co2 %>% filter(fuel_type=='gas') %>% pull(sector) %>% unique()
  if(length(gas_sectors)>2){stop("More than 2 sectors for gas. Shouldn't happen")}
  if(sets_are_equal(gas_sectors, c(SECTOR_ALL, SECTOR_ELEC))){
    co2 %>%
      filter(fuel_type=='gas') %>%
      group_by(iso2, geo, date, fuel_type) %>%
      summarise(value_co2_tonne = sum(value_co2_tonne * case_when(sector==SECTOR_ALL ~ 1,
                                                                  sector==SECTOR_ELEC ~ -1)),
                .groups = "drop") %>%
      mutate(sector='others') %>%
      bind_rows(
        co2 %>% filter(fuel_type!='gas' | (!sector %in% c(SECTOR_OTHERS, SECTOR_ALL)))) %>%
      ungroup()
  }else{
    log_info("Gas already split between electricity and others. Skipping.")
    co2
  }
}

split_gas_to_elec_all <- function(co2){
  gas_sectors <- co2 %>% filter(fuel_type=='gas') %>% pull(sector) %>% unique()
  if(length(gas_sectors)>2){stop("More than 2 sectors for gas. Shouldn't happen")}
  if(sets_are_equal(gas_sectors, c(SECTOR_ELEC, SECTOR_OTHERS))){
    co2 %>%
      filter(fuel_type=='gas', sector != SECTOR_ALL) %>%
      group_by(iso2, geo, date, fuel_type) %>%
      summarise(
        # coverage=weighted.mean(coverage, value_co2_tonne),
        across(value_co2_tonne, sum), .groups = "drop") %>%
      mutate(sector=SECTOR_ALL) %>%
      bind_rows(co2 %>% filter(fuel_type!='gas' | (!sector %in% c(SECTOR_OTHERS, SECTOR_ALL))))
  }else{
    log_info("Gas already split between electricity and all Skipping.")
    co2
  }
}

add_total_co2 <- function(co2){
  co2 %>%
    filter(fuel_type!='total') %>%
    group_by(iso2, geo, date) %>%
    summarise(across(value_co2_tonne, sum), .groups="drop") %>%
    mutate(fuel_type='total', sector=SECTOR_ALL) %>%
    bind_rows(co2 %>% filter(fuel_type!='total')) %>%
    ungroup()
}

format_co2_for_db <- function(co2_daily, pwr_demand, gas_demand, cut_tail_days=3){
  co2_daily %>%
    filter(date < min(max(pwr_demand$date), max(gas_demand$date)) - lubridate::days(cut_tail_days)) %>%
    mutate(across(c(fuel_type, sector), stringr::str_to_title),
           frequency='daily',
           region=iso2,
           unit='t/day',
           version=as.character(packageVersion("creaco2tracker"))) %>%
    select(region, date, fuel=fuel_type, sector, unit, frequency, version, value=value_co2_tonne)
}
