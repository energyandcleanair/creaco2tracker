downscale_daily <- function(co2, pwr_demand, gas_demand){

  #add Gas all sectors total
  gas_sectors <- co2 %>% filter(fuel_type=='gas') %>% pull(sector) %>% unique()
  if(all(sort(gas_sectors) == sort(c(SECTOR_ELEC, SECTOR_OTHERS)))){
    co2 <- co2 %>%
      filter(fuel_type=='gas', sector != SECTOR_ALL) %>%
      group_by(iso2, geo, time, fuel_type) %>%
      summarise(
        # coverage=weighted.mean(coverage, CO2_emissions),
        across(CO2_emissions, sum)) %>%
      mutate(sector=SECTOR_ALL) %>%
      bind_rows(co2 %>% filter(fuel_type!='gas' | sector != SECTOR_ALL))
  }

  daily_proxy <- get_daily_proxy(pwr_demand = pwr_demand, gas_demand = gas_demand)

  #identify variable combos with data
  co2 %>% ungroup %>% filter(CO2_emissions>0) %>% distinct(fuel_type, sector) -> grps

  #dates for which to output estimates
  dts_daily <- seq.Date(min(co2$time), max(daily_proxy$date), by='d')

  #expand monthly data to daily and add daily data
  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    rename(month=time) %>%
    full_join(tibble(date=dts_daily, month=dts_daily %>% 'day<-'(1))) %>%
    mutate(CO2_emissions = CO2_emissions/days_in_month(date)) %>%
    right_join(grps) %>%
    left_join(daily_proxy) ->
    co2_daily

  #use daily data when available
  co2_daily <- co2_daily %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    mutate(has_both=!is.na(CO2_emissions+proxy_value) & date>='2021-03-01',
           proxy_eurostat_ratio = mean(proxy_value[has_both]) / mean(CO2_emissions[has_both]),
           proxy_CO2 = proxy_value / proxy_eurostat_ratio,
           CO2_hybrid = case_when(!is.na(proxy_CO2) & date>='2021-03-01' ~ proxy_CO2,
                                  #CO2_emissions * coverage + proxy_CO2 * 1-coverage,
                                  T ~ CO2_emissions))

  # Resplit gas between electricity and others
  co2_daily <- co2_daily %>%
    filter(fuel_type=='gas') %>%
    group_by(iso2, geo, date, fuel_type) %>%
    summarise(across(c(CO2_hybrid, CO2_emissions), ~.x[sector==SECTOR_ALL]-.x[sector==SECTOR_ELEC])) %>%
    mutate(sector='others') %>%
    bind_rows(co2_daily %>% filter(fuel_type!='gas' | (!sector %in% c(SECTOR_OTHERS, SECTOR_ALL)))) %>%
    ungroup()

  return(co2_daily)
}


get_daily_proxy <- function(pwr_demand, gas_demand){

  #aggregate daily power and gas data
  pwr_demand %>%
    filter(source %in% c('Coal', 'Fossil Gas'),
           country=='EU total') %>%
    group_by(source) %>%
    mutate(proxy_yoy = get_yoy(value_mw, date),
           fuel_type = recode(source, 'Fossil Gas'='gas', 'Coal'='coal'),
           sector=SECTOR_ELEC) %>%
    rename(value=value_mw) ->
    pwr_yoy

  gas_demand %>%
    filter(region_id=='EU',
           unit=='m3') %>%
    group_by(date) %>%
    summarise(across(value, sum)) %>%
    mutate(proxy_yoy = get_yoy(value, date),
           fuel_type='gas',
           sector=SECTOR_ALL) ->
    gas_yoy

  bind_rows(pwr_yoy, gas_yoy) %>%
    ungroup %>%
    mutate(date=as.Date(date)) %>%
    filter(date<=max(gas_yoy$date) - lubridate::days(3)) %>%
    select(date, fuel_type, sector, proxy_value=value, proxy_yoy) ->
    proxy_yoy

  return(proxy_yoy)
}


