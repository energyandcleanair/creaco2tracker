downscale_daily <- function(co2, pwr_demand, gas_demand){

  #add Gas all sectors total
  co2 <- co2 %>%
    split_gas_to_elec_all()

  daily_proxy <- get_daily_proxy(pwr_demand = pwr_demand, gas_demand = gas_demand)

  #identify variable combos with data
  grps <- co2 %>% ungroup %>% filter(value>0) %>% distinct(fuel, sector)

  #dates for which to output estimates
  dts_daily <- seq.Date(min(co2$date), max(daily_proxy$date), by='d')

  #expand monthly data to daily and add daily data
  co2 %>%
    group_by(iso2, geo, fuel, sector, estimate) %>%
    rename(month=date) %>%
    full_join(
      tibble(date=dts_daily, month=dts_daily %>% 'day<-'(1)),
      relationship = "many-to-many",
      by="month"
      ) %>%
    mutate(value = value/days_in_month(date)) %>%
    right_join(grps, by=c("fuel", "sector")) %>%
    left_join(daily_proxy, by=c("fuel", "sector", "date")) ->
    co2_daily

  #use daily data when available
  co2_daily <- co2_daily %>%
    group_by(iso2, geo, fuel, sector, estimate) %>%
    mutate(has_both=!is.na(value+proxy_value) & date>='2021-03-01',
           proxy_eurostat_ratio = mean(proxy_value[has_both]) / mean(value[has_both]),
           proxy_CO2 = proxy_value / proxy_eurostat_ratio,
           value = case_when(!is.na(proxy_CO2) & date>='2021-03-01' ~ proxy_CO2,
                                  #value * coverage + proxy_CO2 * 1-coverage,
                                  T ~ value)) %>%
    # remove proxy data
    select_at(vars(-starts_with('proxy_'))) %>%
    select(-c(month, has_both)) %>%
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
           fuel = recode(source, 'Fossil Gas'='gas', 'Coal'='coal'),
           sector=SECTOR_ELEC) %>%
    rename(value=value_mw) ->
    pwr_yoy

  gas_demand %>%
    filter(region_id=='EU',
           unit=='m3') %>%
    group_by(date) %>%
    summarise(across(value, sum)) %>%
    mutate(proxy_yoy = get_yoy(value, date),
           fuel='gas',
           sector=SECTOR_ALL) ->
    gas_yoy

  bind_rows(pwr_yoy, gas_yoy) %>%
    ungroup %>%
    mutate(date=as.Date(date)) %>%
    filter(date<=max(gas_yoy$date) - lubridate::days(3)) %>%
    select(date, fuel, sector, proxy_value=value, proxy_yoy) ->
    proxy_yoy

  return(proxy_yoy)
}


