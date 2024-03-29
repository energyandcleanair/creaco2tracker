downscale_daily <- function(co2, pwr_demand, gas_demand){

  #add Gas all sectors total
  co2 <- co2 %>%
    split_gas_to_elec_all()

  daily_proxy <- get_daily_proxy(pwr_demand = pwr_demand, gas_demand = gas_demand)

  #identify variable combos with data
  grps <- co2 %>% ungroup %>% filter(value_co2_tonne>0) %>% distinct(fuel_type, sector)

  #dates for which to output estimates
  dts_daily <- seq.Date(min(co2$date), max(daily_proxy$date), by='d')

  #expand monthly data to daily and add daily data
  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    rename(month=date) %>%
    full_join(
      tibble(date=dts_daily, month=dts_daily %>% 'day<-'(1)),
      relationship = "many-to-many",
      by="month"
      ) %>%
    mutate(value_co2_tonne = value_co2_tonne/days_in_month(date)) %>%
    right_join(grps, by=c("fuel_type", "sector")) %>%
    left_join(daily_proxy, by=c("fuel_type", "sector", "date")) ->
    co2_daily

  #use daily data when available
  co2_daily <- co2_daily %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    mutate(has_both=!is.na(value_co2_tonne+proxy_value) & date>='2021-03-01',
           proxy_eurostat_ratio = mean(proxy_value[has_both]) / mean(value_co2_tonne[has_both]),
           proxy_CO2 = proxy_value / proxy_eurostat_ratio,
           value_co2_tonne = case_when(!is.na(proxy_CO2) & date>='2021-03-01' ~ proxy_CO2,
                                  #value_co2_tonne * coverage + proxy_CO2 * 1-coverage,
                                  T ~ value_co2_tonne))

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


