downscale_daily <- function(co2, pwr_generation, gas_demand){

  #add Gas all sectors total
  co2 <- co2 %>%
    split_gas_to_elec_all()


  daily_proxy <- get_daily_proxy(pwr_generation = pwr_generation, gas_demand = gas_demand)

  #identify variable combos with data
  grps <- co2 %>% ungroup %>% filter(value>0) %>% distinct(fuel, sector)

  #dates for which to output estimates
  dts_daily <- seq.Date(min(co2$date), max(daily_proxy$date), by='d')


  # Fill potential missing dates in daily_proxy
  daily_proxy <- daily_proxy %>%
    tidyr::complete(iso2,
                    date=dts_daily,
                    fuel,
                    sector) %>%
    arrange(date) %>%
    group_by(iso2, fuel, sector) %>%
    fill(proxy_value, .direction="down") %>%
    filter(date %in% dts_daily) %>%
    ungroup()


  #expand monthly data to daily and add daily data
  co2_daily_flat <- co2 %>%
    group_by(iso2, fuel, sector, estimate) %>%
    rename(month=date) %>%
    full_join(
      tibble(date=dts_daily, month=dts_daily %>% 'day<-'(1)),
      relationship = "many-to-many",
      by="month"
      ) %>%
    mutate(value = value/days_in_month(date)) %>%
    right_join(grps, by=c("fuel", "sector")) %>%
    left_join(daily_proxy, by=c("iso2", "fuel", "sector", "date"))


  # Use daily data when available
  co2_daily <- co2_daily_flat %>%
    group_by(iso2, fuel, sector, estimate, month) %>%
    mutate(has_both=!is.na(value+proxy_value) & date>='2021-03-01',
           proxy_eurostat_ratio = mean(proxy_value[has_both]) / mean(value[has_both]),
           proxy_CO2 = proxy_value / proxy_eurostat_ratio,
           value = case_when(!is.na(proxy_CO2) & date>='2021-03-01' ~ proxy_CO2,
                                  #value * coverage + proxy_CO2 * 1-coverage,
                                  T ~ value)) %>%
    ungroup() %>%
    # remove proxy data
    select_at(vars(-starts_with('proxy_'))) %>%
    select(-c(month, has_both))

  # Check that monthly total are conserved
  comparison <- left_join(
    co2_daily_flat %>%
      group_by(iso2, fuel, sector, estimate, month=floor_date(date, "month")) %>%
      summarise(value_before=sum(value)),
    co2_daily %>%
      group_by(iso2, fuel, sector, estimate, month=floor_date(date, "month")) %>%
      summarise(value_after=sum(value))
  ) %>%
    filter(!is.na(value_before) | !is.na(value_after))

  stopifnot("Error: changed monthly total"=all(near(comparison$value_before, comparison$value_after, tol=0.1)))

  return(co2_daily)
}


get_daily_proxy <- function(pwr_generation, gas_demand){

  #aggregate daily power and gas data
  pwr_generation %>%
    filter(source %in% c('Coal', 'Fossil Gas')) %>%
    group_by(iso2, source) %>%
    mutate(fuel = recode(source, 'Fossil Gas'='gas', 'Coal'='coal'),
           sector=SECTOR_ELEC) %>%
    rename(value=value_mw) ->
    pwr_yoy

  gas_demand %>%
    filter(unit=='m3') %>%
    group_by(iso2, date) %>%
    summarise(across(value, sum), .groups="drop") %>%
    mutate(fuel='gas',
           sector=SECTOR_ALL) ->
    gas_yoy

  bind_rows(pwr_yoy, gas_yoy) %>%
    ungroup %>%
    mutate(date=as.Date(date)) %>%
    filter(date<=max(gas_yoy$date) - lubridate::days(3)) %>%
    select(iso2, date, fuel, sector, proxy_value=value) ->
    daily_proxy

  return(daily_proxy)
}


