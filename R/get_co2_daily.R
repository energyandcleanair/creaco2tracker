
get_co2_daily <- function(diagnostic_folder='diagnostics',
                          use_cache=F,
                          iso2s = c("EU")
                          ){

  dir.create(diagnostic_folder, F, T)

  # Collect necessary data
  gas_demand <- download_gas_demand(region_id=iso2s, use_cache = use_cache)
  pwr_demand <- get_pwr_demand(use_cache = use_cache)
  eurostat_cons <- get_eurostat_cons(diagnostic_folder = diagnostic_folder, use_cache = use_cache)

  # Quick sanity checks
  diagnostic_pwr(pwr_demand, diagnostic_folder = diagnostic_folder)
  diagnostic_eurostat_cons(eurostat_cons, iso2s=iso2s, diagnostic_folder = diagnostic_folder)

  # Compute emissions
  co2 <- get_co2_from_eurostat_cons(eurostat_cons)

  # Filter regions
  # Note: this need to be done after co2 estimates,
  # as all EU countries will be used to estimate weighted average NCVs
  co2 <- co2 %>%
    filter(iso2 %in% iso2s)

  # Project data til now
  co2_filled <- project_until_now(co2, pwr = pwr_demand)

  # Downscale to daily data
  co2_daily <- downscale_daily(co2 = co2_filled, pwr_demand = pwr_demand, gas_demand = gas_demand)

  # Calculate total CO2
  co2_daily <- co2_daily %>%
    filter(fuel_type!='total') %>%
    group_by(iso2, geo, date) %>%
    summarise(across(c(CO2_hybrid, CO2_emissions), sum)) %>%
    mutate(fuel_type='total', sector=SECTOR_ALL) %>%
    bind_rows(co2_daily %>% filter(fuel_type!='total')) %>%
    ungroup()

  # Diagnostics
  diagnostic_co2(co2_daily, diagnostic_folder=diagnostic_folder)

  # Formatting / cleaning for db
  co2_daily <- co2_daily %>%
    filter(date < min(max(pwr_demand$date), max(gas_demand$date)) - lubridate::days(3)) %>%
    mutate(region=iso2,
           unit='t/day') %>%
    mutate(across(c(fuel_type, sector), stringr::str_to_title),
           frequency='daily',
           version=as.character(packageVersion("creaco2tracker"))) %>%
    select(region, date, fuel=fuel_type, sector, unit, frequency, version, value=CO2_hybrid)

  return(co2_daily)
}


get_pwr_demand <- function(date_from="2016-01-01", region=NULL, use_cache=T, refresh_cache=F) {

    pwr <- creahelpers::api.get('api.energyandcleanair.org/power/generation',
                              date_from=date_from,
                              aggregate_by='country,source,date',
                              region='EU',
                              split_by = 'year',
                              use_cache = use_cache,
                              refresh_cache = refresh_cache,
                              cache_folder = "cache")

  #add total generation
  pwr <- pwr %>%
    filter(source!='Total') %>%
    group_by(iso2, region, country, date) %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(source='Total') %>%
    bind_rows(pwr %>% filter(source!='Total'))

  #add EU total
  pwr <- pwr %>%
    filter(country!='EU total') %>%
    group_by(date, source) %>%
    filter(region=='EU') %>%
    dplyr::summarise_at(c("value_mw", "value_mwh"), sum, na.rm=T) %>%
    mutate(country='EU total',
           iso2='EU') %>%
    bind_rows(pwr %>% filter(country!='EU total')) %>%
    ungroup()

  return(pwr)
}




# get_entsog <- function() {
#
#   #gas data
#   ng_all <- seq(2016, lubridate::year(lubridate::today())) %>%
#     pbapply::pblapply(function(x){Sys.sleep(2);
#       read_csv(sprintf('https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=%s-01-01&date_to=%s-12-31&commodity=natural_gas&bypass_maintenance=true', x, x))}) %>%
#     bind_rows()
#
#   types <- c('distribution','consumption','storage_entry','storage_exit','crossborder','production')
#   entsog <- seq(2016, lubridate::year(lubridate::today())) %>%
#     pbapply::pblapply(function(x){Sys.sleep(2);
#       read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')))}) %>%
#     bind_rows()
#
#   #Gas imports + production+ storage+ implied consumption
#   inflows <- ng_all %>%
#     filter(commodity_origin_country %in% c('Algeria', 'Azerbaijan', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia',
#                                            'United Kingdom', 'Norway', 'Tunisia')) %>%
#     group_by(across(c(starts_with('destination'), date))) %>%
#     summarise(across(value_m3, sum)) %>%
#     mutate(type='imports')
#
#   storage_changes <- entsog %>% filter(type %in% c('storage_entry','storage_exit')) %>%
#     mutate(value_m3 = value_m3 * ifelse(type=='storage_exit', -1, 1)) %>%
#     group_by(across(c(starts_with('destination'), date))) %>%
#     summarise(across(value_m3, sum)) %>%
#     mutate(type='storage drawdown')
#
#   implied_cons <- entsog %>% filter(type == 'production') %>%
#     filter(commodity_origin_country %in% c('Algeria', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia',
#                                            'United Kingdom', 'Norway')) %>%
#     bind_rows(inflows, storage_changes) %>%
#     group_by(across(c(starts_with('destination'), date))) %>%
#     summarise(across(value_m3, sum)) %>%
#     mutate(type='consumption')
#
#   bind_rows(inflows, storage_changes, implied_cons, entsog %>% filter(type == 'production'))
# }



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
