
get_co2_daily <- function(diagnostic_folder='diagnostics', use_cache=F){

  dir.create(diagnostic_folder, F, T)

  gas_demand <- download_gas_demand(region_id='EU', use_cache = use_cache)
  pwr <- get_pwr_demand(use_cache = use_cache)

  #add rolling mean
  pwr <- pwr %>%
    group_by(region, country, source) %>%
    arrange(date) %>%
    mutate(plotdate = date %>% 'year<-'(2022),
           year=year(date),
           output_mw_rollmean=zoo::rollapplyr(value_mw, 7, mean, fill=NA))

  #output range of values for several years
  pwr_ranges <- pwr %>% filter(year %in% 2015:2021) %>%
    group_by(region, country, source, plotdate) %>%
    summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean))


  #plot by source
  if(!is.null(diagnostic_folder)){
    library(rcrea)

    pwr %>% saveRDS('diagnostics/pwr.RDS')

    plt <- pwr %>%
      filter(date<max(date)-3, year %in% 2021:2022, country=='EU total') %>%
      group_by(country) %>%
      filter(mean(value_mw, na.rm=T)>1e3) %>%
      ggplot(aes(plotdate)) +
      facet_wrap(~source, scales='free_y') +
      geom_ribbon(data=pwr_ranges %>% filter(country=='EU total'),
                  aes(ymin=min/1000, ymax=max/1000), fill=crea_palettes$CREA[2]) +
      geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), linewidth=1) +
      expand_limits(y=0) +
      # scale_x_datetime(date_labels = '%b') +
      labs(title='EU power generation by source', y='GW, 7-day mean', x='', col='', fill='') +
      theme_crea(legend.position='top') +
      scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
      scale_fill_crea_d(col.index = 2)

    ggsave(file.path(diagnostic_folder, 'EU power generation by source.png'),
           width=8, height=6, bg='white', plot=plt)
  }

  # EUROSTAT
  cons <- get_eurostat_cons(diagnostic_folder = diagnostic_folder,
                             use_cache = use_cache)



  ncv_gcv_gas <- 0.9

  cons_agg <- cons %>%
    add_ncv() %>%
    add_emission_factor() %>%
    mutate(CO2_emissions=
             case_when(unit=='Thousand tonnes' ~ values * ncv_kjkg / 1000 * co2_factor,
                       unit=='Terajoule (gross calorific value - GCV)' & siec == "Natural gas" ~ values * ncv_gcv_gas * co2_factor
                       )) %>%
    group_by(geo, time, fuel_type, sector) %>%
    summarise_at('CO2_emissions', sum, na.rm=T)

  #dates for which to output daily estimates
  dts <- cons_agg %>%
    bind_rows() %>%
    use_series(time) %>%
    min() %>%
    seq.Date(today() %>% 'day<-'(1), by='month')


  cons_filled <- project_until_now(cons_agg,
                                   pwr = pwr,
                                   dts = dts)

  #add EU
  # cons_filled <- cons_filled %>%
  #   ungroup %>%
  #   mutate(iso2c=countrycode::countrycode(geo, 'country.name', 'iso2c'),
  #          EU=iso2c %in% countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")] & iso2c != 'GB')

  #aggregate by fuel type and sector
  cons_filled %>%
    add_iso2() %>%
    filter(iso2=="EU") %>%
    group_by(time, fuel_type, sector) %>%
    summarise(
      # coverage = sum(CO2_emissions[!is.na(yoy)], na.rm=T)/sum(CO2_emissions, na.rm=T),
      across(CO2_emissions, sum, na.rm=T)) ->
    co2

  #aggregate daily power and gas data
  pwr %>%
    filter(source %in% c('Coal', 'Fossil Gas'), country=='EU total') %>%
    group_by(source) %>%
    mutate(crea_yoy = get_yoy(value_mw, date),
           fuel_type = recode(source, 'Fossil Gas'='gas', 'Coal'='coal'),
           sector='electricity') %>%
    rename(value=value_mw) ->
    pwr_yoy

  gas_demand %>%
    filter(region_id=='EU', unit=='m3') %>%
    group_by(date) %>%
    summarise(across(value, sum)) %>%
    mutate(crea_yoy = get_yoy(value, date), fuel_type='gas', sector='all') ->
    gas_yoy

  bind_rows(pwr_yoy, gas_yoy) %>%
    ungroup %>%
    mutate(date=as.Date(date)) %>%
    filter(date<=max(gas_yoy$date) - lubridate::days(3)) %>%
    select(date, fuel_type, sector, crea_value=value, crea_yoy) ->
    crea_yoy

  #add gas all sectors total
  co2 <- co2 %>%
    filter(fuel_type=='gas', sector != 'all') %>%
    group_by(time, fuel_type) %>%
    summarise(
      # coverage=weighted.mean(coverage, CO2_emissions),
      across(CO2_emissions, sum)) %>%
    mutate(sector='all') %>%
    bind_rows(co2 %>% filter(fuel_type!='gas' | sector != 'all'))

  #identify variable combos with data
  co2 %>% ungroup %>% filter(CO2_emissions>0) %>% distinct(fuel_type, sector) -> grps

  #dates for which to output estimates
  dts <- seq.Date(min(co2$time), max(crea_yoy$date), by='d')

  #expand monthly data to daily and add daily data
  co2 %>%
    group_by(fuel_type, sector) %>%
    rename(month=time) %>%
    full_join(tibble(date=dts, month=dts %>% 'day<-'(1))) %>%
    mutate(CO2_emissions = CO2_emissions/days_in_month(date)) %>%
    right_join(grps) %>%
    left_join(crea_yoy) ->
    co2_daily

  #use daily data when available
  co2_daily <- co2_daily %>%
    group_by(fuel_type, sector) %>%
    mutate(has_both=!is.na(CO2_emissions+crea_value) & date>='2021-03-01',
           crea_eurostat_ratio = mean(crea_value[has_both]) / mean(CO2_emissions[has_both]),
           crea_CO2 = crea_value / crea_eurostat_ratio,
           CO2_hybrid = case_when(!is.na(crea_CO2) & date>='2021-03-01'~crea_CO2,
                                  #CO2_emissions * coverage + crea_CO2 * 1-coverage,
                                  T~CO2_emissions))

  #calculate gas use outside power sector
  co2_daily <- co2_daily %>%
    filter(fuel_type=='gas') %>%
    group_by(date, fuel_type) %>%
    summarise(across(c(CO2_hybrid, CO2_emissions), ~.x[sector=='all']-.x[sector=='electricity'])) %>%
    mutate(sector='others') %>%
    bind_rows(co2_daily %>% filter(fuel_type!='gas' | (!sector %in% c('others', 'all'))))

  #calculate total CO2
  co2_daily <- co2_daily %>%
    filter(fuel_type!='total') %>%
    group_by(date) %>%
    summarise(across(c(CO2_hybrid, CO2_emissions), sum)) %>%
    mutate(fuel_type='total', sector='all') %>%
    bind_rows(co2_daily %>% filter(fuel_type!='total'))

  #plot
  if(!is.null(diagnostic_folder)){
    plt <- co2_daily %>%
      filter(year(date)>=2010) %>%
      mutate(across(c(fuel_type, sector), tolower)) %>%
      group_by(sector, fuel_type) %>%
      mutate(CO2_30d = zoo::rollapplyr(CO2_hybrid, 30, mean, fill=NA),
             year=as.factor(year(date)), plotdate=date %>% 'year<-'(2022)) %>%
      ggplot(aes(plotdate, CO2_30d/1e6, col=year)) +
      geom_line(size=0.2) +
      facet_wrap(~paste(fuel_type, sector), scales='free_y') +
      expand_limits(y=0)  + scale_x_date(expand=c(0,0)) +
      theme_crea() +
      labs(title="EU CO2 emissions", y='Mt/day, 30-day mean', x='')
      # scale_color_crea_d()

    ggsave(file.path(diagnostic_folder,'EU CO2 emissions.png'), plot=plt, width=8, height=6, bg='white')
  }

  # Formatting for db
  co2_daily <- co2_daily %>%
    filter(date < min(max(pwr$date), max(gas_demand$date)) - lubridate::days(3)) %>%
    mutate(region='EU',
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


add_ncv <- function(x){

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
              min=min(value)) %>%
    filter(!is.na(siec))


  # Check dispersion (NCVs shouldn't be too far apart, otherwise need to manually check ratios)
  conversion %>%
    filter(iso2=="DE") %>%
    group_by(siec) %>%
    summarise(ncv_kjkg=mean(ncv_kjkg),
              max=max(max),
              min=min(min))


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
    ungroup()


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
