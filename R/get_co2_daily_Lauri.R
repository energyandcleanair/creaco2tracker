
get_co2_daily <- function(diagnostic_folder='diagnostics', 
                          rollmean_days=28){

  dir.create(diagnostic_folder, F, T)

  implied_cons_all <- get_entsog()

  implied_cons_all %>% filter(commodity_destination_region=='EU') %>% 
    group_by(date) %>% summarise(across(value_m3, sum, na.rm=T)) ->
    implied_cons
  
  # Plot
  if(!is.null(diagnostic_folder)){
    saveRDS(implied_cons_all, file.path(diagnostic_folder, 'implied_cons.RDS'))
    
    plt <- implied_cons %>% 
      mutate(value_m3=zoo::rollapplyr(value_m3, rollmean_days, mean, fill=NA),
             plotdate=date %>% 'year<-'(2022), year=as.factor(year(date))) %>%
      filter(year(date) %in% 2021:2023, date<max(date)-7) %>%
      ggplot(aes(plotdate, value_m3/1e9, alpha=year)) +
      facet_wrap(~commodity_destination_region) +
      geom_line(linewidth=1) +
      labs(title='Europe gas imports, storage and implied consumption', x='',
           y=paste0('billion cubic meters per day, ',rollmean_days,'-day mean'),
           col='flow') +
      theme_crea() + #scale_color_crea_d('dramatic') +
      scale_alpha_manual(values=c(.4, .7,1),
                         guide = guide_legend(override.aes = list(col = c('gray60', 'gray30', 'black'), alpha=c(1,1,1))))
    ggsave(filename=file.path(diagnostic_folder, 'gas imports and consumption trends.png'),
           plot=plt, width=8, height=6, bg='white')
  }

  pwr <- get_entsoe()

  #add rolling mean
  pwr <- pwr %>%
    group_by(region, country, source) %>%
    arrange(date) %>%
    mutate(plotdate = date %>% 'year<-'(2022), year=year(date),
           output_mw_rollmean=zoo::rollapplyr(value_mw, rollmean_days, mean, fill=NA))

  #output range of values for several years
  pwr_ranges <- pwr %>% filter(year %in% 2015:2021) %>%
    group_by(region, country, source, plotdate) %>%
    summarise(min=min(output_mw_rollmean, na.rm=T), max=max(output_mw_rollmean, na.rm=T))


  #plot by source
  if(!is.null(diagnostic_folder)){
    library(rcrea)
    
    pwr %>% saveRDS(file.path(diagnostic_folder, 'pwr.RDS'))

    plt <- pwr %>% filter(date<max(date)-3, year %in% 2021:2023, country=='EU total') %>%
      group_by(country) %>% filter(mean(value_mw, na.rm=T)>1e3) %>%
      ggplot(aes(plotdate)) +
      facet_wrap(~source, scales='free_y') +
      geom_ribbon(data=pwr_ranges %>% filter(country=='EU total'),
                  aes(ymin=min/1000, ymax=max/1000), fill=crea_palettes$CREA[2]) +
      geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), linewidth=1) +
      expand_limits(y=0) +
      # scale_x_datetime(date_labels = '%b') +
      labs(title='EU power generation by source', y='GW, 30-day mean', x='', col='', fill='') +
      theme_crea(legend.position='top') +
      scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
      scale_fill_crea_d(col.index = 2)

    ggsave(file.path(diagnostic_folder, 'EU power generation by source.png'),
           width=8, height=6, bg='white', plot=plt)
  }

  # EUROSTAT
  cons <- get_eurostat_cons(diagnostic_folder = diagnostic_folder)

  #calculate CO2 emissions
  bind_rows(cons$coal, cons$oil, cons$gas) %>%
    mutate(CO2.factor = case_when(siec=='Hard coal'~29.3*5000/7000*94.6,
                                  siec=='Brown coal'~10*102,
                                  siec=='Peat'~9.7*106,
                                  grepl('Oil shale', siec)~6.4*108,
                                  grepl('Oil products', siec)~46*72,
                                  grepl('Crude oil', siec)~44*73,
                                  siec=='Natural gas'~.9*42*55),
           CO2_emissions=values*CO2.factor) ->
    cons_agg

  #save.image('diagnostics/EU energy data.RData')
  

  #dates for which to output daily estimates
  dts <- cons %>% bind_rows() %>% use_series(time) %>% min() %>%
    seq.Date(today() %>% 'day<-'(1), by='month')

  #fill data to present assuming deviation from 3-year average stays same as in last 3 months of data
  cons_agg %>%
    group_by(geo, time, fuel_type, sector) %>%
    summarise_at('CO2_emissions', sum, na.rm=T) %>%
    group_by(geo, fuel_type, sector) %>%
    creahelpers::expand_dates('time', dts) %>% arrange(time) %>%
    group_modify(function(df, ...) {
      df %<>% group_by(month=month(time)) %>%
        mutate(mean3y = CO2_emissions %>% lag %>% zoo::rollapplyr(3, mean, na.rm=F, fill=NA),
               yoy = CO2_emissions / mean3y - 1) %>% ungroup %>% select(-month)

      df$yoy %>% zoo::rollapplyr(3, mean, na.rm=F) %>% na.omit %>% tail(1) -> latest_yoy

      latest_data <- max(df$time[!is.na(df$CO2_emissions)])

      df %>% mutate(CO2_emissions = ifelse(time>latest_data,
                                           mean3y * (1+latest_yoy),
                                           CO2_emissions))
    }) -> cons_filled

  #add EU
  cons_filled <- cons_filled %>%
    ungroup %>%
    mutate(iso2c=countrycode::countrycode(geo, 'country.name', 'iso2c'),
           EU=iso2c %in% countrycode::codelist$iso2c[!is.na(countrycode::codelist$eu28)] & iso2c != 'GB')

  #aggregate by fuel type and sector
  cons_filled %>%
    filter(EU) %>%
    group_by(time, fuel_type, sector) %>%
    summarise(coverage = sum(CO2_emissions[!is.na(yoy)], na.rm=T)/sum(CO2_emissions, na.rm=T),
              across(CO2_emissions, sum, na.rm=T)) ->
    co2

  #aggregate daily power and gas data
  pwr %>% filter(source %in% c('Coal', 'Fossil Gas'), country=='EU total') %>%
    group_by(source) %>%
    mutate(crea_yoy = get_yoy(value_mw, date),
           fuel_type = recode(source, 'Fossil Gas'='gas', 'Coal'='coal'),
           sector='electricity') %>%
    rename(value=value_mw) ->
    pwr_yoy

  implied_cons %>% 
    group_by(date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(crea_yoy = get_yoy(value_m3, date), fuel_type='gas', sector='all') %>%
    rename(value=value_m3) ->
    gas_yoy

  bind_rows(pwr_yoy, gas_yoy) %>% ungroup %>%
    mutate(date=as.Date(date)) %>%
    filter(date<=today()-5) %>%
    select(date, fuel_type, sector, crea_value=value, crea_yoy) ->
    crea_yoy

  #add gas all sectors total
  co2 <- co2 %>% filter(fuel_type=='gas', sector != 'all') %>%
    group_by(time, fuel_type) %>%
    summarise(coverage=weighted.mean(coverage, CO2_emissions),
              across(CO2_emissions, sum)) %>%
    mutate(sector='all') %>%
    bind_rows(co2 %>% filter(fuel_type!='gas' | sector != 'all'))

  #identify variable combos with data
  co2 %>% ungroup %>% filter(CO2_emissions>0) %>% distinct(fuel_type, sector) -> grps

  #dates for which to output estimates
  dts <- seq.Date(min(co2$time), max(crea_yoy$date), by='d')

  #expand monthly data to daily and add daily data
  co2 %>% group_by(fuel_type, sector) %>%
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
    summarise(across(c(CO2_hybrid, CO2_emissions, crea_CO2), ~.x[sector=='all']-.x[sector=='electricity'])) %>%
    mutate(sector='others') %>%
    bind_rows(co2_daily %>% filter(fuel_type!='gas' | (! sector %in% c('others', 'all'))))

  #calculate total CO2
  co2_daily <- co2_daily %>%
    filter(fuel_type!='total') %>%
    group_by(date) %>%
    summarise(across(c(CO2_hybrid, CO2_emissions, crea_CO2), sum)) %>%
    mutate(fuel_type='total', sector='all') %>%
    bind_rows(co2_daily %>% filter(fuel_type!='total'))

  #plot
  if(!is.null(diagnostic_folder)){
    plt <- co2_daily %>% filter(year(date)>=2017) %>%
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
  co2_daily %>%
    filter(date < max(implied_cons$date) - lubridate::days(3)) %>% 
    mutate(region='EU',
           unit='t/day') %>%
    mutate(across(c(fuel_type, sector), stringr::str_to_title),
           frequency='daily',
           version=as.character(packageVersion("creaco2tracker"))) %>%
    select(region, date, fuel=fuel_type, sector, unit, frequency, version, value=CO2_hybrid)
}


get_eurostat_cons <- function(diagnostic_folder='diagnostics'){

  consumption_codes = c("nrg_cb_sffm","nrg_cb_oilm","nrg_cb_gasm")
  cons_monthly_raw <- consumption_codes %>% lapply(eurostat::get_eurostat) %>% lapply(eurostat::label_eurostat)
  names(cons_monthly_raw) <- c('coal', 'oil', 'gas')

  consumption_codes_yearly = c("nrg_cb_sff", "nrg_cb_oil", "nrg_cb_gas")
  cons_yearly_raw <- consumption_codes_yearly %>% lapply(eurostat::get_eurostat) %>% lapply(eurostat::label_eurostat)
  names(cons_yearly_raw) <- c('coal', 'oil', 'gas')

  filter_coal_monthly <- function(x){
    x %>%
      filter(grepl('Transformation input|Final consumption.*(industry sector$|other sectors$)', nrg_bal)) %>%
      mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'others'))
  }

  filter_coal_yearly <- function(x){
    x %>%
      filter(grepl(paste('Transformation input - coke ovens',
                         'Transformation input - electricity and heat generation - main activity producer',
                         'Final consumption - industry sector',
                         'Final consumption - other sectors', sep='|'),
                   nrg_bal)) %>%
      mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'others'))
  }

  filter_oil_monthly <- function(x){
    x %>% filter((grepl('Gross inland deliveries.*observed', nrg_bal) & siec=='Oil products') |
                   (grepl('Direct use', nrg_bal) & grepl('Crude oil, NGL', siec))) %>%
      mutate(sector='all')
  }

  filter_oil_yearly <- function(x){
    x %>%
      filter((grepl('Gross inland deliveries.*observed', nrg_bal) & siec=='Oil products') |
                   (grepl('Direct use', nrg_bal) & grepl('Crude oil, NGL.*hydrocarbons$', siec))) %>%
      mutate(sector='all')
  }

  filter_gas_monthly <- function(x){
    x %>%
      filter(grepl('Inland consumption.*observed|Transformation input', nrg_bal),
             unit=='Million cubic metres') %>%
      mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'all'))
  }

  filter_gas_yearly <- function(x){

    # The values we are after aren't indicated in mcm
    gcv <- x %>%
      filter(values>0) %>%
      tidyr::spread(unit, values) %>%
      mutate(gcv=`Terajoule (gross calorific value - GCV)`/`Million cubic metres`) %>%
      pull(gcv) %>%
      median(na.rm=T)

    x %>%
      filter(grepl(paste('Inland consumption.*observed',
                         'Transformation input - electricity and heat generation - main activity producer',
                         sep='|'), nrg_bal),
             grepl('Terajoule', unit)) %>%
      mutate(values=values/gcv,
             unit='Million cubic metres',
             sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'all'))
  }

  deall_gas <- function(x){
    x %>%
      mutate(values=values*ifelse(sector=='electricity', -1, 1)) %>%
      group_by(geo, time, unit, siec) %>%
      summarise_at('values', sum, na.rm=T) %>%
      mutate(sector='others') %>%
      bind_rows(x %>% filter(sector=='electricity')) %>%
      mutate(fuel_type='gas')
  }

  aggregate <- function(x){
    lapply(names(x), function(commodity){
      x[[commodity]] %>%
        group_by(geo, sector, time, unit, siec) %>%
        summarise_at('values', sum, na.rm=T) %>%
        mutate(fuel_type=commodity)
    }) %>%
      `names<-`(names(x))
  }

  cons_monthly <- list(
    coal = filter_coal_monthly(cons_monthly_raw$coal),
    oil = filter_oil_monthly(cons_monthly_raw$oil),
    gas = filter_gas_monthly(cons_monthly_raw$gas) %>% deall_gas()
  ) %>%
    aggregate()

  cons_yearly <- list(
    coal = filter_coal_yearly(cons_yearly_raw$coal),
    oil = filter_oil_yearly(cons_yearly_raw$oil),
    gas = filter_gas_yearly(cons_yearly_raw$gas) %>% deall_gas()
  ) %>%
    aggregate()


  # Visual check that we kept the right sectors for each fuel
  
  if(!is.null(diagnostic_folder)){
    library(rcrea)
    
    plt <- bind_rows(
      do.call(bind_rows, cons_yearly) %>% mutate(source='yearly'),
      do.call(bind_rows, cons_monthly) %>% mutate(source='monthly')) %>%
      
      filter(grepl('European union', geo, T)) %>%
      filter(siec %in% .[.$source=='monthly',]$siec) %>%
      group_by(year=lubridate::year(time), siec, source, fuel_type, sector) %>%
      summarise(values=sum(values)) %>%
      ggplot(aes(year, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel_type~sector, scales='free_y') +
      theme(legend.position = 'bottom') +
      rcrea::scale_y_crea_zero()
    
    ggsave(file.path(diagnostic_folder,'eurostat_annual_vs_monthly_yearly.png'), plot=plt, width=8, height=6, bg='white')
  }


  # Seasonal adjusment
  month_shares <- lapply(cons_monthly, function(cons){

    month_shares <- cons %>%
      group_by(sector, siec, unit, geo, fuel_type, year=lubridate::year(time)) %>%
      mutate(count=n()) %>%
      filter(count==12) %>%
      group_by(sector, siec, unit, geo, fuel_type, month=lubridate::month(time)) %>%
      summarise(values = sum(values, na.rm=T)) %>%
      group_by(sector, siec, unit, geo, fuel_type) %>%
      mutate(month_share = values / sum(values, na.rm=T)) %>%
      mutate(month_share = replace_na(month_share, 1/12),
             month_share = case_when(is.infinite(month_share) ~ 1/12,
                                     T ~ month_share)
      ) %>%
      select(-c(values))

    # Check ~1
    if(!all(month_shares %>%
            group_by(sector, siec, unit, geo, fuel_type) %>%
            summarise(one=round(sum(month_share), 5)) %>%
            pull(one) %>%
            unique() == 1)){stop('Wrong monthly shares')}

    return(month_shares)
  })


  # Apply monthly adjustment
  cons_yearly_monthly <- lapply(names(cons_yearly), function(commodity){
    cons_yearly[[commodity]] %>%
      mutate(year=lubridate::year(time)) %>%
      # filter(time < min(cons$coal$time)) %>%
      inner_join(month_shares[[commodity]]) %>%
      arrange(sector, siec, unit, geo, fuel_type, time) %>%
      mutate(time=as.Date(sprintf('%s-%0d-01', year, month)),
             values=values * month_share)   %>%
      select(-c(year, month, month_share))
  }) %>%
    `names<-`(names(cons_yearly))

  # Combine
  cons_combined <- bind_rows(
    do.call(bind_rows, cons_yearly_monthly) %>% mutate(source='yearly'),
    do.call(bind_rows, cons_monthly) %>% mutate(source='monthly'),
  ) %>%
    ungroup()

  # Visual check
  if(!is.null(diagnostic_folder)){
    plt <- cons_combined %>%
      # group_by(geo, sector, time, unit, siec, fuel_type) %>%
      # arrange(source) %>%
      # slice(1) %>%
      # ungroup() %>%
      filter(grepl('European union', geo, T)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel_type~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero()
    
    ggsave(file.path(diagnostic_folder,'eurostat_annual_vs_monthly_monthly.png'), plot=plt, width=8, height=6, bg='white')
    
    plt <- cons_combined %>%
      group_by(geo, sector, time, unit, siec, fuel_type) %>%
      arrange(source) %>%
      slice(1) %>%
      ungroup() %>%
      filter(grepl('European union', geo, T)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel_type~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero()
    
    ggsave(file.path(diagnostic_folder,'eurostat_combined.png'), plot=plt, width=8, height=6, bg='white')
  }
  

  # Remove overlaps
  cons <- cons_combined  %>%
    group_by(geo, sector, time, unit, siec, fuel_type) %>%
    arrange(source) %>%
    # Keep monthly when both are available
    slice(1) %>%
    ungroup() %>%
    select(-c(source)) %>%
    split(.$fuel_type)

  return(cons)
}


get_entsoe <- function() {
  
  pwr <- read_csv('https://api.energyandcleanair.org/power/generation?date_from=2016-01-01&aggregate_by=country,source,date&format=csv&region=EU')
  
  #Power generation by source plus total Calvin plot
  pwr <- pwr %>%
    filter(source!='Total') %>%
    group_by(region, country, date) %>%
    dplyr::summarise_at("value_mw", sum, na.rm=T) %>%
    mutate(source='Total') %>%
    bind_rows(pwr %>% filter(source!='Total'))
  
  #add EU total
  pwr <- pwr %>%
    filter(country!='EU total') %>%
    group_by(date, source) %>%
    filter(region=='EU') %>%
    dplyr::summarise_at("value_mw", sum, na.rm=T) %>%
    mutate(country='EU total') %>%
    bind_rows(pwr %>% filter(country!='EU total'))  
  
  #add total generation
  
  
  return(pwr)
}


get_entsog <- function(start_date='2016-01-01', end_date=lubridate::today(),
                       types='consumption') {
  #gas data
  dates <- seq.Date(ymd(start_date), ymd(end_date), by='30 days')
  
  data=list()
  for(i in seq_along(dates)[-1]) {
    message('getting data...', dates[i])
    read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s&date_to=%s&type=%s', 
                     dates[i-1], dates[i], types)) ->
      data[[i]]
    Sys.sleep(2)
  }
  data %>% bind_rows()
}



get_entsog_old <- function(years=seq(2016, lubridate::year(lubridate::today()))) {
  #gas data
  
  years %>% lapply(function(yr) {
    Sys.sleep(2)
    paste0("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=",yr,
           "-01-01&date_to=",yr,"-12-31&commodity=natural_gas") %>% 
    read_csv()
  }) %>% bind_rows ->ng_all
  
  types <- c('distribution','consumption','storage_entry','storage_exit','crossborder','production')
  entsog <- years %>%
    pbapply::pblapply(function(x){
      Sys.sleep(2);
      read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')))}) %>%
    bind_rows()
  
  #Gas imports + production+ storage+ implied consumption
  inflows <- ng_all %>%
    filter(commodity_origin_country %in% c('Algeria', 'Azerbaijan', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia',
                                           'United Kingdom', 'Norway')) %>%
    group_by(across(c(starts_with('destination'), date))) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='imports')
  
  storage_changes <- entsog %>% filter(type %in% c('storage_entry','storage_exit')) %>%
    mutate(value_m3 = value_m3 * ifelse(type=='storage_exit', -1, 1)) %>%
    group_by(across(c(starts_with('destination'), date))) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='storage drawdown')
  
  implied_cons <- entsog %>% filter(type == 'production') %>%
    filter(commodity_origin_country %in% c('Algeria', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia',
                                           'United Kingdom', 'Norway')) %>%
    bind_rows(inflows, storage_changes) %>%
    group_by(across(c(starts_with('destination'), date))) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='consumption')
  
  bind_rows(inflows, storage_changes, implied_cons, entsog %>% filter(type == 'production'))
}
