## ---------------------------
##
## Script name: get_gas_demand.R
##
## Purpose of script: Compute apparent gas consumption from ENTSOG data stored in CREA's DB
## and make it ready for upload in CREA's DB.
##
## Author: Hubert Thieriot
##
## Date Created: 2022-12-26
##
## Copyright (c) CREA, 2018
## Email: hubert@energyandcleanair.org
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

get_gas_demand <- function(diagnostic_folder='diagnostics'){
  years <- seq(2018, lubridate::year(lubridate::today()))
  
  # Estimate with two different methods
  consdist <- get_gas_demand_consdist(years=years)
  apparent <- get_gas_demand_apparent(years=years)
  
  # Keep the best ones, and only those that match quality criteria
  keep_best(consumption=bind_rows(consdist,apparent),
            min_comparison_points = 12,
            diagnostic_folder=diagnostic_folder,
            min_r2=0.95,
            max_rrse=0.3)

  
  # Keep the longest one per country
  gas_demand %>%
    select(region_id=iso2, date, value=value_m3) %>%
    mutate(fuel='fossil_gas',
           sector='total',
           data_source='crea',
           unit='m3',
           frequency='daily',
           region_type=case_when(region_id=='EU' ~ 'region', T ~ 'iso2'))
}


#' Get national gas demand based on Consumption + Distribution ENTSOG points
#'
#' @param years 
#'
#' @return
#' @export
#'
#' @examples
get_gas_demand_consdist <- function(years){
  
  
  types <- c('consumption','distribution')
  
  entsog <- years %>%
    pbapply::pblapply(function(x){
      Sys.sleep(2)
      read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')),
               show_col_types = FALSE)}) %>%
    bind_rows()
  
  consdist <- entsog %>%
    group_by(iso2=destination_iso2, date) %>%
    summarise(value_m3=-sum(value_m3, na.rm=T)) %>%
    ungroup() %>%
    tidyr::complete(date=seq.Date(min(entsog$date), max(entsog$date), by='day'),
                    fill=list(value_m3=0)) %>%
    mutate(method='consdist')
    
  return(consdist)
}



#' Get national gas demand based on Net imports + production + storage drawdown
#'
#' @param years 
#'
#' @return
#' @export
#'
#' @examples
get_gas_demand_apparent <- function(years){
  
  
  eu_iso2 <- setdiff(countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")], "GB")
  types <- c('storage','crossborder','production')
  
  entsog <- years %>%
    pbapply::pblapply(function(x){
      Sys.sleep(2)
      read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')),
               show_col_types = FALSE)}) %>%
    bind_rows()
  
  # We're missing some flows within EU, so we take at the borders only
  # i.e. not doing this: entsog_eu <- entsog %>%
  #   mutate(destination_iso2=destination_region,
  #          departure_iso2=departure_region)
  ex_eu <- c('NO', 'GB', 'UK', 'TR', 'lng',
             'UA', 'RU', 'AZ', 'DZ', 'LY', 'BY', 'MA')
  entsog_eu <- entsog %>%
    mutate(destination_iso2=case_when(destination_iso2 %in% ex_eu ~ 'Others',
                                      T~'EU'),
           departure_iso2=case_when(departure_iso2 %in% ex_eu ~ 'Others',
                                    T~'EU'))
   
  entsog_all <- bind_rows(entsog, entsog_eu)
  
  # Implied consumption = net imports + production + storage_drawdown
  imports <- entsog_all %>%
    filter(type %in% c('crossborder')) %>%
    filter(departure_iso2 != destination_iso2) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='imports')
  
  minus_exports <- entsog_all %>%
    filter(type %in% c('crossborder')) %>%
    filter(departure_iso2 != destination_iso2) %>%
    mutate(tmp=departure_iso2,
           departure_iso2=destination_iso2,
           destination_iso2=tmp,
           value_m3=-value_m3) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='minus_exports')
  
  net_imports <- bind_rows(imports, minus_exports) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='net_imports')
  
  storage_drawdown <- entsog_all %>%
    filter(type %in% c('storage')) %>%
    # mutate(value_m3 = value_m3) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='storage_drawdown')
  
  production <- entsog_all %>%
    filter(type == 'production') %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='production')
  
  apparent <- bind_rows(imports, minus_exports, storage_drawdown, production) %>%
    group_by(iso2=destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    ungroup() %>%
    tidyr::complete(date=seq.Date(min(entsog$date), max(entsog$date), by='day'),
                    fill=list(value_m3=0)) %>%
    mutate(method='apparent')
  
  return(apparent)
}


get_eurostat <- function(years){
  gas_consumption <- eurostat::get_eurostat("nrg_cb_gasm")
  gas_consumption %>%
    filter(unit=='MIO_M3',
           siec=="G3000") %>%
    mutate(type=recode(nrg_bal,
                       IC_OBS='consumption',
                       IPRD='production',
                       IMP='imports',
                       EXP='minus_exports',
                       STK_CHG_MG='storage',
                       .default=NA_character_,
    )) %>%
    filter(!is.na(type)) %>%
    mutate(values=ifelse(type %in% c('minus_exports', 'storage'), -values, values)) %>%
    select(iso2=geo, date=time, value_m3=values, type) %>%
    mutate(value_m3=value_m3*1e6,
           unit='m3',
           iso2=recode(iso2, 'UK'='GB', 'EU27_2020'='EU'),
           method='eurostat')
}



keep_best<- function(consumption,
                     diagnostic_folder,
                     min_comparison_points=24,
                     min_r2=0.95, max_rrse=0.4){
  
  eurostat = get_eurostat() %>% filter(type=='consumption')
  rsq <- function (x, y) cor(x, y) ^ 2
  min_start <- min(consumption$date)
  max_start = max(eurostat$date) - months(min_comparison_points - 1)
  
  # We test the correlation for several starting dates
  # The older the better, but often it becomes accurate only after a certain date
  date_froms <- seq.Date(as.Date(min_start), as.Date(max_start), by='month')
  
  consumption_monthly <- consumption %>%
    filter(date < max(lubridate::floor_date(date, 'month'))) %>%
    group_by(iso2, method, date=lubridate::floor_date(date, 'month')) %>%
    summarise(value_m3=sum(value_m3)) %>%
    ungroup() %>%
    tidyr::complete(date, method, iso2, fill=list(value_m3=0))
  
  bests <- pbapply::pblapply(date_froms, function(date_from){
    consumption_monthly %>%
      filter(date >= date_from) %>%
      left_join(eurostat %>%
                  rename(value_m3_eurostat=value_m3) %>%
                  select(-c(method))) %>%
      filter(!is.na(value_m3_eurostat)) %>%
      group_by(iso2, method) %>%
      summarise(r2=rsq(value_m3_eurostat, value_m3),
                rrse=Metrics::rrse(value_m3_eurostat, value_m3),
                count=n()) %>%
      mutate(valid=(rrse < max_rrse) & (r2 > min_r2)) %>%
      group_by(iso2) %>%
      top_n(n=1, wt=r2) %>%
      ungroup() %>%
      arrange(desc(r2)) %>%
      mutate(date_from=!!date_from)
  }) %>%
    do.call(bind_rows, .)
  
  best <- bests %>%
    filter(valid, count >= min_comparison_points) %>%
    group_by(iso2) %>%
    arrange(date_from) %>%
    # Avoid initial gaps
    filter(rrse < lead(rrse + 0.02)) %>%
    filter(date_from==min(date_from)) %>%
    filter(r2==max(r2))
  
  if(!is.null(diagnostic_folder)){
    plt <- best %>%
      ungroup() %>%
      left_join(consumption_monthly) %>%
      filter(date >= date_from) %>%
      bind_rows(eurostat %>%
                  filter(date <= max(consumption_monthly$date),
                         iso2 %in% best$iso2)) %>%
      group_by(iso2) %>%
      filter(date >= min(date_from, na.rm=T)) %>%
      ungroup() %>%
      mutate(country = countrycode::countrycode(iso2, 'iso2c', 'country.name',
                                                custom_match = c('EU'='EU'))) %>%
      mutate(method=factor(method,
                           levels=c('apparent', 'consdist', 'eurostat'),
                           labels=c('CREA estimate',
                                    'CREA estimate',
                                    'Eurostat'))) %>%
      ggplot() +
      geom_line(aes(date, value_m3/1e6, col=method, size=method)) +
      facet_wrap(~country, scales='free_y') +
      rcrea::scale_y_crea_zero() +
      scale_color_manual(values=c('Apparent consumption'=rcrea::pal_crea[['Turquoise']],
                                  'Reported consumtion'=rcrea::pal_crea[['Blue']],
                                  'CREA estimate'=rcrea::pal_crea[['Blue']],
                                  'Eurostat'=rcrea::pal_crea[['Red']]
                                     )) +
      scale_size_manual(values=c('Apparent consumption'=1,
                                  'Reported consumtion'=1,
                                 'CREA estimate'=1,
                                  'Eurostat'=0.3
      )) +
      labs(title='Fossil gas consumption',
           subtitle='CREA estimate vs Eurostat',
           x=NULL,
           y='mcm / month',
           color=NULL,
           size=NULL,
           caption='Note: Only the best estimate is shown for each country.') +
      rcrea::theme_crea() 
    plt
    ggsave(filename=file.path(diagnostic_folder, 'gas_consumption_estimates.png'),
           plot=plt, width=10, height=6, bg='white')
  }
  
  best %>%
    filter(valid) %>%
    select(-c(r2, rrse)) %>%
    left_join(consumption) %>%
    ungroup()
}
  

remove_power_sector <- function(gas_demand){
  
  region_ids = unique(gas_demand$region_id)
  
  pwr <- download_electricity(region_id=region_ids,
                              data_source='entsoe')

  thermal_eff <- download_thermal_efficiency(region_id=region_ids)
  
  gas_for_power <- pwr %>%
    left_join(thermal_eff %>% select(region_id, eff=value)) %>%
    mutate(value_for_power_m3=value_mwh * 1e3 / eff / ncv_kwh_m3) %>%
    select(region_id, date, value_for_power_m3)
  
  d <- gas_demand %>%
    left_join(gas_for_power)
}



