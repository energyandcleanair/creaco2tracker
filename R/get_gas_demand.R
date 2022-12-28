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
  gas_demand <- keep_best(consumption=bind_rows(consdist,apparent),
                          diagnostic_folder=diagnostic_folder,
                          min_r2=0.95,
                          max_rrse=0.3)
  
  gas_demand %>%
    ungroup() %>%
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
      read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')))}) %>%
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
      read_csv(sprintf('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=%s-01-01&date_to=%s-12-31&type=%s', x, x, paste0(types, collapse=',')))}) %>%
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


keep_best<- function(consumption, diagnostic_folder, min_r2=0.95, max_rrse=0.4){
  
  eurostat = get_eurostat() %>% filter(type=='consumption')
  
  consumption_monthly <- consumption %>%
    group_by(iso2,
             method,
             date=lubridate::floor_date(date, 'month')) %>%
    summarise(value_m3=sum(value_m3)) %>%
    ungroup() %>%
    tidyr::complete(date, method, iso2, fill=list(value_m3=0))
  
  
  rsq <- function (x, y) cor(x, y) ^ 2
  
  best <- consumption_monthly %>%
    left_join(eurostat %>%
                rename(value_m3_eurostat=value_m3) %>%
                select(-c(method))) %>%
    filter(!is.na(value_m3_eurostat)) %>%
    group_by(iso2, method) %>%
    summarise(r2=rsq(value_m3_eurostat, value_m3),
              rrse=Metrics::rrse(value_m3_eurostat, value_m3)) %>%
    mutate(valid=(rrse < max_rrse) & (r2 > min_r2)) %>%
    group_by(iso2) %>%
    top_n(n=1, wt=r2) %>%
    ungroup()
  
  if(!is.null(diagnostic_folder)){
    plt <- best %>%
      ungroup() %>%
      left_join(consumption_monthly) %>%
      bind_rows(eurostat %>%
                  filter(date >= min(consumption_monthly$date),
                         date <= max(consumption_monthly$date),
                         iso2 %in% best$iso2)) %>%
      ungroup() %>%
      ggplot() +
      geom_line(aes(date, value_m3/1e9, col=method)) +
      facet_wrap(~iso2, scales='free_y') +
      # rcrea::scale_y_crea_zero() +
      labs(title='Gas consumption',
           subtitle='Estimate vs Eurostat',
           x=NULL,
           y='m3/month') +
      rcrea::theme_crea() 
    
    ggsave(filename=file.path(diagnostic_folder, 'gas consumption estimates.png'),
           plot=plt, width=8, height=6, bg='white')
  }
  
  best %>%
    filter(valid) %>%
    select(-c(r2, rrse)) %>%
    left_join(consumption) %>%
    ungroup()
}
  