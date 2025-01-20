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

get_gas_demand <- function(diagnostics_folder='diagnostics/gas_demand', verbose=F){

  years <- seq(2018, lubridate::year(lubridate::today()))

  create_dir(diagnostics_folder)

  # Estimate with two different methods
  message('Getting gas demand from Consumption + Distribution ENTSOG points')
  consdist <- get_gas_demand_consdist(years=years, verbose=verbose)
  message('Getting gas demand from storage,crossborder,production')
  apparent <- get_gas_demand_apparent(years=years, verbose=verbose)
  message('Getting gas demand from storage,crossborder,production using AGSI for storage')
  apparent_w_agsi <- get_gas_demand_apparent(years=years, use_agsi_for_storage=T, verbose=verbose)

  # Keep the best ones, and only those that match quality criteria
  gas_demand <- keep_best(consumption=bind_rows(consdist,
                                                apparent,
                                                apparent_w_agsi),
            min_comparison_points = 12,
            diagnostics_folder=diagnostics_folder,
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
get_gas_demand_consdist <- function(years, verbose=F){

  entsog <- creahelpers::api.get("https://api.russiafossiltracker.com/v0/entsogflow",
                                 date_from=glue("{min(years)}-01-01}"),
                                 date_to=glue("{max(years)}-12-31}"),
                                 type='consumption,distribution',
                                 split_by='year',
                                 verbose=verbose
                                 )




  consdist <- entsog %>%
    group_by(iso2=destination_iso2, date) %>%
    summarise(value_m3=-sum(value_m3, na.rm=T)) %>%
    arrange(date) %>%

    # Interpolate missing data
    tidyr::complete(date=seq.Date(min(as.Date(date)),
                                  max(as.Date(date)), by='day'),
                    fill=list(value_m3=NA)) %>%
    mutate(value_m3=zoo::na.approx(value_m3)) %>%
    ungroup() %>%

    # Fill with 0 before and after each country's reporting period
    tidyr::complete(date=seq.Date(min(as.Date(entsog$date)),
                                  max(as.Date(entsog$date)), by='day'),
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
get_gas_demand_apparent <- function(years, use_agsi_for_storage=F, verbose=F){


  eu_iso2 <- setdiff(countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")], "GB")

  entsog <- creahelpers::api.get("https://api.russiafossiltracker.com/v0/entsogflow",
                                 date_from=glue("{min(years)}-01-01}"),
                                 date_to=glue("{max(years)}-12-31}"),
                                 type='storage,crossborder,production',
                                 split_by='year',
                                 verbose=verbose)


  # Fill missing data
  entsog <- entsog %>%
    select(destination_iso2, departure_iso2, date, type, value_m3) %>%
    arrange(date) %>%
    group_by(destination_iso2, departure_iso2, type) %>%
    # Interpolate missing data
    tidyr::complete(date=seq.Date(min(as.Date(date)),
                                  max(as.Date(date)), by='day'),
                    fill=list(value_m3=NA)) %>%
    mutate(value_m3=zoo::na.approx(value_m3)) %>%
    ungroup()



  if(use_agsi_for_storage){
    # In some instances, AGSI is better than ENTSOG
    # Well, at least for Austria which has no storage data in ENTSOG
    storage_drawdown <- agsi.get_storage_change(date_from=min(as.Date(entsog$date)),
                                                date_to=max(as.Date(entsog$date)),
                                                iso2=eu_iso2,
                                                verbose=verbose
                                                )
    entsog <- entsog %>%
      filter(type!='storage') %>%
      bind_rows(
        storage_drawdown %>%
          select(destination_iso2=iso2,
                 date,
                 value_m3) %>%
          mutate(type='storage')
      )
  }


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
    tidyr::complete(date=seq.Date(min(as.Date(entsog$date)), max(as.Date(entsog$date)), by='day'),
                    fill=list(value_m3=0)) %>%
    mutate(method=ifelse(use_agsi_for_storage, 'apparent_agsi', 'apparent'))

  return(apparent)
}


get_eurostat_gas <- function(years){
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
    select(iso2=geo, date=TIME_PERIOD, value_m3=values, type) %>%
    mutate(value_m3=value_m3*1e6,
           unit='m3',
           iso2=recode(iso2, 'UK'='GB', 'EU27_2020'='EU'),
           method='eurostat')
}



keep_best<- function(consumption,
                     diagnostics_folder,
                     min_comparison_points=24,
                     min_r2=0.95, max_rrse=0.4){

  eurostat = get_eurostat_gas() %>% filter(type=='consumption')
  rsq <- function (x, y) cor(x, y) ^ 2
  min_start <- min(consumption$date)
  max_start = max(eurostat$date) - months(min_comparison_points - 1)

  # We test the correlation for several starting dates
  # The older the better, but often it becomes accurate only after a certain date
  date_froms <- seq.Date(as.Date(min_start), as.Date(max_start), by='month')
  consumption_date_to <- consumption %>%
    group_by(iso2, method) %>%
    summarise(date_to=max(date))

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
    left_join(consumption_date_to) %>%
    filter(valid, count >= min_comparison_points) %>%
    group_by(iso2) %>%
    arrange(date_from) %>%
    # Avoid initial gaps
    filter(rrse < lead(rrse) + 0.02) %>%
    # Avoid sub-optimal
    filter(rrse < min(rrse) + 0.1) %>%
    # Avoid those that aren't updated
    filter(date_to > max(date_to) - lubridate::days(10)) %>%
    # Take earlier one
    filter(date_from==min(date_from)) %>%
    filter(r2==max(r2)) %>%
    # Sometimes apparent strictly equivalent to apparent_asgi
    distinct(iso2, .keep_all = T)

  if(!is.null(diagnostics_folder)){

    plt_data <- best %>%
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
                           levels=c('apparent', 'apparent_agsi', 'consdist', 'eurostat'),
                           labels=c('CREA estimate',
                                    'CREA estimate',
                                    'CREA estimate',
                                    'Eurostat')))
    plt <- ggplot(plt_data) +
      geom_line(aes(date, value_m3/1e6, col=method, size=method)) +
      facet_wrap(~country, scales='free_y',
                 ncol=3) +
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
      labs(title='Fossil gas consumption - CREA estimate vs Eurostat',
           subtitle='Million cubic meter per month',
           x=NULL,
           y=NULL,
           color=NULL,
           size=NULL,
           caption='Note: Only the best estimate is shown for each country.') +
      rcrea::theme_crea() +
      theme(legend.position = 'bottom') +
      guides(size=guide_legend(nrow=1),
             color=guide_legend(nrow=1))
    plt
    ggsave(filename=file.path(diagnostics_folder, 'gas_consumption_estimates.png'),
           plot=plt, width=10, height=10, bg='white')

    # Export a version for Flourish
    plt_data %>%
      tidyr::spread(method, value_m3) %>%
      write_csv(file.path(diagnostics_folder, 'gas_consumption_estimates_wide.csv'))
  }

  best %>%
    filter(valid) %>%
    select(-c(r2, rrse)) %>%
    left_join(consumption) %>%
    ungroup()
}

