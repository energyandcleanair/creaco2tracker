
#' Project CO2 emissions until today using various proxies
#'
#' @param co2
#' @param pwr
#' @param gas_demand
#'
#' @return
#' @export
#'
#' @examples
project_until_now <- function(co2, pwr, gas_demand){

  dts_month <- seq.Date(min(co2$date), today() %>% 'day<-'(1), by='month')

  co2 %>%
    split_gas_to_elec_all() %>%
    project_until_now_elec(pwr_demand=pwr, dts_month=dts_month) %>%
    project_until_now_gas(gas_demand=gas_demand, dts_month=dts_month) %>%
    project_until_now_yoy(dts_month=dts_month)
}


#' Project til now using a linear model with proxy data (e.g. elec, gas or industrial production index)
#'
#' @param co2
#' @param proxy
#' @param dts_month
#' @param last_years number of years to use for training the model
#' @param min_r2 minimum R2 for model to be considered good enough. If not good enough, data is left as NA
#'
#' @return
#' @export
#'
#' @examples
project_until_now_lm <- function(co2, proxy, dts_month, last_years=5, min_r2=0.9){

  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    expand_dates('date', dts_month) %>%
    arrange(desc(date)) %>%
    ungroup() %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    group_modify(function(df, keys, ...) {

      iso2 <- keys %>% add_iso2() %>% pull(iso2)
      fuel_type <- unique(keys$fuel_type)
      geo <- unique(keys$geo)
      sector <- unique(keys$sector)

      data <- df %>%
        left_join(keys %>%
                    left_join(proxy %>%
                                select(iso2, date, value_proxy, fuel_type, sector),
                              by=c('iso2', 'fuel_type', 'sector')),
                  by=c('date'))

      data_training <- data %>%
        filter(value_co2_tonne>0,
               value_proxy>0,
               date >= max(date) - lubridate::years(last_years))

      if(all(is.na(data_training$value_proxy))){
        # message(glue("{iso2} - {fuel_type}: Missing proxy data. Leaving as NA."))
        return(df)
      }

      if(sum(data_training$value_co2_tonne)==0){
        return(df)
      }

      model <- lm(data=data_training, formula= value_co2_tonne ~ value_proxy + 0)

      r2 <- summary(model)$adj.r.squared
      if(r2 < min_r2){
        log_warn(glue("{iso2} - {fuel_type} {sector}: Model CO2 ~ proxy not good enough (R2={round(r2,2)}). Leaving as NA."))
        return(df)
      }else{
        log_success(glue("{iso2} - {fuel_type} {sector}: Model CO2 ~ proxy OK (R2={round(r2,2)})"))
      }

      df$value_co2_tonne <- coalesce(df$value_co2_tonne, predict(model, data))
      df
    }) %>%
    ungroup()
}


#' Fill CO2 emission data assuming deviation from n1-year average stays same as in last n2 months of data
#'
#' @param co2
#' @param dts_month
#' @param last_years
#' @param last_months
#'
#' @return
#' @export
#'
#' @examples
project_until_now_yoy <- function(co2, dts_month, last_years=3, last_months=3){

  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    expand_dates('date', dts_month) %>%
    arrange(date) %>%
    group_modify(function(df, ...) {

      df %<>%
        group_by(month=month(date)) %>%
        mutate(mean3y = value_co2_tonne %>%
                 lag %>%
                 zoo::rollapplyr(last_years, mean, na.rm=F, fill=NA),
               yoy = value_co2_tonne / mean3y - 1) %>%
        ungroup %>%
        select(-month)

      latest_yoy <- df$yoy %>%
        zoo::rollapplyr(last_months, mean, na.rm=F) %>%
        na.omit %>%
        tail(1)

      latest_data <- max(df$date[!is.na(df$value_co2_tonne)])

      df %>% mutate(value_co2_tonne = ifelse(date > latest_data,
                                           mean3y * (1+latest_yoy),
                                           value_co2_tonne)) %>%
        select(-c(mean3y, yoy))
    }) %>%
    ungroup()
}


project_until_now_elec <- function(co2, pwr_demand, dts_month, last_years=5, min_r2=0.85){

  proxy <- pwr_demand %>%
    filter(iso2 %in% co2$iso2) %>%
    ungroup() %>%
    mutate(fuel_type=case_when(
      source=='Coal' ~ 'coal',
      source=='Fossil Gas' ~ 'gas',
      T ~ NA_character_
    )) %>%
    filter(!is.na(fuel_type)) %>%
    group_by(iso2, sector=SECTOR_ELEC, fuel_type, date=floor_date(date, 'month')) %>%
    summarise(value_proxy=sum(value_mwh), .groups = 'drop') %>%
    arrange(desc(date))


  project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=last_years, min_r2=min_r2)

}


project_until_now_gas <- function(co2, gas_demand, dts_month, last_years=5, min_r2=0.85){

  proxy <- gas_demand %>%
    rename(iso2=region_id) %>%
    filter(iso2 %in% co2$iso2) %>%
    ungroup() %>%
    filter(unit=='m3') %>%
    group_by(iso2, date=floor_date(date, 'month')) %>%
    summarise(value_proxy=sum(value), .groups = 'drop') %>%
    arrange(desc(date)) %>%
    mutate(fuel_type='gas', sector=SECTOR_ALL)

  project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=last_years, min_r2=min_r2)
}


