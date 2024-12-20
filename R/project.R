
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
    # project_until_now_coal_others(eurostat_indprod=eurostat_indprod, dts_month=dts_month) %>%
    project_until_now_yoy(dts_month=dts_month)
}


#' Project til now using a linear model with proxy data (e.g. elec, gas or industrial production index)
#'
#' @param co2
#' @param proxy a data frame with iso2, date, value_proxy (optional value_proxy2, etc.), fuel, sector
#' @param dts_month
#' @param last_years number of years to use for training the model
#' @param min_r2 minimum R2 for model to be considered good enough. If not good enough, data is left as NA
#' @param force_overwrite if TRUE, overwrite existing data with predicted one. Mostly for debugging.
#'
#' @return
#' @export
#'
#' @examples
project_until_now_lm <- function(co2, proxy, dts_month, last_years=5, min_r2=0.9, force_overwrite=F){

  co2 %>%
    group_by(iso2, geo, fuel, sector) %>%
    expand_dates('date', dts_month) %>%
    arrange(desc(date)) %>%
    ungroup() %>%
    group_by(iso2, geo, fuel, sector) %>%
    group_modify(function(df, keys, ...) {

      iso2 <- keys %>% add_iso2() %>% pull(iso2)
      fuel <- unique(keys$fuel)
      geo <- unique(keys$geo)
      sector <- unique(keys$sector)

      value_proxy_cols <- grep('value_proxy', colnames(proxy), value=T)
      data <- df %>%
        left_join(keys %>%
                    left_join(proxy %>%
                                select_at(c('iso2', 'date', value_proxy_cols, 'fuel', 'sector')),
                              by=c('iso2', 'fuel', 'sector')),
                  by=c('date'))

      data_training <- data %>%
        filter(value_co2_tonne>0,
               date >= max(date) - lubridate::years(last_years)) %>%
        # only positive values in value_proxy_cols
        filter_at(value_proxy_cols, all_vars(.>0))

      # If all value_proxy_cols are NA, stop
      if(data_training %>% select_at(value_proxy_cols) %>% filter_all(any_vars(!is.na(.))) %>% nrow == 0){
        message(glue("{iso2} - {fuel}: Missing proxy data. Leaving as such"))
        return(df)
      }

      if(sum(data_training$value_co2_tonne)==0){
        return(df)
      }

      formula_str <- paste0(value_proxy_cols, collapse=' + ')
      model <- lm(data=data_training, formula= paste0('value_co2_tonne ~ 0+', formula_str))

      r2 <- summary(model)$adj.r.squared
      if(r2 < min_r2){
        log_warn(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy not good enough (R2={round(r2,2)}). Leaving as NA."))
        return(df)
      }else{
        log_success(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy OK (R2={round(r2,2)})"))
      }

      if(force_overwrite){
        df$value_co2_tonne <- predict(model, data)
      }else{
        df$value_co2_tonne <- coalesce(df$value_co2_tonne, predict(model, data))
      }
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
    group_by(iso2, geo, fuel, sector) %>%
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
    mutate(fuel=case_when(
      source=='Coal' ~ 'coal',
      source=='Fossil Gas' ~ 'gas',
      T ~ NA_character_
    )) %>%
    filter(!is.na(fuel)) %>%
    group_by(iso2, sector=SECTOR_ELEC, fuel, date=floor_date(date, 'month')) %>%
    summarise(value_proxy=sum(value_mwh), .groups = 'drop') %>%
    arrange(desc(date))


  project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=last_years, min_r2=min_r2)

}


project_until_now_gas <- function(co2, gas_demand, dts_month, last_years=5, min_r2=0.85){

  proxy <- gas_demand %>%
    rename(iso2) %>%
    filter(iso2 %in% co2$iso2) %>%
    ungroup() %>%
    filter(unit=='m3') %>%
    group_by(iso2, date=floor_date(date, 'month')) %>%
    summarise(value_proxy=sum(value), .groups = 'drop') %>%
    arrange(desc(date)) %>%
    mutate(fuel='gas', sector=SECTOR_ALL)

  project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=last_years, min_r2=min_r2)
}

project_until_now_coal_others <- function(co2, eurostat_indprod, dts_month, last_years=5, min_r2=0.85){

  # Cement, Steel, Glass, Coke
  products_ind <-
    list(
      # "C19"="x",
      "C191"="coke",
      "C192"="refined",
      # "C201"="chemicals",
      # "C2011"="x",
      "C2012"="x",
      "C2013"="x",
      # "C2014"="x",
      "C2015"="x",
      "C2016"="x",
      "C2017"="x",



      "C2015"="chemicals_fertilisers",
      "C202"="agrochemicals",
      # "C204"="cosmetics",
      # "C211"="pharmaceutical",
      "C221"="rubber",
      # "C222"="plastics",
      # "C23"="nonmetallic",
      "C235"="cement",
      "C241"="iron",
      "C231"="glass"
      # "C23",
      # "C24",
      # "C25"
    )


  indprod <- eurostat_indprod %>%
    filter(nace_r2_code %in% names(products_ind)) %>%
    filter(unit=="Index, 2021=100",
           grepl("Calendar adjusted ", s_adj))


  # See if model works for EU first
  proxy <- indprod %>%
    filter(iso2 %in% co2$iso2) %>%
    ungroup() %>%
    select(iso2, date=time, nace_r2_code, value=values) %>%
    pivot_wider(names_from=nace_r2_code, values_from=value, names_prefix="value_proxy_") %>%
    filter(year(date) >= year(today()) - last_years) %>%
    mutate(
      fuel='coal',
      sector=SECTOR_OTHERS
    )

  co2_filled <- project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=20, min_r2=min_r2,
                                     force_overwrite=T)

  bind_rows(co2 %>% mutate(type="original"),
            co2_filled %>% mutate(type="filled")) %>%
    filter(sector==SECTOR_OTHERS, fuel=='coal') %>%
    filter(date >= "2023-01-01") %>%
    ggplot(aes(date, value_co2_tonne, col=type)) +
    geom_line()

}


