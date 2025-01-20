#' Project CO2 emissions until today using various proxies
#'
#' @param co2
#' @param pwr_demand
#' @param gas_demand
#'
#' @return
#' @export
#'
#' @examples
project_until_now <- function(co2, pwr_demand, gas_demand, eurostat_indprod){

  dts_month <- seq.Date(min(co2$date), today() %>% 'day<-'(1), by='month')

  co2 %>%
    split_gas_to_elec_all() %>%
    project_until_now_elec(pwr_demand=pwr_demand, dts_month=dts_month) %>%
    project_until_now_gas(gas_demand=gas_demand, dts_month=dts_month) %>%
    project_until_now_coal_others(eurostat_indprod=eurostat_indprod, dts_month=dts_month) %>%
    project_until_now_oil(dts_month=dts_month) %>%

    # Before we apply ETS projection (which has large attached uncertainty)
    # We give a chance to other = total - sum(other sectors)
    # It applies to coal and oil so far
    detotalise_co2() %>%

    # Then run projections
    project_until_now_forecast(dts_month=dts_month) %>%

    # And re-detotalise, since data from total and other sectors may now overlap again
    detotalise_co2()
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
project_until_now_lm <- function(co2, proxy, dts_month, last_years=5, min_r2=0.9, force_overwrite=F, verbose=F){

  co2 %>%
    group_by(iso2, geo, fuel, sector, unit) %>%
    expand_dates('date', dts_month) %>%
    arrange(desc(date)) %>%
    ungroup() %>%
    group_by(iso2, geo, fuel, sector, unit) %>%
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
        filter(value>0,
               date >= max(date) - lubridate::years(last_years)) %>%
        # only positive values in value_proxy_cols
        filter_at(value_proxy_cols, all_vars(.>0))

      # If all value_proxy_cols are NA, stop
      if(data_training %>% select_at(value_proxy_cols) %>% filter_all(any_vars(!is.na(.))) %>% nrow == 0){
        # message(glue("{iso2} - {fuel}: Missing proxy data. Leaving as such"))
        return(df)
      }

      if(sum(data_training$value)==0){
        return(df)
      }

      formula_str <- paste0(value_proxy_cols, collapse=' + ')
      model <- lm(data=data_training, formula= paste0('value ~ 0+', formula_str))
      if(verbose){
        print(summary(model))
      }

      r2 <- summary(model)$adj.r.squared
      if(r2 < min_r2){
        log_warn(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy not good enough (R2={round(r2,2)}). Leaving as NA."))
        return(df)
      }else{
        log_success(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy OK (R2={round(r2,2)})"))
      }

      if(force_overwrite){
        df$value <- predict(model, data)
      }else{
        df$value <- coalesce(df$value, predict(model, data))
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
      "C191"="coke",
      "C192"="refined",
      "C2013"="x",
      "C221"="rubber",
      "C235"="cement",
      "C241"="iron",
      "C231"="glass"
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


  #TODO
  # Discuss internally the discrepancy within hard coal
  # co2_filled_overwritten <- project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=20, min_r2=min_r2,
  #                                                force_overwrite=T)
  # bind_rows(co2 %>% mutate(type="original"),
  #           co2_filled %>% mutate(type="filled")) %>%
  #   filter(sector==SECTOR_OTHERS, fuel=='coal') %>%
  #   filter(date >= "2000-01-01") %>%
  #   ggplot(aes(date, value, col=type)) +
  #   geom_line() +
  #   scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y") +
  #   rcrea::scale_y_crea_zero()
  project_until_now_lm(co2, proxy, dts_month=dts_month, last_years=20, min_r2=min_r2)

}


#' Complete latest months of EU Oil using European countries latest data
#'
#' @param co2
#' @param dts_month
#' @param last_years
#' @param min_r2
#'
#' @return
#' @export
#'
#' @examples
project_until_now_oil <- function(co2, dts_month, last_years=5, min_r2=0.85) {

  # Prepare proxy data from other countries
  proxy <- co2 %>%
    filter(fuel == 'oil') %>%
    complete(
      date,
      iso2,
      fuel,
      sector
    ) %>%
    # Split into EU and others
    mutate(is_eu = iso2 == 'EU') %>%
    filter(iso2 %in% get_eu_iso2s()) %>%
    arrange(desc(date)) %>%
    # Get the last month with good coverage for non-EU countries
    group_by(date, fuel, sector) %>%
    mutate(coverage = mean(!is.na(value[!is_eu]))) %>%
    group_by(fuel, sector) %>%
    # Exclude date tails with low coverage
    filter(date <= max(date[coverage >= 0.8])) %>%
    filter(date >= min(date[coverage >= 0.8])) %>%
    filter(!is_eu) %>%
    # Fill missing country-level data through interpolation
    arrange(date) %>%
    group_by(fuel, sector) %>%
    mutate(
      value = zoo::na.approx(value, na.rm = F)
    ) %>%
    # Sum all EU countries
    group_by(date, fuel, sector) %>%
    summarise(
      value_proxy = sum(value, na.rm = T),
      .groups = 'drop'
    ) %>%
    mutate(iso2="EU")

  # Filter EU data
  co2_eu <- co2 %>%
    filter(iso2 == 'EU', fuel == 'oil')

  # Use project_until_now_lm for the actual projection
  new_eu_oil <- project_until_now_lm(
    co2_eu,
    proxy,
    dts_month = dts_month,
    last_years = last_years,
    min_r2 = min_r2
  )

  bind_rows(
    co2 %>% filter(fuel != 'oil' | iso2 != 'EU'),
    new_eu_oil
  )
}


project_until_now_forecast <- function(co2, dts_month, last_years=10, conf_level=0.90){

  res <- co2 %>%
    group_by(iso2, geo, fuel, sector) %>%
    expand_dates('date', dts_month) %>%
    arrange(date) %>%
    group_modify(function(df, group_keys) {

      # Get the latest date with actual data
      latest_data <- max(df$date[!is.na(df$value)])

      if(max(dts_month) == latest_data | all(is.na(df$value))) {
        return(df %>%
                 rename(value_central = value))
      }

      # Create time series object from historical data, limited to last_years
      historical_data <- df %>%
        filter(date <= latest_data,
               date >= latest_data - years(last_years)) %>%
        arrange(date)  # Ensure data is in chronological order

      ts_data <- ts(historical_data$value,
                    frequency = 12,
                    start = c(year(min(historical_data$date)),
                             month(min(historical_data$date))))

      # Forecast using Holt-Winters method
      forecasted <- tryCatch({
        forecast::hw(ts_data, level=conf_level) %>%
          as.data.frame() %>%
          `names<-`(c("mean", "lower", "upper")) %>%
          mutate(date = strptime(paste("01", row.names(.)), format="%d %b %Y") %>% as.Date()) %>%
          `rownames<-`(NULL) %>%
          filter(date %in% dts_month)
      }, error = function(e) {
        log_warn(glue("{group_keys$iso2} - {group_keys$fuel} {group_keys$sector}: Forecast model failed."))
        return(NULL)
      })

      if(is.null(forecasted)){
        return(df)
      }

      # Update values in the original dataframe
      df %>%
        left_join(forecasted, by="date") %>%
        mutate(
          value_central = coalesce(value, mean),
          value_lower = coalesce(value, lower),
          value_upper = coalesce(value, upper)
      ) %>%
        select(-c(value, mean, lower, upper))
    }) %>%
    ungroup()

  # Quick diagnostic plot
  # res %>%
  #   fill_lower_upper() %>%
  #   pivot_longer(cols=starts_with('value_'), names_to='estimate', values_to='value') %>%
  #   ggplot() +
  #   geom_line(aes(date, value, col=estimate)) +
  #   facet_wrap(sector~fuel + iso2)

  res %>%
    fill_lower_upper() %>%
    pivot_longer(cols=starts_with('value_'), names_to='estimate', values_to='value',
                 names_prefix='value_')
}

fill_lower_upper <- function(df) {
  df %>%
    mutate(
      value_lower = coalesce(value_lower, value_central),
      value_upper = coalesce(value_upper, value_central)
    )
}



