#' Project CO2 emissions until today using various proxies
#'
#' @param co2
#' @param pwr_generation
#' @param gas_demand
#'
#' @return
#' @export
#'
#' @examples
project_until_now <- function(co2, pwr_generation, gas_demand, eurostat_indprod){

  dts_month <- seq.Date(min(co2$date), today() %>% 'day<-'(1), by='month')

  co2 %>%
    split_gas_to_elec_all() %>%
    project_until_now_elec(pwr_generation=pwr_generation, dts_month=dts_month) %>%
    project_until_now_gas(gas_demand=gas_demand, dts_month=dts_month) %>%
    project_eu_from_countries(dts_month=dts_month) %>%

    # Before we apply ETS projection (which has large attached uncertainty)
    # We give a chance to other = total - sum(other sectors)
    # It applies to coal and oil so far
    detotalise_co2() %>%

    # We use industry for coal
    project_until_now_coal_others(eurostat_indprod=eurostat_indprod, dts_month=dts_month) %>%

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
project_until_now_lm <- function(co2, proxy, dts_month, min_r2=0.9, force_overwrite=F, verbose=F){

  co2_untouched <- co2 %>% anti_join(proxy %>% distinct(iso2, fuel, sector),
                                      by=c('iso2', 'fuel', 'sector')
                                     )
  co2_touched <- co2 %>%
    inner_join(proxy %>% distinct(iso2, fuel, sector),
               by=c('iso2', 'fuel', 'sector')) %>%
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

      get_trained_model <- function(data, n_year){
        data_training <- data %>%
          filter(value>0,
                 date >= max(date) - lubridate::years(n_year)) %>%
          # only positive values in value_proxy_cols
          filter_at(value_proxy_cols, all_vars(.>0))

        # If all value_proxy_cols are NA, stop
        if(data_training %>% select_at(value_proxy_cols) %>% filter_all(any_vars(!is.na(.))) %>% nrow == 0){
          # message(glue("{iso2} - {fuel}: Missing proxy data. Leaving as such"))
          return(NULL)
        }

        if(sum(data_training$value)==0){
          return(NULL)
        }

        if(data_training %>% select_at(value_proxy_cols) %>% filter_all(any_vars(!is.na(.))) %>% nrow > 0){
          lm(data=data_training, formula= paste0('value ~ 0+', paste0(value_proxy_cols, collapse=' + ')))
        }else{
          NULL
        }
      }

      # We automatically determine the number of years to consider based on adjusted R2
      n_years <- seq(1,10)
      r2s <- lapply(n_years, function(n_year){
        model <- get_trained_model(data, n_year)
        if(!is.null(model)){
          summary(model)$adj.r.squared
        }else{
          0
        }
      })

      # Best R2
      if(all(r2s==0)){
        log_warn(glue("{iso2} - {fuel} {sector}: No proxy data. Leaving as such."))
        return(df)
      }

      n_year <- n_years[which.max(r2s)]
      model <- get_trained_model(data, n_year)

      r2 <- summary(model)$adj.r.squared
      if(r2 < min_r2){
        log_warn(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy not good enough (R2={round(r2,2)}). Leaving as NA."))
        return(df)
      }else{
        log_success(glue("{iso2} - {fuel} {sector}: Model CO2 ~ proxy OK (R2={round(r2,2)}). Using {n_year} years for training."))
      }

      if(force_overwrite){
        df$value <- predict(model, data)
      }else{
        df$value <- coalesce(df$value, predict(model, data))
      }
      df
    }) %>%
    ungroup()


  bind_rows(
    co2_untouched,
    co2_touched
  )
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

project_until_now_elec <- function(co2, pwr_generation, dts_month, min_r2=0.85){

  proxy <- pwr_generation %>%
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


  project_until_now_lm(co2, proxy, dts_month=dts_month, min_r2=min_r2)

}


project_until_now_gas <- function(co2, gas_demand, dts_month, min_r2=0.85){

  proxy <- gas_demand %>%
    filter(iso2 %in% co2$iso2) %>%
    ungroup() %>%
    filter(unit=='m3') %>%
    group_by(iso2, date=floor_date(date, 'month')) %>%
    summarise(value_proxy=sum(value), .groups = 'drop') %>%
    arrange(desc(date)) %>%
    mutate(fuel='gas', sector=SECTOR_ALL)

  project_until_now_lm(co2, proxy, dts_month=dts_month, min_r2=min_r2)
}

project_until_now_coal_others <- function(co2, eurostat_indprod, dts_month, min_r2=0.85){

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
  project_until_now_lm(co2, proxy, dts_month=dts_month, min_r2=min_r2)

}


project_until_now_forecast <- function(co2, dts_month, last_years=10, conf_level=0.90){

  res <- co2 %>%
    group_by(iso2, geo, fuel, sector, unit) %>%
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

#' Project EU emissions using available country data
#'
#' @param co2 CO2 emissions dataframe
#' @param dts_month Sequence of months to project
#' @param min_countries Minimum number of countries required for projection (default 25)
#' @param sectors Optional vector of sectors to project (default NULL for all sectors)
#' @param fuels Optional vector of fuels to project (default NULL for all fuels)
#'
#' @return Updated CO2 emissions dataframe with projected EU values
#' @export
project_eu_from_countries <- function(co2, dts_month, min_countries = 20,
                                    sectors = NULL, fuels = NULL) {

  # Apply sector and fuel filters if provided
  co2_filtered <- co2
  if (!is.null(sectors)) {
    co2_filtered <- co2_filtered %>% filter(sector %in% sectors)
  }
  if (!is.null(fuels)) {
    co2_filtered <- co2_filtered %>% filter(fuel %in% fuels)
  }

  # Split EU and country data
  co2_eu <- co2_filtered %>% filter(iso2 == "EU")
  co2_countries <- co2_filtered %>%
    filter(iso2 %in% get_eu_iso2s(), iso2 != "EU")

  # Get latest date with EU data for each fuel/sector combination
  latest_eu_dates <- co2_eu %>%
    filter(!is.na(value)) %>%
    group_by(fuel, sector) %>%
    summarise(latest_eu_date = max(date), .groups = "drop")

  # For each month after latest EU data, get country availability info
  countries_availability <- co2_countries %>%
    left_join(latest_eu_dates, by = c("fuel", "sector")) %>%
    filter(date > latest_eu_date) %>%
    group_by(date, fuel, sector) %>%
    summarise(
      available_countries = list(iso2[!is.na(value)]),
      n_countries = length(available_countries[[1]]),
      .groups = "drop"
    ) %>%
    filter(n_countries >= min_countries)

  # Get distinct country sets
  country_sets <- countries_availability %>%
    distinct(available_countries, n_countries) %>%
    arrange(desc(n_countries))

  if(nrow(country_sets) == 0){
    return(co2)
  }

  # Initialize results list
  projected_results <- list()

  # Loop through each distinct set of countries
  for (i in 1:nrow(country_sets)) {
    current_countries <- country_sets$available_countries[[i]]

    # Prepare proxy data using only these countries
    proxy <- co2_countries %>%
      filter(iso2 %in% current_countries) %>%
      left_join(latest_eu_dates, by = c("fuel", "sector")) %>%
      # Get dates where we have all countries in the set
      group_by(date, fuel, sector) %>%
      filter(n_distinct(iso2[!is.na(value)]) == length(current_countries)) %>%
      summarise(
        value_proxy = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(iso2 = "EU")

    # Project using this specific set of countries
    current_projection <- project_until_now_lm(
      co2_eu,
      proxy,
      dts_month = dts_month,
      min_r2 = 0.95
    ) %>%
      mutate(order = i)  # Add order based on country set size (1 = largest set)

    projected_results[[i]] <- current_projection
  }

  # Combine all projections and take first available value for each combination
  final_projection <- bind_rows(projected_results) %>%
    group_by(iso2, date, fuel, sector) %>%
    filter(!is.na(value)) %>%
    slice_min(order, n = 1) %>%  # Take row with smallest order number
    ungroup() %>%
    select(-order)  # Remove the order column

  # Combine projected EU data with original data
  bind_rows(
    co2 %>% anti_join(final_projection, by = c("iso2", "date", "fuel", "sector")),
    final_projection
  )
}



