
project_until_now <- function(co2, pwr){

  dts_month <- seq.Date(min(co2$time), today() %>% 'day<-'(1), by='month')

  # Add missing dates
  co2_filled1 <- co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    expand_dates('time', dts_month) %>%
    arrange(desc(time)) %>%
    ungroup()

  # First fill electricity sector using power generation
  co2_filled2 <-
    bind_rows(
      co2_filled1 %>% filter(sector==SECTOR_ELEC) %>%
        project_until_now_elec(pwr=pwr, dts_month=dts_month),
      co2_filled1 %>% filter(sector!=SECTOR_ELEC),
    )

  if(!nrow(co2_filled2) == nrow(co2_filled1)) stop("Unexpected change in rows")

  # Fill all missing (can be electricity as well if model wasn't good enough)
  co2_filled3 <- project_until_now_yoy(co2_filled2, dts_month=dts_month)
  if(!nrow(co2_filled3) == nrow(co2_filled1)) stop("Unexpected change in rows")


  return(ungroup(co2_filled3))
}

project_until_now_elec <- function(co2, pwr, dts_month, last_years=5, min_r2=0.85){

  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    group_modify(function(df, keys, ...) {

      iso2 <- keys %>% add_iso2() %>% pull(iso2)
      fuel_type <- unique(keys$fuel_type)
      geo <- unique(keys$geo)
      pwr_group <- pwr %>%
        ungroup() %>%
        filter(iso2==!!iso2) %>%
        filter(source==case_when(fuel_type=='coal' ~ 'Coal',
                                 fuel_type=='gas' ~ 'Fossil Gas')) %>%
        select(date, value_mwh) %>%
        group_by(time=floor_date(date, 'month')) %>%
        summarise(value_mwh=sum(value_mwh)) %>%
        arrange(desc(time))

      data <- df %>%
        left_join(pwr_group, by="time")

      data_training <- data %>%
        filter(CO2_emissions>0,
               value_mwh>0,
               time >= max(time) - lubridate::years(last_years))

      if(all(is.na(data_training$value_mwh)) & sum(data_training$CO2_emissions, na.rm=T)>0){
        warning(glue("{iso2} - {fuel_type}: Missing power data. Leaving as NA."))
        return(df)
      }

      if(sum(data_training$CO2_emissions, na.rm=T)==0){
        return(df %>%
                 mutate(CO2_emissions=tidyr::replace_na(CO2_emissions, 0)))
      }


      model <- lm(data=data_training, formula= CO2_emissions ~ value_mwh + 0)

      # ggplot(data_training) +
      #   geom_line(aes(time, CO2_emissions, col='estimated'))+
      #   geom_line(aes(time, predict(model, data_training), col='predicted'))

      r2 <- summary(model)$adj.r.squared
      if(r2 < min_r2){
        warning(glue("{geo} - {fuel_type}: Model CO2 ~ power not good enough (r2={round(r2,2)}. Leaving as NA."))
        return(df)
      }

      df$CO2_emissions <- coalesce(df$CO2_emissions, predict(model, data))
      df
    }) %>%
    ungroup()
}

project_until_now_yoy <- function(co2, dts_month){
  #fill data to present assuming deviation from 3-year average stays same as in last 3 months of data
  co2 %>%
    group_by(iso2, geo, fuel_type, sector) %>%
    expand_dates('time', dts_month) %>%
    arrange(time) %>%
    group_modify(function(df, ...) {

      df %<>% group_by(month=month(time)) %>%
        mutate(mean3y = CO2_emissions %>% lag %>% zoo::rollapplyr(3, mean, na.rm=F, fill=NA),
               yoy = CO2_emissions / mean3y - 1) %>% ungroup %>% select(-month)

      df$yoy %>% zoo::rollapplyr(3, mean, na.rm=F) %>% na.omit %>% tail(1) -> latest_yoy

      latest_data <- max(df$time[!is.na(df$CO2_emissions)])

      df %>% mutate(CO2_emissions = ifelse(time > latest_data,
                                           mean3y * (1+latest_yoy),
                                           CO2_emissions)) %>%
        select(-c(mean3y, yoy))
    }) %>%
    ungroup()
}
