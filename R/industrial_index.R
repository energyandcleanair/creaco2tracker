#' Returns a oil/gas/coal-demand-weighted industrial production indexes
#' Following: https://www.energypolicy.columbia.edu/publications/anatomy-of-the-european-industrial-gas-demand-drop/
#'
#' @return
#' @export
#'
#' @examples
get_industrial_indexes <- function(index_date="last",
                                   frequency="month",
                                   project=T
                                   ){

  # Energy consumption by NACE
  filter_nace <- function(x){
    filter(x,
    nace_r2_code == "B" |
    # three letter starting with C
    (stringr::str_length(nace_r2_code) %in% c(3,7)) & (substr(nace_r2_code, 1, 1) == "C"))
  }


  product_codes <- list(
    'gas' = 'P13',
    'oil' = c('P12','P14','P15','P16','P17','P18','P19', 'P20', 'P21'),
    'coal' = c('P08','P09','P10','P11')
  )


  pefasu <- get_eurostat_from_code("env_ac_pefasu")


  consumption_per_sector <- lapply(names(product_codes), function(product){
    pefasu %>%
      filter(prod_nrg_code %in% product_codes[[product]],
             stk_flow=="Emission-relevant use",
             unit=="Terajoule",
      ) %>%
      add_iso2() %>%
      filter(iso2 %in% get_eu_iso2s(include_eu=T)) %>%
      filter_nace() %>%
      mutate(product=product)
  }) %>%
    bind_rows() %>%
    group_by(iso2, geo, time, nace_r2_code, nace_r2, product) %>%
    summarise(energy_tj=sum(values, na.rm=T))

  # Get index year
  if(index_date=="last" | is.null(index_date)){
    index_date <- max(consumption_per_sector$time)
  }else{
    index_date <- as.Date(paste0(index_date, "-01-01"))
  }


  # Industrial production
  indprod <- get_eurostat_indprod()

  # Be sure not to double count / having overlapping sectors
  indprod_filtered <- indprod %>%
    filter_nace() %>%
    filter(
      unit=="Index, 2021=100",
      grepl("Calendar adjusted data", s_adj)
    )

  # Fill incomplete data
  consumption_filled <- consumption_per_sector %>%
    left_join(indprod_filtered %>%
                group_by(iso2, time=floor_date(time, "year"), nace_r2_code) %>%
                summarise(value_prod=sum(values, na.rm=T))) %>%
    mutate(ff_intensity = case_when(
      # Prevent 0 values
      energy_tj > 0 ~ energy_tj / value_prod,
      T ~ NA
      )) %>%
    # Fill first by interpolation
    group_by(iso2, geo, nace_r2_code, product) %>%
    arrange(time) %>%
    fill(ff_intensity, .direction="updown")  %>%
    # Remove inf
    mutate(ff_intensity=ifelse(ff_intensity==Inf, NA, ff_intensity))


  # consumption_filled %>% filter(iso2=="EU") %>% arrange(ff_intensity)
  #
  # ggplot(consumption_filled %>% filter(iso2=="EU")) +
  #   geom_line(aes(time, ff_intensity, col=nace_r2), show.legend = F) +
  #   facet_wrap(~product, scales='free_y')

  # Fill last dates using forecasting
  fill_last_dates <- function(x, frequency, last_years=10, conf_level=0.90){

    max_date <- ceiling_date(max(x$time), frequency) - months(1)

    x %>%
      rename(value=values) %>%
      ungroup() %>%
      tidyr::complete(
        nesting(nace_r2_code, nace_r2),
        nesting(iso2, geo),
        time = seq.Date(min(time), max_date, by="month")
      ) %>%
      group_by(iso2, nace_r2_code) %>%
      group_modify(function(df, group_keys) {
        # Get the latest date with actual data
        latest_data <- max(df$time[!is.na(df$value)])

        if(max(df$time) == latest_data | all(is.na(df$value))) {
          return(df %>%
                 mutate(value_lower = value,
                        value_upper = value) %>%
                 rename(value_central = value)
                   )
        }

        # Create time series object from historical data
        historical_data <- df %>%
          filter(time <= latest_data,
                 time >= latest_data - years(last_years)) %>%
          arrange(time)

        ts_data <- ts(historical_data$value,
                      frequency = 12,
                      start = c(year(min(historical_data$time)),
                               month(min(historical_data$time))))

        # Forecast using Holt-Winters method
        forecasted <- tryCatch({
          forecast::hw(ts_data, level=conf_level) %>%
            as.data.frame() %>%
            `names<-`(c("mean", "lower", "upper")) %>%
            mutate(time = strptime(paste("01", row.names(.)), format="%d %b %Y") %>% as.Date()) %>%
            `rownames<-`(NULL)
        }, error = function(e) {
          log_warn(glue("{group_keys$iso2} - {group_keys$nace_r2_code}: Forecast model failed."))
          return(NULL)
        })

        if(is.null(forecasted)) {
          return(df %>%
                 mutate(
                   value_lower = value,
                   value_upper = value) %>%
                   rename(value_central = value)
                 )
        }

        # Update values in the original dataframe
        df %>%
          left_join(forecasted, by=c("time")) %>%
          mutate(
            value_central = coalesce(value, mean),
            value_lower = coalesce(value, lower),
            value_upper = coalesce(value, upper)
          ) %>%
          select(-c(value, mean, lower, upper))
      }) %>%
      ungroup() %>%
      pivot_longer(cols = c("value_central", "value_lower", "value_upper"),
                   names_prefix = "value_",
                   names_to = "estimate",
                   values_to = "value")
  }

  # Index production
  indprod_filled <- indprod_filtered %>%
    select(nace_r2_code, nace_r2, time, geo, values, iso2) %>%
    {
      if(project){
        fill_last_dates(., frequency)
      } else {
        .
      }
    }


  industrial_indexes <- indprod_filled %>%
    left_join(
      consumption_filled %>%
        filter(time==index_date) %>%
        select(nace_r2_code, iso2, ff_intensity, product),
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(ff_intensity)) %>%

    mutate(energy_tj = ff_intensity * value) %>%

    # Group by period
    group_by(iso2, geo, nace_r2_code, nace_r2, product, date=floor_date(time, frequency), ff_intensity, estimate) %>%
    summarise(value=sum(value),
              energy_tj=sum(energy_tj)) %>%

    # Index prod per nace (100: base year for each nace)
    group_by(iso2, geo, nace_r2_code, nace_r2, product, estimate) %>%
    mutate(index_per_nace = value / value[date==index_date] * 100) %>%

    # Index prod per product (100: base year for each product)
    group_by(iso2, product, estimate) %>%
    mutate(index_per_product = value / sum(value[date==index_date], na.rm=T) * 100) %>%
    select(-c(value)) %>%
    ungroup()


  # Quick check on manual value: DE, B, 2021: 12863.4TJ
  expected_value_yearly <- 12863.4
  expected_value_monthly <- 12863.4 * 82.9 / 1193.7
  expected_value <- case_when(frequency=="year" ~ expected_value_yearly,
                              frequency=="month" ~ expected_value_monthly)
  filter_tbl <- tibble(iso2="DE", date=index_date, nace_r2_code="B", product="gas", estimate="central", time=index_date)
  stopifnot(
    round(sum(inner_join(industrial_indexes, filter_tbl) %>%
                pull(energy_tj), na.rm=T), 1) == round(expected_value,1)
  )
  stopifnot(
    round(sum(inner_join(consumption_filled, filter_tbl) %>%
                pull(energy_tj), na.rm=T), 1) == round(expected_value_yearly,1)
  )
  stopifnot(
    round(sum(inner_join(industrial_indexes, filter_tbl) %>%
                pull(energy_tj), na.rm=T), 1) == round(expected_value,1)
  )



  return(industrial_indexes)
}

