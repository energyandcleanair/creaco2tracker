get_corrected_demand <- function(diagnostics_folder='diagnostics',
                                 use_co2_in_db=T){

  create_dir(diagnostics_folder)
  eu_members <- get_eu_iso2s()

  # load data
  co2 <- if(use_co2_in_db){
    download_co2()
  }else{
    get_co2(diagnostics_folder=diagnostics_folder, downscale_daily=T)
  }


  gas_demand <- co2 %>%
    filter(sector=='Others', fuel=='Gas') %>%
    mutate(value_TWh = value / 55 / 3.6 / 1000) %>%
    select(date, value_TWh)

  # gas_demand2 <- download_gas_demand(iso2='EU') %>%
  #   mutate(value_TWh=value*gcv_kwh_m3/1e9)
  #
  #
  # bind_rows(gas_demand1 %>% mutate(source='co2'), gas_demand2 %>% mutate(source='demand') %>% filter(region_id=='EU')) %>%
  #   ggplot() +
  #   geom_line(aes(date, value_TWh, col=source))
  #
  #   filter(sector=='Others', fuel=='Gas') %>%
  #   mutate(value_TWh = value / 55 / 3.6 / 1000) %>%
  #   select(date, value_TWh)

  pwr <- entsoe.get_power_generation(use_cache = F)

  pwr_generation <- pwr %>%
    filter(region=='EU', source=='Total') %>%
    group_by(source, region, date) %>%
    dplyr::summarise(across(value_mw, sum, na.rm=T)) %>%
    mutate(country='EU')

  # hdd and cdd
  creahelpers::api.get("api.energyandcleanair.org/v1/weather", variable="HDD,CDD", region_id="EU") %>%
    mutate(across(variable, tolower)) ->
    dd

  # Fill na values
  dd %>%
    mutate(date=lubridate::date(date)) %>%
    ungroup() %>%
    tidyr::complete(date=seq.Date(min(date), max(date), by='day'),
                    tidyr::nesting(variable, unit, region_id, region_type, region_iso2, averaging_period, source, region_name)) %>%
    group_by(variable, unit, region_id, region_type, region_iso2, averaging_period, source, region_name) %>%
    arrange(date) %>%
    fill(value) %>%
    ungroup() ->
  dd

  #defaults for saving plots
  quicksave <- function(file, width=8, height=6, bg='white', ...) {
    ggsave(file, width=width, height=height, bg=bg, ...)
  }

  #function to fit model for energy demand vs temperature
  fit_model <- function(data,
                        independents='hdd|cdd',
                        countries=get_eu_iso2s(include_eu = T),
                        plot_moving_average_days=7) {
    dd %>%
      select(region_id, date, variable, value) %>%
      filter(region_id %in% countries,
             date %in% data$date) %>%
      unite(variable, variable, region_id) %>%
      spread(variable, value) %>%
      left_join(data, .) -> modeldata

    regression_vars <- grep(independents, names(modeldata), value=T)

    # Remove '_CN'
    regression_vars <- grep('_CN$',regression_vars, value=T, invert=T)

    regression_formula <- as.formula(paste0('value ~ ',
                                            paste(regression_vars, collapse='+'),
                                            ' + as.factor(wday(date))'))

    modeldata %>% lm(regression_formula, .) -> m

    m %>% summary()
    predict(m, modeldata) -> data$value_pred

    data %>%
      mutate(anomaly = value - value_pred,
             value_temperature_corrected = mean(value_pred, na.rm=T) + anomaly,
             year=year(date),
             plotdate=date %>% 'year<-'(2022)) %>%
      pivot_longer(c(anomaly, value, value_pred, value_temperature_corrected),
                   names_to = 'measure') %>%
      filter(date>='2019-01-01',
             date <= max(dd$date)) ->
      data

    # Timeseries
    data %>%
      group_by(measure) %>%
      mutate(across(value, zoo::rollapplyr, FUN=mean, width=plot_moving_average_days, fill=NA, na.rm=T)) %>%
      ungroup() %>%
      filter(measure=='value_temperature_corrected') %>%
      ggplot(aes(plotdate, value, col=as.factor(year))) + geom_line(linewidth=1) +
      labs(title=paste('EU temperature corrected', unique(data$name)),
           y=paste0(unique(data$unit), ', ', plot_moving_average_days, '-day mean'),
           x='', col='year') +
      theme_crea_new() +
      scale_color_crea_d('change', col.index = c(1:3,5:7)) +
      scale_y_continuous(labels=scales::comma) +
      scale_x_date(date_labels = '%b') +
      rcrea::scale_y_crea_zero() -> p

    quicksave(file.path(diagnostics_folder,
                           paste0('temp_corrected_ts_ ', unique(data$name), '.png')),
              plot=p)

    # By year
    data %>%
      group_by(year) %>%
      summarise(value=mean(value[measure=="anomaly"], na.rm=T)) %>%
      ungroup() %>%
      ggplot(aes(factor(year), value)) +
      geom_col() +
      labs(title=paste('EU temperature corrected anomaly', unique(data$name)),
           y=paste0(unique(data$unit), ', ', plot_moving_average_days, '-day mean'),
           x='', col='year') +
      theme_crea_new() +
      scale_color_crea_d('change', col.index = c(1:3,5:7)) +
      scale_y_continuous(labels=scales::comma) -> p

    quicksave(file.path(diagnostics_folder,
                        paste0('temp_corrected_year_ ', unique(data$name), '.png')),
              plot=p)

    data %>% group_by(measure) %>%
      mutate(yoy=creahelpers::get_yoy(value, date)) %>% select(date, yoy) %>%
      slice_tail(n=1) %>%
      write_csv(file.path(diagnostics_folder,
                          paste0(unique(data$name), ', YoY changes, past ',
                                 plot_moving_average_days,' days.csv'))) ->
      changes

    list(data=data, plot=p, model=m, recent_changes=changes)
  }

  #fit models
  pwr_generation %>%
    rename(value=value_mw) %>%
    mutate(value=value/1e3, name='electricity demand', unit='GW') %>%
    mutate(date=as.Date(date)) %>%
    fit_model() ->
    pwr_model

  gas_demand %>%
    filter(date>='2021-01-01') %>%
    rename(value=value_TWh) %>%
    mutate(name='gas demand outside the power sector', unit='TWh/day') %>%
    mutate(date=as.Date(date)) %>%
    fit_model() ->
    gas_model

  #additional plots
  dd %>%
    filter(year(date) %in% 2018:2024, region_id=='EU') %>%
    group_by(variable) %>%
    mutate(plotdate = date %>% 'year<-'(2000),
           across(value, zoo::rollapplyr, FUN=mean, width=30, fill=NA, na.rm=T)) ->
    dd_plot

  plt <- dd_plot %>%
    mutate(plotdate=as.Date(plotdate)) %>%
    mutate(year=as.factor(year(date)),
           variable_name=ifelse(variable=='cdd', 'cooling', 'heating')) %>%
    ggplot(aes(plotdate, value, alpha=year, col=variable_name)) +
    facet_wrap(~variable_name, scales='free_y', ncol=1) +
    geom_line(size=1) +
    labs(title='EU average cooling and heating needs',
         subtitle='population-weighted average for EU-27',
         y='degree-days', x='') +
    theme_crea(legend.position='top') + scale_color_crea_d('change', col.index = c(7,1), guide='none') +
    scale_alpha_discrete(range=c(.33,1),
                         # guide=guide_legend(nrow=1, override.aes = list(alpha=1, color=c('gray66', 'gray33', 'black')), title.position = 'left')
                         ) +
    scale_x_date(date_labels = '%b', expand=expansion(mult=.01)) +
    rcrea::scale_y_crea_zero()

  ggsave(file.path(diagnostics_folder, 'EU average cooling and heating needs.png'),
         plot=plt,
         width=10,
         height=8)


  plt_zh <- dd_plot %>%
    mutate(plotdate=as.Date(plotdate)) %>%
    mutate(year=as.factor(paste0(year(date),'年')),
           variable_name=ifelse(variable=='cdd', '制冷', '采暖')) %>%
    ggplot(aes(plotdate, value, alpha=year, col=variable_name)) +
    facet_wrap(~variable_name, scales='free_y', ncol=1) +
    geom_line(size=1) +
    labs(title='欧盟的采暖和制冷需求',
         subtitle='整个欧盟人口加权平均值',
         y='度日', x='', alpha='') +
    theme_crea(legend.position='top') + scale_color_crea_d('change', col.index = c(7,1), guide='none') +
    scale_alpha_discrete(range=c(.33,1),
                         guide=guide_legend(nrow=1,
                                            # override.aes = list(alpha=1, color=c('gray66', 'gray33', 'black')),
                                            title.position = 'left')) +
    scale_x_date(labels = function(x) paste0(month(x), '月'), expand=expansion(mult=.01)) +
    rcrea::scale_y_crea_zero()

  ggsave(file.path(diagnostics_folder, 'EU average cooling and heating needs ZH.png'),
         plot=plt_zh,
         width=10,
         height=8)


  # Format for DB
  corrected_gas_demand <- gas_model$data %>%
    ungroup() %>%
    filter(measure=='value_temperature_corrected') %>%
    select(date, unit, value) %>%
    mutate(fuel='fossil_gas_temperature_corrected',
           sector='except_power',
           data_source='crea',
           unit='TWh/day',
           frequency='daily',
           region_id='EU',
           region_type='region')

  corrected_electricity_demand <- pwr_model$data %>%
    ungroup() %>%
    filter(measure=='value_temperature_corrected') %>%
    select(date, unit, value) %>%
    mutate(fuel='electricity_temperature_corrected',
           sector='total',
           data_source='crea',
           unit='GW',
           frequency='daily',
           region_id='EU',
           region_type='region')

  return(bind_rows(corrected_gas_demand,
                   corrected_electricity_demand))
}

