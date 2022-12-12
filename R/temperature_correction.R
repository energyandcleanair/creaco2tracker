require(tidyverse)
require(magrittr)
require(lubridate)
require(rcrea)
require(creahelpers)
require(creaco2tracker)
require(countrycode)

source('R/get_co2_daily.R')
eu_members <- codelist$iso2c[!is.na(codelist$eu28)][-28]

#load data
co2 <- get_co2_daily()
co2 %>% filter(sector=='Others', fuel=='Gas') %>% mutate(value_TWh = value / 55 / 3.6 / 1000) %>% 
  select(date, value_TWh) -> gas_demand

#saveRDS(co2, 'diagnostics/CO2.RDS')
#co2 <- readRDS('diagnostics/CO2.RDS')

pwr <- get_entsoe()

pwr %>% 
  filter(region=='EU') %>%
  group_by(region, date) %>%
  dplyr::summarise(across(value_mw, sum, na.rm=T)) %>%
  mutate(source='Total', country='EU total') ->
  pwr_demand

#hdd and cdd
read_csv("https://api.energyandcleanair.org/v1/weather?variable=HDD,CDD&format=csv") %>% 
  mutate(across(variable, tolower)) ->
  dd

#defaults for saving plots
quicksave <- function(file, width=8, height=6, bg='white', ...) {
  ggsave(file, width=width, height=height, bg=bg, ...)
}

#function to fit model for energy demand vs temperature
fit_model <- function(data, 
                      independents='hdd|cdd', 
                      countries=eu_members,
                      plot_moving_average_days=7) {
  dd %>% select(region_id, date, variable, value) %>% 
    filter(region_id %in% countries) %>% 
    unite(variable, variable, region_id) %>% 
    spread(variable, value) %>% 
    left_join(data, .) -> modeldata
  
  regression_vars <- grep(independents, names(modeldata), value=T)
  
  regression_formula <- as.formula(paste0('value ~ ',
                                          paste(regression_vars, collapse='+'),
                                          ' + as.factor(wday(date))'))
  
  modeldata %>% lm(regression_formula, .) -> m
  
  m %>% summary()
  predict(m, modeldata) -> data$value_pred
  
  data %>% mutate(anomaly = value - value_pred,
                  value_temperature_corrected = mean(value_pred, na.rm=T) + anomaly,
                  year=year(date), plotdate=date %>% 'year<-'(2022)) %>% 
    pivot_longer(c(anomaly, value, value_pred, value_temperature_corrected),
                 names_to = 'measure') %>% 
    group_by(measure) %>% 
    mutate(across(value, zoo::rollapplyr, FUN=mean, width=plot_moving_average_days, fill=NA, na.rm=T)) %>% 
    filter(date>='2019-01-01') ->
    data
  
  data %>% filter(measure=='value_temperature_corrected') %>% 
    ggplot(aes(plotdate, value, col=as.factor(year))) + geom_line(size=1) +
    labs(title=paste('EU temperature corrected', unique(data$name)),
         y=paste0(unique(data$unit), ', ', plot_moving_average_days, '-day mean'),
         x='', col='year') +
    theme_crea() + 
    scale_color_crea_d('change', col.index = c(1:3,5:7)) + 
    scale_y_continuous(labels=scales::comma) +
    scale_x_date(date_labels = '%b') ->
    p
  
  quicksave(p, file.path(paste0('EU temperature corrected ', unique(data$name), '.png')))
  
  data %>% group_by(measure) %>% 
    mutate(yoy=get_yoy(value, date)) %>% select(date, yoy) %>% 
    slice_tail(n=1) %>% 
    write_csv(file.path('reports', 
                        paste0(unique(data$name), ', YoY changes, past ',
                               plot_moving_average_days,' days.csv'))) ->
    changes
  
  list(data=data, plot=p, model=m, recent_changes=changes)
}

#fit models
pwr_demand %>% rename(value=value_mw) %>% 
  mutate(value=value/1e3, name='electricity demand', unit='GW') %>% fit_model() ->
  pwr_model

gas_demand %>% filter(date>='2021-01-01') %>% 
  rename(value=value_TWh) %>% 
  mutate(name='gas demand outside the power sector', unit='TWh/day') %>% fit_model() ->
  gas_model

#additional plots
dd %>% filter(year(date) %in% 2020:2022, region_id=='EU') %>% 
  group_by(variable) %>% 
  mutate(plotdate = date %>% 'year<-'(2022),
         across(value, zoo::rollapplyr, FUN=mean, width=7, fill=NA, na.rm=T)) ->
  dd_plot

dd_plot %>% 
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
                       guide=guide_legend(nrow=1, override.aes = list(alpha=1, color=c('gray66', 'gray33', 'black')),
                                          title.position = 'left')) +
  scale_x_date(date_labels = '%b', expand=expansion(mult=.01)) +
  x_at_zero()
quicksave(file.path('reports', 'EU average cooling and heating needs.png'))



dd_plot %>% 
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
                       guide=guide_legend(nrow=1, override.aes = list(alpha=1, color=c('gray66', 'gray33', 'black')),
                                          title.position = 'left')) +
  scale_x_date(labels = function(x) paste0(month(x), '月'), expand=expansion(mult=.01)) +
  x_at_zero()
quicksave(file.path('reports', 'EU average cooling and heating needs ZH.png'))
