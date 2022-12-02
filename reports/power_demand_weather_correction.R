require(tidyverse); require(lubridate); require(magrittr); require(wbstats)

pwr %>% filter(country=='EU total', source=='Total') %>% mutate(across(date, as.Date)) -> pwr_demand
co2_daily %>% filter(sector=='others', fuel_type=='gas') %>% mutate(value_TWh = CO2_hybrid / 55 / 3.6 / 1000) %>% 
  select(date, value_TWh) -> gas_demand

c('result_2020-01-01_2022-10-30_weighted_cdd.csv',
  'result_2020-01-01_2022-10-30_weighted_hdd.csv') %>% 
  file.path('~/../Downloads/', .) %>% 
  lapply(function(f) {
    read_csv(f) %>% 
      mutate(variable=ifelse(grepl('_hdd', f), 'hdd', 'cdd'))
  }) %>% bind_rows() %>% 
  select(-...1) %>% 
  pivot_longer(-c(Date, variable), names_to='country') %>% 
  rename(date=Date) ->
  dd

wb_data('SP.POP.TOTL', return_wide = F) %>% filter(date==2021) -> pop

dd %<>% mutate(iso3c=countrycode(country, 'country.name.en', 'iso3c')) %>% 
  left_join(pop %>% select(iso3c, pop=value))

dd %>% filter(iso3c %in% codelist$iso3c[!is.na(codelist$eu28)], iso3c != 'GBR') %>% 
  group_by(variable, date) %>% summarise(across(value, ~weighted.mean(.x, pop))) ->
  dd_eu

dd_eu %>% spread(variable, value) %>% left_join(pwr_demand, .) -> pwr_demand
pwr_demand %>% lm(value_mw ~ hdd + cdd + as.factor(wday(date)), data=.) -> m_pwr

m_pwr %>% summary()
predict(m_pwr, pwr_demand) -> pwr_demand$value_mw_pred

pwr_demand %>% mutate(anomaly = value_mw - value_mw_pred,
                  value_mw_temperature_corrected = mean(value_mw_pred, na.rm=T) + anomaly) %>% 
  pivot_longer(c(anomaly, value_mw, value_mw_pred, value_mw_temperature_corrected)) %>% 
  group_by(name) %>% 
  mutate(across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T)) %>% 
  filter(date>='2020-01-28') %>% 
  ggplot(aes(date, value, col=name)) + geom_line() +
  labs(title='EU power demand')


dd_eu %>% spread(variable, value) %>% left_join(gas_demand, .) -> gas_demand
gas_demand %>% lm(value_TWh ~ hdd + as.factor(wday(date)), data=.) -> m_gas

m_gas %>% summary()
predict(m_gas, gas_demand) -> gas_demand$value_TWh_pred

gas_demand %>% mutate(anomaly = value_TWh - value_TWh_pred,
                  value_TWh_temperature_corrected = mean(value_TWh_pred, na.rm=T) + anomaly) %>% 
  pivot_longer(c(anomaly, value_TWh, value_TWh_pred, value_TWh_temperature_corrected)) %>% 
  group_by(name) %>% 
  mutate(across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T)) %>% 
  filter(date>='2020-01-28') %>% 
  ggplot(aes(date, value, col=name)) + geom_line() +
  labs(title='EU gas demand')



dd_eu %>% 
  group_by(variable) %>% 
  mutate(plotdate = date %>% 'year<-'(2022),
         year=as.factor(year(date)),
         across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T),
         variable_name=ifelse(variable=='cdd', 'cooling', 'heating')) %>% 
  write_csv(file.path(output_dir, 'EU average cooling and heating needs.csv')) %>% 
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
quicksave(file.path(output_dir, 'EU average cooling and heating needs.png'))



dd_eu %>% 
  group_by(variable) %>% 
  mutate(plotdate = date %>% 'year<-'(2022),
         year=as.factor(paste0(year(date),'年')),
         across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T),
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
quicksave(file.path(output_dir, 'EU average cooling and heating needs ZH.png'))
