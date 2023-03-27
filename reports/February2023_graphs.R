require(tidyverse)
require(magrittr)
require(lubridate)
require(rcrea)
require(creahelpers)
require(countrycode)

source('R/get_co2_daily.R')

load('Feb2023.RData')

co2 <- get_co2_daily()
saveRDS(co2, 'diagnostics/CO2.RDS')
co2 <- readRDS('diagnostics/CO2.RDS')

co2 %<>% filter(sector == 'Electricity', fuel %in% c('Coal', 'Gas')) %>% 
  group_by(region, date, sector, unit, frequency) %>% 
  summarise(across(value, sum)) %>% 
  mutate(fuel='Fossil total') %>% 
  bind_rows(co2)

co2 %<>% filter(fuel == 'Gas', sector %in% c('Electricity', 'Others')) %>% 
  group_by(region, date, fuel, unit, frequency) %>% 
  summarise(across(value, sum)) %>% 
  mutate(sector='All') %>% 
  bind_rows(co2)

dates_to_label <- tibble(date=ymd(c('2020-03-11', '2022-02-24')),
                         event=c('WHO declares COVID-19 pandemic',
                                 'Russia invades Ukraine'),
                         eventZH=c('世卫组织宣布新冠大流行', '俄罗斯入侵乌克兰'))

co2 %>% 
  group_by(fuel, date) %>% summarise(across(value, sum)) %>% 
  mutate(across(value, zoo::rollmeanr, 365, fill=NA)) %>% 
  filter(!grepl('total', fuel, ignore.case=T), date>='2005-01-01') ->
  coal_plotdata

library(showtext)
showtext_auto(enable=F)
coal_plotdata %>% filter(fuel=='Coal') %>% 
  mutate(value=value/1e6*365) %>% 
  (function(df) {
    df %>% filter(year(date) %in% 2005:2019) %>% 
      lm(value~date, data=.) -> m
    df %>% mutate(trend=predict(m, df))
  }) %>% 
  write_csv('~/EU CO2 emissions from coal.csv') %>% 
  ggplot(aes(date, value)) + 
  geom_area(fill=crea_palettes$CREA[2]) +
  geom_vline(data=dates_to_label, aes(xintercept=date), linetype='dotted', linewidth=1) +
  geom_smooth(aes(y=trend, color='pre-pandemic trend')) +
  geom_label(data=dates_to_label, aes(y=1000, label=event), hjust=1, nudge_y=c(0,-70), nudge_x=-80, angle=90) +
  theme_crea(legend.position='top') +
  labs(title='EU CO2 emissions from coal', x='', y='Mt/year, 12-month rolling mean', color='') +
  scale_color_crea_d(guide=guide_legend(override.aes = list(fill=NA))) +
  scale_x_date(date_breaks = '2 years', expand=expansion(), date_labels = '%Y') + 
  x_at_zero() ->
  plt
quicksave(file.path(output_dir, 'EU CO2 emissions from coal nolabel.png'), plot=plt)

showtext_auto(enable=T)

coal_plotdata %>% filter(fuel=='Coal') %>% 
  ggplot(aes(date, value/1e8*365)) + 
  geom_area(fill=crea_palettes$CREA[2]) +
  geom_vline(data=dates_to_label, aes(xintercept=date), linetype='dotted', linewidth=1) +
  geom_smooth(data=coal_plotdata %>% filter(year(date) %in% 2005:2019, fuel=='Coal'),
              method='lm', fullrange=T, aes(color='非新冠疫情影响的常态排放趋势')) +
  geom_label(data=dates_to_label, aes(y=10, label=eventZH), hjust=1, nudge_y=c(0,-.7), nudge_x=-80, size=10) +
  theme_crea(legend.position='top') +
  theme(text = element_text(family = "Source Sans", size=32, lineheight=1),
        plot.title = element_text(size=48),
        legend.title = element_text(size=10)) +
  labs(title='欧盟煤炭消费产生的碳排放量', x='', y='亿吨/年，12个月移动平均') +
  scale_color_crea_d(guide=guide_legend(override.aes = list(fill=NA)), name=' ') +
  scale_x_date(date_breaks = '2 years', expand=expansion(), date_labels = '%Y年') + 
  x_at_zero() ->
  plt
quicksave(file.path(output_dir, 'EU CO2 emissions from coal ZH.png'), plot=plt)

coal_plotdata %<>% group_by(date) %>% 
  summarise(across(value, sum)) %>% 
  mutate(fuel='All') %>% 
  bind_rows(coal_plotdata)

coal_plotdata %>% 
  ggplot(aes(date, value/1e6*365)) + 
  facet_wrap(~fuel) +
  geom_area(fill=crea_palettes$CREA[2]) +
  geom_vline(data=dates_to_label, aes(xintercept=date), linetype='dotted', linewidth=1) +
  geom_smooth(data=coal_plotdata %>% filter(year(date) %in% 2005:2019),
              method='lm', fullrange=T, aes(color='pre-pandemic trend')) +
  geom_label(data=dates_to_label, aes(label=event), hjust=1, nudge_y=c(0,-70), nudge_x=-80) +
  theme_crea(legend.position='top') +
  labs(title='EU CO2 emissions from coal', x='', y='Mt/year, 12-month rolling mean', color='') +
  scale_color_crea_d(guide=guide_legend(override.aes = list(fill=NA))) +
  snug_x_date + x_at_zero() ->
  plt
quicksave(file.path(output_dir, 'EU CO2 emissions by fuel.png'), plot=plt)


pwr <- get_entsoe()

#add rolling mean
pwr <- pwr %>%
  group_by(region, country, source) %>%
  arrange(date) %>%
  mutate(plotdate = date %>% 'year<-'(2022), year=year(date),
         output_mw_rollmean=zoo::rollapplyr(value_mw, rollmean_days, mean, fill=NA))

#output range of values for several years
pwr_ranges <- pwr %>% filter(year %in% 2015:2021) %>%
  group_by(region, country, source, plotdate) %>%
  summarise(min=min(output_mw_rollmean, na.rm=T), max=max(output_mw_rollmean, na.rm=T))


pwr %>% left_join(pwr_ranges) %>% 
  filter(date<max(date)-3, year %in% 2021:2023, country=='EU total') %>%
  group_by(country) %>% filter(mean(value_mw, na.rm=T)>1e3) %>%
  select(date, source, country, plotdate, year, output_mw_rollmean, min, max) %>% 
  mutate(across(c(output_gw_rollmean=output_mw_rollmean, min, max), ~.x/1000)) %>% 
  write_csv('~/EU power generation by source.csv') %>% 
  ggplot(aes(plotdate)) +
  facet_wrap(~source, scales='free_y', ncol=2) +
  geom_ribbon(aes(ymin=min, ymax=max, fill='2016–2021 range')) +
  geom_line(aes(y=output_gw_rollmean, col=as.factor(year)), linewidth=1) +
  expand_limits(y=0) +
  # scale_x_datetime(date_labels = '%b') +
  labs(title='EU power generation by source', y='GW, 30-day mean', x='', col='', fill='') +
  theme_crea(legend.position='top') +
  scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  scale_fill_crea_d(col.index = 2) +
  scale_x_date(date_labels = '%b') -> plt
quicksave(file.path(output_dir, 'EU power generation.png'), width=6, height=8, plot=plt, dpi=450)


pwr <- read_csv('https://api.energyandcleanair.org/power/generation?date_from=2016-01-01&aggregate_by=country,source,date&format=csv&country=United Kingdom')
pwr <- read_csv('https://api.energyandcleanair.org/power/generation?date_from=2016-01-01&aggregate_by=country,source,date&format=csv&region=EU') %>% 
  bind_rows(pwr)

pwr %<>% mutate(across(date, lubridate::date))

pwr <- pwr %>%
  filter(source!='Total') %>%
  group_by(region, country, date) %>%
  dplyr::summarise_at("value_mw", sum, na.rm=T) %>%
  mutate(source='Total') %>%
  bind_rows(pwr %>% filter(source!='Total'))

#add EU total
pwr <- pwr %>%
  group_by(date, source) %>%
  filter(region=='EU') %>%
  dplyr::summarise_at("value_mw", sum, na.rm=T) %>%
  mutate(country='EU total') %>%
  bind_rows(pwr %>% filter(country!='EU total'))


pwr %<>% filter(source %in% c('Coal', 'Fossil Gas')) %>% 
  group_by(country, date) %>% 
  summarise(across(value_mw, sum)) %>% 
  mutate(source='Fossil total') %>% 
  bind_rows(pwr %>% filter(source!='Fossil total'))

pwr %>% filter(country=='EU total') %>% 
  group_by(ym=date %>% 'day<-'(1), country, source) %>% 
  summarise(across(value_mw/1e3, mean)) %>% 
  ggplot(aes(ym, value_mw)) + geom_col() + facet_wrap(~source) + 
  theme_crea()

pwr %>% filter(month(date)==11, source=='Fossil total') %>% 
  group_by(country) %>% filter(mean(value_mw)>1e1) %>% 
  group_by(year=year(date), country, source) %>% 
  summarise(across(value_mw, mean)) %>% 
  ggplot(aes(year, value_mw)) + geom_col() + facet_wrap(~country, scales='free_y')


sources <- pwr$source %>% unique %>% sort
sources <- sources[c(1,2,4,5,9,7,6,8)]
sourcecols <- c(crea_palettes$dramatic[c(1,2)], crea_palettes$CREA[1:6])
names(sourcecols) <- sources

pwr %>% filter(month(date)==11) %>% 
  group_by(country) %>% filter(mean(value_mw)>2e3) %>% 
  group_by(year=year(date), country, source) %>% 
  summarise(across(value_mw, mean)) %>% 
  group_by(country, source) %>% 
  summarise(change=value_mw[year==2022]-value_mw[year==2021]) %>% 
  ggplot(aes(source, change/1000, fill=source)) + geom_col() + facet_wrap(~country, scales='free_x') +
  coord_flip() + theme_crea() + 
  labs(title='Changes in power generation in EU countries',
       subtitle='from November 2021 to 2022',
       caption='largest EU countries by generation', y='GW', x='') +
  scale_fill_manual(values=sourcecols, guide='none') +
  scale_x_discrete(limits=rev(sources))
quicksave('reports/generation changes by country.png')

implied_cons <- get_entsog() %>% filter(type=='consumption')

allflows <- readRDS('diagnostics/allflows.RDS') %>% filter(type=='consumption')
pwr <- readRDS('diagnostics/pwr.RDS')


#hdd and cdd
read_csv("https://api.energyandcleanair.org/v1/weather?variable=HDD,CDD&format=csv") ->
  dd

dd %<>% mutate(across(variable, tolower))

pwr_demand <- pwr %>% filter(country=='EU total', source=='Total') %>% 
  mutate(region_id = countrycode(country, 'country.name.en', 'iso2c', 
                                 custom_match=c('EU total'='EU')))

dd %>% select(region_id, date, variable, value) %>% 
  spread(variable, value) %>% left_join(pwr_demand, .) -> pwr_demand

pwr_demand %>% lm(value_mw ~ hdd + cdd + as.factor(wday(date)), data=.) -> m_pwr

m_pwr %>% summary()
predict(m_pwr, pwr_demand) -> pwr_demand$value_mw_pred

pwr_demand %>% mutate(anomaly = value_mw - value_mw_pred,
                      value_mw_temperature_corrected = mean(value_mw_pred, na.rm=T) + anomaly) %>% 
  pivot_longer(c(anomaly, value_mw, value_mw_pred, value_mw_temperature_corrected)) %>% 
  group_by(name) %>% 
  mutate(across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T)) %>% 
  filter(date>='2020-01-28') ->
  pwr_plotdata

pwr_plotdata %>% filter(!grepl('corrected', name), date<today()-4) %>% 
  ggplot(aes(date, value, col=name)) + geom_line() +
  labs(title='EU power demand')


pwr_plotdata %>% filter(name=='value_mw_temperature_corrected') %>% 
  ggplot(aes(plotdate, value, col=as.factor(year))) + geom_line() +
  labs(title='EU temperature corrected power demand')

pwr_plotdata %>% group_by(name) %>% 
  mutate(yoy=get_yoy(value, date)) %>% select(date, yoy) %>% 
  slice_tail(n=1)

#gas
co2 %>% filter(sector=='Others', fuel=='Gas') %>% mutate(value_TWh = value / 55 / 3.6 / 1000) %>% 
  select(date, value_TWh) -> gas_demand

dd %>% select(region_id, date, variable, value) %>% 
  spread(variable, value) %>% left_join(gas_demand, .) -> gas_demand
gas_demand %>% filter(date>='2021-01-01') %>% 
  lm(value_TWh ~ hdd + as.factor(wday(date)), data=.) -> m_gas

m_gas %>% summary()
predict(m_gas, gas_demand) -> gas_demand$value_TWh_pred

gas_demand %>% mutate(anomaly = value_TWh - value_TWh_pred,
                      value_TWh_temperature_corrected = mean(value_TWh_pred, na.rm=T) + anomaly) %>% 
  pivot_longer(c(anomaly, value_TWh, value_TWh_pred, value_TWh_temperature_corrected)) %>% 
  group_by(name) %>% 
  mutate(across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T)) %>% 
  filter(date>='2020-01-28') ->
  gas_plotdata

gas_plotdata %>% filter(!grepl('corrected', name), date<today()-7) %>% 
  ggplot(aes(date, value, col=name)) + geom_line() +
  labs(title='EU gas demand',
       subtitle='outside power sector')

gas_plotdata %>% mutate(plotdate = date %>% 'year<-'(2022), year=year(date)) %>% 
  filter(name=='value_TWh_temperature_corrected') %>% 
  ggplot(aes(plotdate, value, col=as.factor(year))) + geom_line() +
  labs(title='EU temperature corrected gas demand',
       subtitle='outside power sector')

gas_plotdata %>% group_by(name) %>% 
  mutate(yoy=get_yoy(value, date)) %>% select(date, yoy) %>% 
  slice_tail(n=1)


dd %>% filter(year(date) %in% 2020:2022) %>% 
  group_by(variable) %>% 
  mutate(plotdate = date %>% 'year<-'(2022),
         year=as.factor(year(date)),
         across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T),
         variable_name=ifelse(variable=='cdd', 'cooling', 'heating')) %>% 
  #write_csv(file.path(output_dir, 'EU average cooling and heating needs.csv')) %>% 
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