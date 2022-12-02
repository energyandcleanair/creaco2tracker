require(tidyverse)
require(magrittr)
require(lubridate)
require(rcrea)

source('R/get_co2_daily.R')

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


co2 %>% 
  mutate(sector = case_when(fuel=='Fossil total'~'Electricity',
                            sector=='All' & fuel == 'Total'~'Total',
                            sector=='All' & fuel == 'Gas'~'Gas consumption')) %>% 
  filter(month(date)==11, !is.na(sector)) %>% 
  group_by(region, unit, sector, fuel, year=year(date)) %>% 
  summarise(across(value, mean)) -> plotdata

plotdata %>% 
  mutate(value_relative=rank(value)) %>% 
  ggplot(aes(year, value/1e6, fill=value_relative)) + geom_col() + 
  facet_wrap(~sector, scales='free_y', ncol=1) +
  theme_crea() + 
  theme(panel.border = element_rect(size=1, linetype = 'solid', color='gray')) +
  scale_x_continuous(expand=expansion(mult=.02)) + 
  x_at_zero() + 
  scale_fill_gradientn(colors=crea_palettes$change[c(1,2,5,7)], guide='none') +
  labs(title='EU November CO2 emissions by year', y='Mt/day', x='')
quicksave('reports/EU November CO2 emissions by year.png')

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

pwr %>% filter(month(date)==11, country=='EU total') %>% 
  group_by(year=year(date), country, source) %>% 
  summarise(across(value_mw, mean)) %>% 
  ggplot(aes(year, value_mw/1e3, fill=year)) + geom_col() + facet_wrap(~source) +
  labs(title='EU November power generation by source', y='GW', x='') +
  theme_crea() + x_at_zero() + scale_fill_gradientn(colors=crea_palettes$CREA[3:1], guide='none')
quicksave(file.path(output_dir, 'EU November power generation by source.png'))

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
read_csv("https://api.energyandcleanair.org/v1/weather?variable=HDD,CDD&region_id=EU&format=csv") ->
  dd

dd %<>% mutate(across(variable, tolower))

pwr_demand <- pwr %>% filter(country=='EU total', source=='Total')
dd %>% select(date, variable, value) %>% spread(variable, value) %>% left_join(pwr_demand, .) -> pwr_demand
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

pwr_plotdata %>% 
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

dd %>% select(date, variable, value) %>% spread(variable, value) %>% left_join(gas_demand, .) -> gas_demand
gas_demand %>% lm(value_TWh ~ hdd + as.factor(wday(date)), data=.) -> m_gas

m_gas %>% summary()
predict(m_gas, gas_demand) -> gas_demand$value_TWh_pred

gas_demand %>% mutate(anomaly = value_TWh - value_TWh_pred,
                      value_TWh_temperature_corrected = mean(value_TWh_pred, na.rm=T) + anomaly) %>% 
  pivot_longer(c(anomaly, value_TWh, value_TWh_pred, value_TWh_temperature_corrected)) %>% 
  group_by(name) %>% 
  mutate(across(value, zoo::rollapplyr, FUN=mean, width=28, fill=NA, na.rm=T)) %>% 
  filter(date>='2020-01-28') ->
  gas_plotdata

gas_plotdata %>% 
  ggplot(aes(date, value, col=name)) + geom_line() +
  labs(title='EU gas demand',
       subtitle='outside power sector')

gas_plotdata %>% mutate(plotdate = date %>% 'year<-'(2022), year=year(date)) %>% 
  filter(name=='value_TWh_temperature_corrected') %>% 
  ggplot(aes(plotdate, value, col=as.factor(year))) + geom_line() +
  labs(title='EU temperature corrected gas demand',
       subtitle='outside power sector')

gas_plotdata %>% group_by(name) %>% 
  mutate(yoy=get_yoy(value, date)) %>% select(yoy) %>% 
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