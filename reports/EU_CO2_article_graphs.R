read_csv("https://api.energyandcleanair.org/co2/emission?format=csv&date_from=2016-01-01") -> co2_tracker
output_dir='reports'


co2_tracker$value[co2_tracker$fuel_sector=='Coal - Others' &
                  year(co2_tracker$date)==2022 & 
                    month(co2_tracker$date)==10] %<>% multiply_by(.95)

end_date = today()-3

co2_tracker %>% 
  filter(date<=end_date,
         month(date) != month(end_date) | day(date) <= day(end_date)) %>% 
  group_by(fuel, sector, fuel_sector = paste(fuel, '-', sector), unit, month=date %>% 'day<-'(1)) %>% 
  summarise(across(value, mean)) %>% 
  mutate(value_3m = zoo::rollapplyr(value, 3, mean, na.rm=T, fill=NA)) %>% 
  group_modify(function(df, ...) {
    last.year.dates <- df$month
    year(last.year.dates) %<>% subtract(1)
    last.year.ind <- match(last.year.dates, df$month)
    df %>% mutate(yoy = value / value[last.year.ind] - 1,
                  yoy_3m = value_3m / value_3m[last.year.ind] - 1)
  }) ->
  co2_yoy

co2_yoy %>% filter(fuel_sector=='Total - All') %>% 
  select(month, value_3m, yoy, yoy_3m, unit) %T>% (function(x) print(tail(x))) %>% 
  write_csv(file.path(output_dir,'EU CO2 emissions growth.csv')) -> co2plot

co2plot %>% filter(year(month)>=2018) %>% 
  ggplot(aes(month, yoy_3m, fill=yoy_3m>0)) + geom_col() + 
  theme_crea() +
  scale_y_continuous(label=scales::percent) +
  scale_fill_manual(values=unname(crea_palettes$change[c(1,7)]), guide='none') + 
  labs(title='EU CO2 emissions growth', subtitle='3-month average',
       y='change year-on-year, 3-month mean', x='') +
  scale_x_date(expand=expansion(mult=.01))
quicksave(file.path(output_dir, 'EU CO2 emissions growth, 3-month mean.png'))

co2plot %>% 
  ggplot(aes(month, yoy, fill=yoy>0)) + geom_col() + 
  theme_crea() +
  scale_y_continuous(label=scales::percent) +
  scale_fill_manual(values=unname(crea_palettes$change[c(1,7)]), guide='none') + 
  labs(title='EU monthly CO2 emissions growth',
       y='change year-on-year', x='') +
  scale_x_date(expand=expansion(mult=.01))
quicksave(file.path(output_dir, 'EU CO2 emissions growth.png'))

co2plot %>% 
  ggplot(aes(month, yoy, fill=yoy>0)) + geom_col() + 
  theme_crea() +
  scale_y_continuous(label=scales::percent) +
  scale_fill_manual(values=unname(crea_palettes$change[c(1,7)]), guide='none') + 
  labs(title='欧盟能源二氧化碳排放量同比变化',
       y='同比变化', x='') +
  scale_x_date(expand=expansion(mult=.01), labels = function(x) paste0(year(x), "年"))
quicksave(file.path(output_dir, 'EU CO2 emissions growth ZH.png'))



co2_yoy %>% filter(!grepl('Total - All|Fossil total|Gas - All', fuel_sector), year(month)>=2018) %>% 
  select(month, value, yoy, unit) %>% 
  write_csv(file.path(output_dir,'EU CO2 emissions growth by sector and fuel.csv')) %>% 
  ggplot(aes(month, yoy, fill=yoy>0)) + 
  facet_wrap(~fuel_sector) + 
  geom_col() + 
  theme_crea() +
  scale_y_continuous(label=scales::percent) +
  scale_fill_manual(values=unname(crea_palettes$change[c(1,7)]), guide='none') + 
  labs(title='EU monthly CO2 emissions growth by sector and fuel',
       y='change year-on-year', x='') +
  scale_x_date(expand=expansion(mult=.01))
quicksave(file.path(output_dir, 'EU CO2 emissions growth by sector and fuel.png'))

co2_yoy %>% filter(fuel_sector!='Total - All', year(month)>=2018) %>% 
  select(month, value, yoy, unit) %>% 
  mutate(fuel = recode(fuel, "Coal"="燃煤", "Gas"="燃气", "Oil"="石油"),
         sector = recode(sector, "Electricity"="发电行业", "Others"="其他行业",
                         "All"="所有的行业"),
         fuel_sector=paste(fuel, "-", sector)) %>% 
  ggplot(aes(month, yoy, fill=yoy>0)) + 
  facet_wrap(~fuel_sector) + 
  geom_col() + 
  theme_crea() +
  scale_y_continuous(label=scales::percent) +
  scale_fill_manual(values=unname(crea_palettes$change[c(1,7)]), guide='none') + 
  labs(title='欧盟能源二氧化碳排放量同比变化',
       subtitle='按行业和燃料细分',
       y='同比变化', x='') +
  scale_x_date(expand=expansion(mult=.01), labels = function(x) paste0(year(x), "年")) +
  theme(axis.text.x=element_text(hjust=.2))
quicksave(file.path(output_dir, 'EU CO2 emissions growth by sector and fuel ZH.png'))


ng_all %>% 
  filter(destination_region=='EU', commodity_origin_country=='Russia',
         month(date) %in% 1:9) %>% 
  group_by(year(date)) %>% summarise(across(value_m3, sum)) %>% 
  mutate(value_TWh=value_m3/1e9*11.63*.9)


pwr  %>% 
  filter(country %in% c('EU total', 'France', 'Germany'), month(date) %in% 1:9, year(date) %in% 2021:2022) %>% 
  #mutate(source = ifelse(grepl('Wind|Solar', source), 'W&S', source)) %>% 
  group_by(country, source, year=year(date)) %>% summarise(across(value_mw, sum)) %>%
  mutate(value_TWh = value_mw*24/1e6) %>% 
  summarise(change_TWh = value_TWh[year==2022]-value_TWh[year==2021],
            change_perc = value_TWh[year==2022]/value_TWh[year==2021]-1)
  
pwr  %>% 
  filter(source=='Hydro', region=='EU', month(date) %in% 1:9, year(date) %in% 2021:2022) %>% 
  group_by(country, source, year=year(date)) %>% summarise(across(value_mw, sum)) %>%
  mutate(value_mw = value_mw*24/1e6) %>% 
  summarise(value_mw = value_mw[year==2022]-value_mw[year==2021]) %>% 
  arrange(value_mw)

co2_tracker %>% 
  group_by(fuel, year=year(date)) %>% 
  filter(month(date) %in% 1:6, year %in% 2021:2022) %>% 
  summarise(across(value, sum)) %>% 
  mutate(value_TWh = value / case_when(fuel=='Coal'~94.6,
                                       fuel=='Oil'~72,
                                       fuel=='Gas'~55) / 3.6) %>% 
  summarise(change_TWh = value_TWh[2]-value_TWh[1],
            change_perc=value_TWh[2]/value_TWh[1]-1)



c('crossborder') %>% #'transmission_entry','transmission_exit',
  pblapply(function(x) read_csv(paste0('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=2019-01-01&type=', x))) %>% 
  bind_rows() -> entsog

ng_all <- read_csv("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=2019-01-01&commodity=natural_gas")

entsog %>% filter(commodity_origin_iso2=='RU', date > '2021-01-01', date<today(),
                  destination_region=='EU') %>% 
  group_by(destination_country) %>% summarise(across(value_m3, sum)) %>% arrange(desc(value_m3)) ->
  ng_ranking

entsog %>% filter(commodity_origin_iso2=='RU', destination_region=='EU') %>% 
  mutate(destination_country = ifelse(destination_country %in% ng_ranking$destination_country[1:5],
                                      destination_country, 'Others')) %>% 
  group_by(destination_country, date) %>% summarise(across(value_m3, sum)) %>% 
  expand.dates('date') %>% 
  mutate(across(value_m3, rollmean_date, dates=date, width=30)) %>% 
  filter(date > '2021-01-01') %>% 
  write_csv(file.path(output_dir, 'Pipeline gas exports.csv')) %>% 
  ggplot(aes(date, value_m3/1e6, fill=destination_country)) + geom_area() +
  theme_crea() + scale_fill_crea_d(name='destination country') + snug_x_date + x_at_zero() + 
  labs(title='Pipeline gas exports from Russia to the EU',
       subtitle='by destination country',
       x='', y='mln m3/day, 30-day mean')
quicksave(file.path(output_dir, 'Pipeline gas exports.png'))


entsog %>% filter(commodity_origin_iso2=='RU', destination_region=='EU') %>% 
  mutate(destination_country = ifelse(destination_country %in% ng_ranking$destination_country[1:5],
                                      destination_country, 'Others')) %>% 
  group_by(destination_country, date) %>% summarise(across(value_m3, sum)) %>% 
  expand.dates('date') %>% 
  mutate(across(value_m3, rollmean_date, dates=date, width=30)) %>% 
  filter(date > '2021-01-01') %>% 
  write_csv(file.path(output_dir, 'Pipeline gas exports.csv')) %>% 
  ggplot(aes(date, value_m3/1e6, fill=destination_country)) + geom_area() +
  theme_crea() + scale_fill_crea_d(name='destination country') + snug_x_date + x_at_zero() + 
  labs(title='Pipeline gas exports from Russia to the EU',
       subtitle='by destination country',
       x='', y='mln m3/day, 30-day mean')
quicksave(file.path(output_dir, 'Pipeline gas exports.png'))

entsog %>% filter(commodity_origin_iso2=='RU', destination_region=='EU') %>% 
  mutate(destination_country = ifelse(destination_country %in% ng_ranking$destination_country[1:5],
                                      destination_country, 'Others')) %>% 
  group_by(destination_country, date) %>% summarise(across(value_m3, sum)) %>% 
  expand.dates('date') %>% 
  mutate(across(value_m3, rollmean_date, dates=date, width=30),
         destination_country=recode(destination_country,
                                    "Germany"="德国",
                                    "Slovakia"="斯洛伐克",
                                    "Poland"="波兰",
                                    "Bulgaria"="保加利亚",
                                    "Hungary"="匈牙利",
                                    "Others"="其他欧盟国家")) %>% 
  filter(date > '2021-01-01') %>% 
  ggplot(aes(date, value_m3/1e8, fill=destination_country)) + geom_area() +
  theme_crea() + scale_fill_crea_d() + snug_x_date + x_at_zero() + 
  labs(title='俄罗斯对欧盟管道天然气出口量',
       subtitle='按进口国家分别',
       x='', y='每天亿立方米, 30天平均值',
       fill='进口国家')
quicksave(file.path(output_dir, 'Pipeline gas exports ZH.png'))



#Power generation by source plus total Calvin plot
2016:2022 %>% 
  lapply(function(yr) read_csv(paste0('https://api.energyandcleanair.org/power/generation?date_from=',
  yr,'-01-01&date_to=',yr,'-12-31&aggregate_by=country,source,date&region=EU&format=csv'))) ->
  pwrdata

pwrdata %<>% bind_rows %>% filter(region=='EU')

#add total generation
pwrdata %<>% 
  filter(source!='Total') %>% 
  group_by(region, country, date) %>% 
  summarise(across(value_mw, sum, na.rm=T)) %>% 
  mutate(source='Total') %>% 
  bind_rows(pwrdata %>% filter(source!='Total'))

#add EU total
pwrdata %<>% filter(country!='EU total') %>% 
  group_by(date, source) %>% 
  filter(region=='EU') %>% 
  summarise(across(value_mw, sum, na.rm=T)) %>% 
  mutate(country='EU total') %>% 
  bind_rows(pwr %>% filter(country!='EU total'))

rollmean_date <- function(x, dates, width=7) {
  x.out <- x
  x.out[] <- NA
  for(i in 1:length(x))
    x.out[i] <- sum(x[dates %in% (dates[i]-0:(width-1))], na.rm=T)/width
  return(x.out)
}

#add rolling mean
pwrdata %<>% group_by(region, country, source) %>% arrange(date) %>% 
  mutate(date=date(date),
         plotdate = date %>% 'year<-'(2022), year=year(date),
         output_mw_rollmean=rollmean_date(value_mw, date, 28))

#output range of values for several years
pwrdata %>% filter(year %in% 2016:2021, date>min(date)+27) %>% 
  group_by(region, country, source, plotdate) %>% 
  summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean)) ->
  pwr_ranges


#plot by source
pwrdata %>% filter(date<max(date)-3, year %in% 2021:2022, country=='EU total') %>% 
  group_by(country) %>% filter(mean(value_mw, na.rm=T)>1e3) %>% 
  #write_csv(file.path(output_dir, 'EU power generation by source.csv')) %>% 
  ggplot(aes(plotdate)) +
  facet_wrap(~source, scales='free_y') + 
  geom_ribbon(data=pwr_ranges %>% filter(country=='EU total'), 
              aes(ymin=min/1000, ymax=max/1000, fill='2016–2021 range')) +
  geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), size=1) +
  expand_limits(y=0) +
  scale_x_date(date_labels = '%b') +
  labs(title='EU power generation by source', y='GW, 4-week mean', x='', col='', 
       fill='') +
  theme_crea(legend.position='top') + 
  scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  scale_fill_crea_d(col.index = 2)
quicksave(file.path(output_dir, 'EU power generation by source.png'))


recodeSources = function(df) {
  df %>% mutate(source=recode(source, "Coal"="燃煤","Fossil Gas"="燃气","Hydro"="水电",
                              "Nuclear"="核电","Other"="其他","Solar"="光伏",
                              "Total"="总计","Wind"="风电","Biomass"="生物"))
}

pwr %>% filter(date<max(date)-3, year %in% 2021:2022, country=='EU total') %>% 
  group_by(source) %>% filter(mean(value_mw, na.rm=T)>.5e3) %>% 
  recodeSources %>% 
  ggplot(aes(plotdate)) +
  facet_wrap(~source, scales='free_y') + 
  geom_ribbon(data=pwr_ranges %>% filter(country=='EU total') %>% recodeSources, 
              aes(ymin=min/10, ymax=max/10, fill='2016–2021年范围')) +
  geom_line(aes(y=output_mw_rollmean/10, col=paste0(year, '年')), size=1) +
  expand_limits(y=0) +
  scale_x_date(date_labels = '%b') +
  labs(title='欧盟发电量与来源', y='万千瓦, 四周平均值', x='', col='', 
       fill='') +
  theme_crea(legend.position='top') + 
  scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  scale_fill_crea_d(col.index = 2)
quicksave(file.path(output_dir, 'EU power generation by source ZH.png'))


#Thermal power fuel mix
#plot fuel mix
pwrdata %>% mutate(across(date, lubridate::date)) %>% 
  filter(source %in% c('Coal', 'Fossil Gas', 'Other'), 
         source != 'Total', year(date) %in% 2018:2022,
         country=='EU total') %>% 
  write_csv(file.path(output_dir, 'Thermal power fuel mix in the EU.csv')) %>% 
  ggplot(aes(date, output_mw_rollmean, fill=source)) + geom_area(position='fill') +
  theme_crea() +
  scale_fill_crea_d('dramatic') + 
  scale_y_continuous(expand=c(0,0), labels=scales::percent) +
  scale_x_date(expand=c(0,0)) +
  labs(title='Thermal power fuel mix in the EU', x='', y='7-day mean')
quicksave(file.path(output_dir, 'Thermal power fuel mix in the EU.png'))
