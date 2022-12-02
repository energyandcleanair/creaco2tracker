source('helper_functions.R')

putLast <- (function(x, last='Total') {
  factor(x, levels=x %>% unique %>% (function(lev) lev[order(lev==last, lev)]))
})


ng_all <- read_csv("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=2019-01-01&commodity=natural_gas")
c('distribution','consumption','storage_entry','storage_exit','crossborder','production') %>% #'transmission_entry','transmission_exit',
  pblapply(function(x) read_csv(paste0('https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=2019-01-01&type=', x))) %>% 
  bind_rows() -> entsog

#Gas imports + production+ storage+ implied consumption
ng_all %>% 
  filter(commodity_origin_country %in% c('Algeria', 'Azerbaijan', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia', 
                                         'United Kingdom', 'Norway')) %>% 
  group_by(across(c(starts_with('destination'), date))) %>% 
  summarise(across(value_m3, sum)) %>% 
  mutate(type='imports') ->
  inflows

entsog %>% filter(type %in% c('storage_entry','storage_exit')) %>% 
  mutate(value_m3 = value_m3 * ifelse(type=='storage_exit', -1, 1)) %>% 
  group_by(across(c(starts_with('destination'), date))) %>% 
  summarise(across(value_m3, sum)) %>% 
  mutate(type='storage drawdown') ->
  storage_changes

entsog %>% filter(type == 'production') %>% 
  filter(commodity_origin_country %in% c('Algeria', 'LNG', 'Libya', 'Netherlands', 'Albania', 'Russia', 
                                         'United Kingdom', 'Norway')) %>% 
  bind_rows(inflows, storage_changes) %>% 
  group_by(across(c(starts_with('destination'), date))) %>% 
  summarise(across(value_m3, sum)) %>% 
  mutate(type='consumption') ->
  implied_cons

bind_rows(inflows, storage_changes, implied_cons, entsog %>% filter(type == 'production')) %>% 
  filter(destination_region=='EU') %>% 
  group_by(type, date) %>% 
  summarise(across(value_m3, sum, na.rm=T)) %>% 
  group_by(type) %>% 
  mutate(value_m3=zoo::rollapplyr(value_m3, 30, mean, fill=NA),
         plotdate=date %>% 'year<-'(2022), year=as.factor(year(date))) %>% 
  filter(year(date) %in% 2021:2022, date<max(date)-7) %>% 
  ggplot(aes(plotdate, value_m3/1e9, col=type, alpha=year)) + 
  facet_wrap(~type) + 
  geom_line(size=1) +
  labs(title='Europe gas imports, storage and implied consumption', x='', 
       y='billion cubic meters per day, 30-day mean',
       col='flow') +
  theme_crea() + scale_color_crea_d('dramatic') + 
  scale_alpha_manual(values=c(.5,1),
                     guide = guide_legend(override.aes = list(col = c('gray50', 'black'), alpha=c(1,1))))
ggsave('gas imports and consumption trends.png', width=8, height=6)

#Power generation by source plus total Calvin plot
read_csv('https://api.energyandcleanair.org/power/generation?date_from=2016-01-01&aggregate_by=country,source,date&format=csv') ->
  pwr

#add total generation
pwr %<>% 
  filter(source!='Total') %>% 
  group_by(region, country, date) %>% 
  summarise(across(value_mw, sum, na.rm=T)) %>% 
  mutate(source='Total') %>% 
  bind_rows(pwr %>% filter(source!='Total'))

#add EU total
pwr %<>% filter(country!='EU total') %>% 
  group_by(date, source) %>% 
  filter(region=='EU') %>% 
  summarise(across(value_mw, sum, na.rm=T)) %>% 
  mutate(country='EU total') %>% 
  bind_rows(pwr %>% filter(country!='EU total'))

#add rolling mean
pwr %<>% group_by(region, country, source) %<>% arrange(date) %>% 
  mutate(plotdate = date %>% 'year<-'(2022), year=year(date),
         output_mw_rollmean=zoo::rollapplyr(value_mw, 28, mean, fill=NA))

#output range of values for several years
pwr %>% filter(year %in% 2016:2021) %>% 
  group_by(region, country, source, plotdate) %>% 
  summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean)) ->
  pwr_ranges


#plot by source
pwr %>% filter(date<max(date)-3, year %in% 2021:2022, country=='EU total') %>% 
  group_by(country) %>% filter(mean(value_mw, na.rm=T)>1e3) %>% 
  ggplot(aes(plotdate)) +
  facet_wrap(~source, scales='free_y') + 
  geom_ribbon(data=pwr_ranges %>% filter(country=='EU total'), 
              aes(ymin=min/1000, ymax=max/1000), fill=crea_palettes$CREA[2]) +
  geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), size=1) +
  expand_limits(y=0) +
  scale_x_datetime(date_labels = '%b') +
  labs(title='EU power generation by source', y='GW, 4-week mean', x='', col='', fill='') +
  theme_crea(legend.position='top') + 
  scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
  scale_fill_crea_d(col.index = 2)
ggsave('EU power generation by source.png', width=8, height=6)


#Thermal power fuel mix
#plot fuel mix
pwr %>% filter(source %in% c('Coal', 'Fossil Gas', 'Other'), 
               source != 'Total', year(date) %in% 2021:2022,
               country=='EU total') %>% 
  ggplot(aes(date, output_mw_rollmean, fill=source)) + geom_area(position='fill') +
  theme_crea() +
  scale_fill_crea_d('dramatic') + 
  scale_y_continuous(expand=c(0,0), labels=scales::percent) +
  scale_x_datetime(expand=c(0,0)) +
  labs(title='Thermal power fuel mix in the EU', x='', y='7-day mean')
ggsave('Thermal power fuel mix in the EU.png', width=8, height=6)



#Gas consumption+coal consumption (estimated from generation) + oil consumption extrapolated from Eurostat
require(eurostat)

consumption_codes = c("nrg_cb_sffm","nrg_cb_oilm","nrg_cb_gasm")

consumption_codes %>% lapply(get_eurostat) %>% lapply(label_eurostat) -> cons
names(cons) <- c('coal', 'oil', 'gas')

cons$coal %>% filter(grepl('Transformation input|Final consumption.*(industry sector$|other sectors$)', nrg_bal)) %>% 
  mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'others')) %>% 
  group_by(geo, sector, time, unit, siec) %>% summarise(across(values, sum, na.rm=T)) %>% 
  mutate(fuel_type='Coal') ->
  coal_cons

cons$oil %>% filter((grepl('Gross inland deliveries.*observed', nrg_bal) & siec=='Oil products') |
                      (grepl('Direct use', nrg_bal) & grepl('Crude oil, NGL', siec))) %>% 
  group_by(geo, time, unit, siec) %>% summarise(across(values, sum, na.rm=T)) %>% 
  mutate(fuel_type='Oil', sector='all') ->
  oil_cons

cons$gas %>% filter(grepl('Inland consumption.*observed|Transformation input', nrg_bal),
                    unit=='Million cubic metres') %>% 
  mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'all')) %>% 
  group_by(geo, sector, time, unit, siec) %>% summarise(across(values, sum, na.rm=T)) ->
  gas_cons

gas_cons %>% mutate(values=values*ifelse(sector=='electricity', -1, 1)) %>% 
  group_by(geo, time, unit, siec) %>% summarise(across(values, sum, na.rm=T)) %>% 
  mutate(sector='others') %>% bind_rows(gas_cons %>% filter(sector=='electricity')) %>% 
  mutate(fuel_type='gas') ->
  gas_cons

bind_rows(coal_cons, oil_cons, gas_cons) %>% 
  mutate(CO2.factor = case_when(siec=='Hard coal'~29.3*5000/7000*94.6,
                                siec=='Brown coal'~10*102,
                                siec=='Peat'~9.7*106,
                                grepl('Oil shale', siec)~6.4*108,
                                grepl('Oil products', siec)~46*72,
                                grepl('Crude oil', siec)~44*73,
                                siec=='Natural gas'~.9*42*55),
         CO2_emissions=values*CO2.factor) ->
  cons_agg

save.image('EU energy data.RData')

load('EU energy data.RData')

dts <- cons %>% bind_rows() %>% use_series(time) %>% min() %>% 
  seq.Date(today() %>% 'day<-'(1), by='month')

cons_agg %>% 
  group_by(geo, time, fuel_type, sector) %>% 
  summarise(across(CO2_emissions, sum, na.rm=T)) %>% 
  group_by(geo, fuel_type, sector) %>% 
  expand.dates('time', dts) %>% arrange(time) %>% 
  group_modify(function(df, ...) {
    df %<>% group_by(month=month(time)) %>% 
      mutate(mean3y = CO2_emissions %>% lag %>% zoo::rollapplyr(3, mean, na.rm=F, fill=NA),
             yoy = CO2_emissions / mean3y - 1) %>% ungroup %>% select(-month)
    
    df$yoy %>% zoo::rollapplyr(3, mean, na.rm=F) %>% na.omit %>% tail(1) -> latest_yoy
    
    latest_data <- max(df$time[!is.na(df$CO2_emissions)])
    
    df %>% mutate(CO2_emissions = ifelse(time>latest_data,
                                         mean3y * (1+latest_yoy),
                                         CO2_emissions))
  }) -> cons_filled

cons_filled %<>% ungroup %>% 
  mutate(iso2c=get_iso2(geo),
         EU=iso2c %in% codelist$iso2c[!is.na(codelist$eu28)] & iso2c != 'GB')
  

cons_filled %>% 
  filter(EU) %>% 
  group_by(time, fuel_type, sector) %>% 
  summarise(coverage = sum(CO2_emissions[!is.na(yoy)], na.rm=T)/sum(CO2_emissions, na.rm=T),
            across(CO2_emissions, sum, na.rm=T)) ->
  co2

co2 %>% filter(CO2_emissions>0, year(time)>=2018, time<='2022-09-01') %>% 
  mutate(year=as.factor(year(time)), plotdate=time %>% 'year<-'(2022)) %>% 
  ggplot(aes(plotdate, CO2_emissions, col=year)) + geom_line() + facet_wrap(~paste(fuel_type, sector))

get_yoy <- function(x, date) {
  lastyr <- date
  year(lastyr) %<>% subtract(1)
  ind = match(lastyr, date)
  x / x[ind] - 1
}

pwr %>% filter(source %in% c('Coal', 'Fossil Gas'), country=='EU total') %>% 
  group_by(source) %>% 
  mutate(crea_yoy = get_yoy(value_mw, date),
         fuel_type = recode(source, 'Fossil Gas'='gas'),
         sector='electricity') %>% 
  rename(value=value_mw) ->
  pwr_yoy

implied_cons %>% filter(destination_region=='EU') %>% 
  group_by(date) %>% 
  summarise(across(value_m3, sum)) %>% 
  mutate(crea_yoy = get_yoy(value_m3, date), fuel_type='gas', sector='all') %>% 
  rename(value=value_m3) ->
  gas_yoy

bind_rows(pwr_yoy, gas_yoy) %>% ungroup %>% 
  mutate(date=date(date)) %>% filter(date<=today()-5) %>% 
  select(date, fuel_type, sector, crea_value=value, crea_yoy) ->
  crea_yoy

co2 %<>% filter(fuel_type=='gas', sector != 'all') %>% 
  group_by(time, fuel_type) %>% 
  summarise(coverage=weighted.mean(coverage, CO2_emissions),
            across(CO2_emissions, sum)) %>% 
  mutate(sector='all') %>% 
  bind_rows(co2 %>% filter(fuel_type!='gas' | sector != 'all'))

co2 %>% ungroup %>% filter(CO2_emissions>0) %>% distinct(fuel_type, sector) -> grps

dts <- seq.Date(min(co2$time), max(crea_yoy$date), by='d')
co2 %>% group_by(fuel_type, sector) %>% 
  rename(month=time) %>% 
  full_join(tibble(date=dts, month=dts %>% 'day<-'(1))) %>% 
  mutate(CO2_emissions = CO2_emissions/days_in_month(date)) %>% 
  right_join(grps) %>% 
  left_join(crea_yoy) ->
  co2_daily

co2_daily %<>% 
  group_by(fuel_type, sector) %>% 
  mutate(has_both=!is.na(CO2_emissions+crea_value) & date>='2021-03-01',
         crea_eurostat_ratio = mean(crea_value[has_both]) / mean(CO2_emissions[has_both]),
         #crea_CO2 = CO2_emissions / (1+eurostat_yoy) * (1+crea_yoy),
         #eurostat_yoy = get_yoy(CO2_emissions, date),
         crea_CO2 = crea_value / crea_eurostat_ratio,
         CO2_hybrid = case_when(!is.na(crea_CO2) & date>='2021-03-01'~crea_CO2,
                                  #CO2_emissions * coverage + crea_CO2 * 1-coverage,
                                T~CO2_emissions))

co2_daily %<>% filter(fuel_type=='gas') %>% 
  group_by(date, fuel_type) %>% 
  summarise(across(c(CO2_hybrid, CO2_emissions), ~.x[sector=='all']-.x[sector=='electricity'])) %>% 
  mutate(sector='others') %>% 
  bind_rows(co2_daily %>% filter(fuel_type!='gas' | sector %notin% c('others', 'all')))

co2_daily %<>% filter(fuel_type!='total') %>% 
  group_by(date) %>% 
  summarise(across(c(CO2_hybrid, CO2_emissions), sum)) %>% 
  mutate(fuel_type='total', sector='all') %>% 
  bind_rows(co2_daily %>% filter(fuel_type!='total'))


co2_daily %>% filter(year(date)>=2018) %>% 
  group_by(sector, fuel_type) %>% 
  mutate(CO2_30d = zoo::rollapplyr(CO2_hybrid, 30, mean, fill=NA)) %>% 
  mutate(year=as.factor(year(date)), plotdate=date %>% 'year<-'(2022)) %>% 
  ggplot(aes(plotdate, CO2_30d/1e6, col=year)) + 
  geom_line() + 
  facet_wrap(~paste(fuel_type, sector), scales='free_y') +
  expand_limits(y=0) + x_at_zero() + scale_x_date(expand=c(0,0)) +
  theme_crea() +
  labs(title="EU CO2 emissions", y='Mt/day, 30-day mean', x='') +
  scale_color_crea_d()




#Russian gas import volume vs price vs revenue
ng_all %>% 
  filter(destination_region=='EU', commodity_origin_country=='Russia') %>%
  group_by(date) %>% 
  summarise(across(c('million cubic meters/day'=value_m3, 
                     'million EUR/day'=value_eur), sum, na.rm=T)) %>% 
  pivot_longer(-date) %>% 
  group_by(name) %>% mutate(value_rollmean=rollmean_date(value, date, 30)) %>% 
  filter(date>='2021-02-01') %>% 
  write_csv(file.path(output_dir, 'EU gas imports volume and value.csv')) %>% 
  ggplot(aes(date, value_rollmean/1e6, col=name)) + geom_line(size=1) +
  expand_limits(y=0) +
  theme_crea(legend.position='top') + 
  scale_color_crea_d('dramatic', guide=guide_legend(nrow=1)) +
  labs(title="Fossil gas exports from Russia to the EU",
       y='30-day mean', x='', col='') +
  x_at_zero()
ggsave(file.path(output_dir, 'EU gas imports volume and value.png'), width=8, height=6)

ng_all %>% group_by(commodity_origin_country) %>% filter(date>Sys.Date()-90) %>% 
  summarise(across(starts_with('value'), sum, na.rm=T)) %>% arrange(desc(value_m3))

#mix of impors into EU
ng_all %>% ungroup %>% 
  filter(destination_region=='EU', commodity=='natural_gas') %>% 
  mutate(commodity_origin_country = 
           case_when(commodity_origin_country %in% c('Azerbaijan','Algeria', 'LNG', 'Libya', 
                                                     'Albania', 'Russia', 'Norway') ~ 
                       commodity_origin_country, 
                     commodity_origin_country %in% c('Netherlands', 'Romania', 'Poland')~'EU',
                     T~'others')) %>% 
  mutate(across(commodity_origin_country, recode, 'Albania'='Azerbaijan')) %>% 
  group_by(across(c(commodity_origin_country, contains('date'), contains('destination')))) %>% 
  summarise(across(starts_with('value'), sum, na.rm=T)) ->
  ng_all2

ng_all2 %<>% filter(destination_country!='EU total', destination_region=='EU') %>% 
  group_by(date, commodity_origin_country) %>% 
  summarise(across(value_m3, sum, na.rm=T)) %>% 
  mutate(destination_country='EU total') %>% 
  bind_rows(ng_all2 %>% filter(destination_country!='EU total'))

ng_all2$destination_country %<>% putLast('EU total')

ng_all2 %>% ungroup %>% 
  group_by(commodity_origin_country, destination_country) %>% 
  expand.dates('date') %>% 
  mutate(value_m3 = rollmean_date(value_m3, date, 30)) %>% 
  ggplot(aes(date, value_m3, fill=commodity_origin_country)) + geom_col(position='fill', width=1) +
  facet_wrap(~destination_country, scales='free_y') +
  scale_fill_crea_d() +
  theme_crea() + 
  labs(title='EU gas imports by origin', x='', y='30-day mean') +
  scale_x_date(expand=expansion(mult=0)) +
  scale_y_continuous(labels=scales::percent, expand=expansion(mult=0))
ggsave('EU gas imports by origin and destination.png', width=8, height=6)

ng_all2 %>% ungroup %>% filter(destination_country=='EU total') %>% 
  group_by(commodity_origin_country) %>% 
  expand.dates('date') %>% 
  mutate(value_m3 = rollmean_date(value_m3, date, 30)) %>% 
  filter(date>='2021-02-01', date<Sys.Date()-7) %>% 
  ggplot(aes(date, value_m3/1e9, fill=commodity_origin_country)) + geom_col(width=1) +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[c(1:4,6,7)], crea_palettes$dramatic))) +
  theme_crea() + 
  labs(title='EU gas imports by origin', x='', y='bcm/day, 30-day mean', fill='origin') +
  scale_x_date(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) #, labels=scales::percent
ggsave('EU gas imports by origin.png', width=8, height=6)
