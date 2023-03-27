require(tidyverse); require(magrittr); require(lubridate)

read_csv("https://api.energyandcleanair.org/co2/emission?format=csv&date_from=2016-01-01") -> co2_tracker

co2_tracker %>% filter(fuel=='Gas', sector=='Others') %>% 
  group_by(year=year(date)) %>% filter(year<2023) %>%  
  summarise(across(value, sum)) %>% 
  ggplot(aes(year, value)) + geom_col()



co2_tracker %>% 
  group_by(fuel, sector, year=year(date)) %>% filter(year<2023) %>%  
  summarise(across(value, sum)) %>% 
  summarise(change=value[year==2022]-value[year==2021],
            yoy=value[year==2022]/value[year==2021]-1,
            value_2022=value[year==2022])
