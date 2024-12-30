




validate_power <- function(pwr_demand=download_pwr_demand(), folder="validation"){

  ember_explorer <- ember.get_eu_generation()
  ember_1 <- ember_explorer %>%
    mutate(source=recode(variable,
                         Gas='Fossil Gas',
                         Bioenergy='Other',
                         `Other Fossil`='Other',
                         `Other Renewables`='Other')) %>%
    group_by(year, source) %>%
    summarise(value_twh=sum(generation_twh),
              data_source='EMBER (Data Explorer)')


  filepath <- "data/ember_yearly_full_release_long_format.csv"
  url <- "https://storage.googleapis.com/emb-prod-bkt-publicdata/public-downloads/yearly_full_release_long_format.csv"
  if(!file.exists(filepath)){
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
    download.file(url, filepath)
  }

  ember_catalogue <- read_csv(filepath)
  ember_2 <- ember_catalogue %>%
    filter(`Area type`=='Country',
           EU==1, Unit=='TWh', Category=='Electricity generation') %>%
    filter(Variable %in% c(
      'Coal','Gas','Hydro','Nuclear','Other Fossil', 'Other Renewables',
      'Solar', 'Wind', 'Bioenergy'
    )) %>%
    mutate(source=recode(Variable,
                         Gas='Fossil Gas',
                         Bioenergy='Other',
                         `Other Fossil`='Other',
                         `Other Renewables`='Other')) %>%
    group_by(year=Year,
             source) %>%
    summarise(value_twh = sum(Value),
              data_source='EMBER (Data Catalogue)') %>%
    ungroup()




  data <- bind_rows(

    ember_1,
    ember_1 %>% group_by(year, data_source) %>%
      summarise(value_twh=sum(value_twh), source='Total'),

    ember_2,
    ember_2 %>% group_by(year, data_source) %>%
      summarise(value_twh=sum(value_twh), source='Total'),



  pwr_demand %>%
    filter(country=="EU total", date < "2024-01-01") %>%
    group_by(year=year(date), source) %>%
    summarise(value_twh=sum(value_mwh, na.rm=T) / 1e6,
              data_source='ENTSOE')
  )


  ggplot(data) +
    geom_line(aes(year, value_twh, col=data_source)) +
    facet_wrap(~source) +
    rcrea::scale_y_crea_zero() +
    labs(title='Comparison of EU power generation',
         subtitle="TWh",
         x=NULL,
         y=NULL)

}
