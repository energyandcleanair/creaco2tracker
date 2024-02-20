validate_co2 <- function(co2_daily=NULL, folder="diagnostics"){

  library(ggrepel)
  dir.create(folder, F, T)

  co2_crea <- creahelpers::default_if_null(co2_daily, download_co2_daily(date_from="1990-01-01")) %>%
    filter(sector=='All',
           fuel=='Total',
           region=='EU') %>%
    group_by(year=year(date)) %>%
    summarise(value=sum(value)/1e6,
              unit="mt",
              source='CREA') %>%
    filter(year < 2024)


  co2_validate <-
    bind_rows(
      read_csv('data/ghg-emissions-climatewatch.csv') %>% mutate(source='Climate Watch'),
      read_csv('data/ghg-emissions-unfccc.csv') %>% mutate(source='UNFCCC'),
      read_csv('data/ghg-emissions-gcp.csv') %>% mutate(source='GCP'),
      read_csv('data/ghg-emissions-pik.csv') %>% mutate(source='PIK')
      ) %>%
    filter(iso=='EUU') %>%
    tidyr::gather(key="year", value="value",
                  -c(iso, `Country/Region`, unit, source)) %>%
    mutate(year=as.numeric(year))

  co2_projected <- co2_crea %>%
    filter(year %in% seq(2021, 2023)) %>%
    arrange(year) %>%
    mutate(ratio = value/value[year==2021]) %>%
    select(year, ratio) %>%
    cross_join(
      co2_validate %>%
        filter(year==2021) %>%
        select(-c(year))) %>%
    mutate(value=value * ratio)


  (bind_rows(
    co2_crea %>% filter(year >= 1990) %>% mutate(type='estimated'),
    co2_validate %>% mutate(type='estimated'),
    co2_projected %>% mutate(type='projected')
  ) %>%
      write_csv(file.path(folder, "validation.csv")) %>%
      # filter(type=="estimated") %>%
      mutate(
        source=factor(source, levels=c("CREA", unique(co2_validate$source)))
      ) %>%
      # filter(year >= 1990) %>%
      ggplot() +
      geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source, linetype=type)) +
      scale_x_continuous(limits=c(min(co2_crea$year), NA)) +
      scale_alpha_manual(values=c(0.9, 1, 1, 1, 1)) +
      scale_linewidth_manual(values=c(1.6, 0.5, 0.5, 0.5, 0.5)) +
      scale_color_manual(values=unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red")])) +
      rcrea::theme_crea() +
      rcrea::scale_y_crea_zero() +
      labs(title="EU CO2 emissions from fossil fuels",
           subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
           y=NULL,
           x=NULL,
           linewidth="Source",
           linetype=NULL,
           alpha="Source",
           color="Source",
           caption="Source: CREA analysis based on Climate Watch data. Agriculture and LULUCF are not included in this comparison.") -> plt)

  plt
  quicksave(file.path(folder, "validation.jpg"), plot=plt)


  # Create a version with hline and vline -----------------------------------
  interpolated <- co2_validate %>%
    filter(source %in% c('PIK', 'GCP')) %>%
    group_by(source) %>%
    arrange(year) %>%
    left_join(co2_projected %>%
                filter(source %in% c('PIK', 'GCP'),
                       year==2023) %>% select(target_value=value, source)) %>%
    dplyr::summarise(

      interpolated_year = {
        # Finding the two points around the target value
        lower = max(year[value < target_value[1]], na.rm = TRUE)
        upper = min(year[value > target_value[1]], na.rm = TRUE)
        lower_val = value[year == lower]
        upper_val = value[year == upper]

        # Linear interpolation
        if (!is.na(lower) && !is.na(upper) && lower_val != upper_val) {
          lower + (target_value[1] - lower_val) / (upper_val - lower_val) * (upper - lower)
        } else {
          NA_real_ # NA if interpolation is not possible
        }
      })


  (plt +
    scale_x_continuous(limits=c(1940, NA)) +
    geom_hline(
      data = co2_projected %>% filter(year==max(year), source %in% c('PIK', 'GCP')),
      aes(yintercept = value / 1e3, col=source), linetype='solid', alpha=0.2, linewidth=1,
      show.legend = F) +
    geom_vline(
      data = interpolated,
      aes(xintercept = interpolated_year, col=source), linetype='solid', alpha=0.2, linewidth=1,
      show.legend = F) +
    # Indicate the interpolated value
    geom_text_repel(
      data = interpolated,
      # inherit.aes = F,
      aes(x = interpolated_year, y = 4, label = round(interpolated_year), col=source, alpha="CREA"),
      size = 4,
      show.legend = F,
      segment.size = 0.2,
      segment.alpha = 0,
      segment.color = 'grey50',
      direction = 'x',
      nudge_y = 0.5,
      # add padding around text
      box.padding = 0.5,
      min.segment.length = 0.5,
      ) +
    labs(
      caption=paste(c("Source: Climate Watch and CREA's own estimates based on ENTSOE, ENTSOG, EUROSTAT and IPCC.",
    "Agriculture and LULUCF are not included. GCP and PIK times series are extended to 2024 using CREA’s estimates."), collapse="\n")) -> plt_full)


  quicksave(file.path(folder, "validation_full.jpg"),
            plot=plt_full,
            width=8,
            height=5,
            scale=1,
            logo_scale=1.4)


  # Check fuel --------------------------------------------------------------
  co2_yearly <- co2_daily %>%
    filter(fuel!='Total',
           region=='EU') %>%
    group_by(year=year(date), fuel) %>%
    summarise(value=sum(value)/1e6,
              unit="mt",
              source='CREA') %>%
    filter(year < 2022)

  co2_validate
  ggplot(co2_yearly) +
    geom_area(aes(year, value, fill=fuel)) +
    geom_line(data=co2_validate %>% filter(year >= 1990), aes(year, value, col=source)) +
    geom_line(data=co2_yearly %>% filter(year >= 1990), aes(year, value, col=fuel))

  co2_monthly <- co2_daily %>%
    filter(fuel!='Total',
           region=='EU') %>%
    group_by(date=floor_date(date, 'month'), fuel) %>%
    summarise(value=sum(value)/1e6,
              unit="mt",
              source='CREA')


  ggplot(co2_monthly) +
    geom_area(aes(date, value, fill=fuel)) +
    geom_line(data=co2_validate %>% filter(year >= 1990), aes(year, value, col=source)) +
    geom_line(data=co2_yearly %>% filter(year >= 1990), aes(year, value, col=fuel))


# Are we understimating any? ----------------------------------------------
  summary(co2_validate %>% filter(source=='PIK') %>%
    left_join(co2_yearly %>% select(year, fuel, value) %>%
                tidyr::spread(fuel, value)) %>%
    filter(!is.na(Oil)) %>%
    lm(value - Oil - Gas ~ Coal + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(Oil)) %>%
            lm(value - Oil - Coal ~ Gas + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(Oil)) %>%
            lm(value - Gas - Coal ~ Oil + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(Oil)) %>%
            lm(value ~ Gas + Coal + Oil + 0, data=.))

}



validate_power <- function(pwr_demand=get_pwr_demand(), folder="validation"){

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



  ember_catalogue <- read_csv('data/yearly_full_release_long_format.csv')
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
