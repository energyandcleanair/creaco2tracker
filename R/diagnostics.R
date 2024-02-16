diagnostic_pwr <- function(pwr_demand, diagnostic_folder="diagnostics"){

  dir.create(diagnostic_folder, showWarnings = FALSE)

  #add rolling mean
  pwr_demand <- pwr_demand %>%
    group_by(region, country, source) %>%
    arrange(date) %>%
    mutate(plotdate = date %>% 'year<-'(2022),
           year=year(date),
           output_mw_rollmean=zoo::rollapplyr(value_mw, 7, mean, fill=NA))

  #output range of values for several years
  pwr_ranges <- pwr_demand %>% filter(year %in% 2015:2021) %>%
    group_by(region, country, source, plotdate) %>%
    summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean))

  #plot by source
  if(!is.null(diagnostic_folder)){

    plt <- pwr_demand %>%
      filter(date<max(date)-3, year %in% 2021:2022, country=='EU total') %>%
      group_by(country) %>%
      filter(mean(value_mw, na.rm=T)>1e3) %>%
      ggplot(aes(plotdate)) +
      facet_wrap(~source, scales='free_y') +
      geom_ribbon(data=pwr_ranges %>% filter(country=='EU total'),
                  aes(ymin=min/1000, ymax=max/1000), fill=crea_palettes$CREA[2]) +
      geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), linewidth=1) +
      expand_limits(y=0) +
      # scale_x_datetime(date_labels = '%b') +
      labs(title='EU power generation by source', y='GW, 7-day mean', x='', col='', fill='') +
      rcrea::theme_crea(legend.position='top') +
      rcrea::scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
      rcrea::scale_fill_crea_d(col.index = 2)

    ggsave(file.path(diagnostic_folder, 'EU power generation by source.png'),
           width=8, height=6, bg='white', plot=plt)
  }
}


diagnostic_eurostat_cons <- function(eurostat_cons, iso2s, diagnostic_folder="diagnostics"){

  dir.create(diagnostic_folder, showWarnings = FALSE)

  # Plot heatmap of consumption by sector

  if(!is.null(diagnostic_folder)){
    (plt <- eurostat_cons %>%
       filter(iso2 %in% iso2s) %>%
       group_by(geo, fuel_type, sector, unit, time) %>%
       summarise(values=sum(values)) %>%
      ggplot(aes(time, values, col=sector)) +
      geom_line() +
       rcrea::scale_y_crea_zero() +
       ggh4x::facet_grid2(geo ~ glue("{fuel_type}\n({unit})"), scales = "free_y", independent = "y") +
      # facet_wrap(geo ~ glue("{fuel_type} ({unit})"), scales='free_y')) +
      rcrea::theme_crea())

    ggsave(file.path(diagnostic_folder, 'eurostat_cons.png'),
           width=10, height=min(30,max(4, 1.5*length(iso2s))), bg='white', plot=plt, scale=1.5)
  }
}

diagnostic_co2 <- function(co2_daily, diagnostic_folder="diagnostics"){

  dir.create(diagnostic_folder, showWarnings = FALSE)

  if(!is.null(diagnostic_folder)){
    plt <- co2_daily %>%
      filter(year(date)>=2010) %>%
      mutate(across(c(fuel_type, sector), tolower)) %>%
      group_by(sector, fuel_type) %>%
      mutate(CO2_30d = zoo::rollapplyr(CO2_hybrid, 30, mean, fill=NA),
             year=as.factor(year(date)), plotdate=date %>% 'year<-'(2022)) %>%
      ggplot(aes(plotdate, CO2_30d/1e6, col=year)) +
      geom_line(size=0.2) +
      facet_wrap(~paste(fuel_type, sector), scales='free_y') +
      expand_limits(y=0)  + scale_x_date(expand=c(0,0)) +
      theme_crea() +
      labs(title="EU CO2 emissions", y='Mt/day, 30-day mean', x='')

    ggsave(file.path(diagnostic_folder,'EU CO2 emissions.png'), plot=plt, width=8, height=6, bg='white')
  }

  diagnostic_co2_benchmark_yearly(co2_daily, diagnostic_folder)
  diagnostic_co2_benchmark_monthly(co2_daily, diagnostic_folder)
}

diagnostic_co2_benchmark_yearly <- function(co2_daily, diagnostic_folder="diagnostics"){

  co2_crea <- co2_daily %>%
    filter(sector=='all',
           fuel_type=='total') %>%
    group_by(iso2, year=year(date)) %>%
    summarise(value=sum(CO2_emissions)/1e6,
              unit="mt",
              source='CREA') %>%
    filter(year < 2024) %>%
    ungroup()

  read_benchmark <- function(path){
    # Read and convert all columns that are not in c(iso, `Country/Region`, unit) to numeric
    read_csv(path) %>%
      mutate_at(vars(-c(iso, `Country/Region`, unit)), as.numeric)
  }

  co2_validate <-
    bind_rows(
      read_benchmark('data/ghg-emissions-climatewatch-withindustry.csv') %>% mutate(source='Climate Watch'),
      read_benchmark('data/ghg-emissions-unfccc-withindustry.csv') %>% mutate(source='UNFCCC'),
      read_benchmark('data/ghg-emissions-gcp.csv') %>% mutate(source='GCP'),
      read_benchmark('data/ghg-emissions-pik.csv') %>% mutate(source='PIK')
    ) %>%
    mutate(iso2=countrycode::countrycode(iso, "iso3c", "iso2c",
                                         custom_match=c("EUU"="EU"))) %>%
    filter(iso2 %in% unique(co2_crea$iso2)) %>%
    tidyr::gather(key="year", value="value",
                  -c(iso2, iso, `Country/Region`, unit, source)) %>%
    mutate(year=as.numeric(year)) %>%
    mutate(value=as.numeric(value)) %>%
    filter(!is.na(value))

  # co2_projected <- co2_crea %>%
  #   filter(year %in% seq(2021, 2023)) %>%
  #   arrange(year) %>%
  #   mutate(ratio = value/value[year==2021]) %>%
  #   select(iso2, year, ratio) %>%
  #   cross_join(
  #     co2_validate %>%
  #       filter(year==2021) %>%
  #       select(-c(year))) %>%
  #   mutate(value=value * ratio)

  (bind_rows(
    co2_crea %>% filter(year >= 1990) %>% mutate(type='estimated'),
    co2_validate %>% mutate(type='estimated'),
    # co2_projected %>% mutate(type='projected')
  ) %>%
      write_csv(file.path(diagnostic_folder, "co2_benchmark.csv")) %>%
      # filter(type=="estimated") %>%
      mutate(
        source=factor(source, levels=c("CREA", unique(co2_validate$source)))
      ) %>%
      # filter(source=='UNFCCC') %>%
      # filter(year >= 1990) %>%
      ggplot() +
      geom_line(aes(year, value/1e3, col=source,
                    linewidth=source,
                    alpha=source,
                    # linetype=type
                    )) +
      scale_x_continuous(limits=c(min(co2_crea$year), NA)) +
      scale_alpha_manual(values=c(0.9, 1, 1, 1, 1)) +
      scale_linewidth_manual(values=c(1.6, 0.5, 0.5, 0.5, 0.5)) +
      scale_color_manual(values=unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red")])) +
      rcrea::theme_crea() +
      rcrea::scale_y_crea_zero() +
      facet_wrap(~iso2, scales='free_y') +
      labs(title="CO2 emissions from fossil fuels",
           subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
           y=NULL,
           x=NULL,
           linewidth="Source",
           linetype=NULL,
           alpha="Source",
           color="Source",
           caption="Source: CREA analysis based on Climate Watch data. Cement, agriculture and LULUCF are not included in this comparison.") -> plt)

  plt
  if(!is.null(diagnostic_folder)){
    quicksave(file.path(diagnostic_folder, "co2_benchmark.jpg"), plot=plt)
  }
}

diagnostic_co2_benchmark_monthly <- function(co2_daily, diagnostic_folder="diagnostics"){

  co2_crea <- co2_daily %>%
    filter(fuel_type!='total') %>%
    group_by(iso2, geo, month=floor_date(date, 'month')) %>%
    summarise(value=sum(CO2_emissions)/1e6/(as.integer(difftime(max(date),min(date)))+1),
              unit="mt/day",
              source='CREA') %>%
    ungroup() %>%
    rename(date=month)

  #https://s3-eu-west-1.amazonaws.com/pfigshare-u-files/40572224/CM_EU.csv?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIYCQYOYV5JSSROOA/20240216/eu-west-1/s3/aws4_request&X-Amz-Date=20240216T035423Z&X-Amz-Expires=10&X-Amz-SignedHeaders=host&X-Amz-Signature=a3cdf29e62dc68f5d346797df01cd31613f927eff526d969bf94fbb5997d4738
  # co2_carbonmonitor <-
  co2_carbonmonitor <-
    bind_rows(
      readxl::read_xlsx('data/carbon-monitor-EU.xlsx') %>%
        rename(value=`MtCO2 per day`),
      read_csv('data/CM_EU.csv')) %>%
    distinct(country, date, sector, .keep_all = T)

  co2_validate <- bind_rows(
    co2_carbonmonitor %>%
      rename(geo=country) %>%
      add_iso2() %>%
      rename(country=geo) %>%
      filter(iso2 %in% get_eu_iso2s()),
    co2_carbonmonitor %>%
      filter(country %in% c("EU27 & UK", "United Kingdom")) %>%
      group_by(date, sector) %>%
      summarise(value=sum(value * case_when(country=="United Kingdom" ~ -1, TRUE ~ 1)),
                country="EU27",
                iso2="EU")) %>%
    mutate(unit='Mt',
           source='Carbon Monitor',
           date=as.Date(lubridate::fast_strptime(date, "%d/%m/%Y"))) %>%
    # rename(value=`MtCO2 per day`) %>%
    group_by(iso2, date=floor_date(date, 'month'), source, sector) %>%
    summarise(value=sum(value/lubridate::days_in_month(date)),
              unit='mt/day')%>%
    ungroup()


    ggplot(data=NULL, aes(date, value)) +
    geom_area(data=co2_validate %>%
                filter(date %in% co2_crea$date,
                       iso2 %in% co2_crea$iso2,
                      !grepl('Aviation', sector)
                      ),
              aes(fill=sector),
              alpha=0.5) +
      geom_line(data=co2_validate %>%
                  filter(date %in% co2_crea$date,
                         iso2 %in% co2_crea$iso2,
                         !grepl('Aviation', sector)
                  ) %>%
                  group_by(date, iso2, source) %>%
                  summarise(value=sum(value)),
                aes(col=source)) +
    geom_line(data=co2_crea  %>%
                filter(date >= min(co2_validate$date)),
              aes(col=source)) +
    facet_grid(iso2~., scales='free_y') +
      rcrea::theme_crea() +
      rcrea::scale_y_crea_zero() +
      labs(title="EU CO2 emissions from fossil fuels",
           subtitle="Comparison with Carbon Monitor",
           y="mt/day",
           x=NULL,
           alpha="Source",
           color="Source",
           fill="Sector",
           caption="Source: CREA analysis.") -> plt

plt
if(!is.null(diagnostic_folder)){
  quicksave(file.path(diagnostic_folder, "co2_benchmark_carbonmonitor.jpg"), plot=plt,
            height=max(4, 1.5*length(unique(co2_crea$iso2))))
}
}
