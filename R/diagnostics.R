diagnostic_pwr <- function(pwr_demand, diagnostics_folder="diagnostics"){

  #plot by source
  if(!is.null(diagnostics_folder)){
    dir.create(diagnostics_folder, showWarnings = FALSE)

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
      summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean),
                .groups="drop")

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

    ggsave(file.path(diagnostics_folder, 'EU power generation by source.png'),
           width=8, height=6, bg='white', plot=plt)
  }
}


diagnostic_eurostat_cons_yearly_monthly <- function(cons_yearly, cons_monthly, cons_combined, diagnostics_folder="diagnostics"){

  if(!is.null(diagnostics_folder)){
    (plt <- bind_rows(
      do.call(bind_rows, cons_yearly) %>% mutate(source='yearly'),
      do.call(bind_rows, cons_monthly) %>% mutate(source='monthly')) %>%

        filter(grepl('27', geo, T)) %>%
        recode_siec() %>%
        filter(siec %in% .[.$source=='monthly',]$siec) %>%
        group_by(year=lubridate::year(time), siec, source, fuel_type, sector) %>%
        summarise(values=sum(values)) %>%
        ggplot(aes(year, values, col=siec, linetype=source)) +
        geom_line() +
        facet_grid(fuel_type~sector, scales='free_y') +
        theme(legend.position = 'bottom') +
        rcrea::scale_y_crea_zero())

    ggsave(file.path(diagnostics_folder,'eurostat_annual_vs_monthly_yearly.png'), plot=plt, width=12, height=6, bg='white')

    plt <- cons_combined %>%
      filter(grepl('European union', geo, T)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel_type~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero()

    ggsave(file.path(diagnostics_folder,'eurostat_annual_vs_monthly_monthly.png'), plot=plt, width=8, height=6, bg='white')

    plt <- cons_combined %>%
      group_by(geo, sector, time, unit, siec, fuel_type) %>%
      arrange(source) %>%
      slice(1) %>%
      ungroup() %>%
      filter(grepl('European union', geo, T)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel_type~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero()

    ggsave(file.path(diagnostics_folder,'eurostat_combined.png'), plot=plt, width=8, height=6, bg='white')
  }

}

diagnostic_eurostat_cons <- function(eurostat_cons, iso2s, diagnostics_folder="diagnostics"){

  # Plot heatmap of consumption by sector
  if(!is.null(diagnostics_folder)){

    dir.create(diagnostics_folder, showWarnings = FALSE)

    (plt <- eurostat_cons %>%
       filter(iso2 %in% iso2s) %>%
       group_by(geo, fuel_type, sector, unit, time) %>%
       summarise(values=sum(values)) %>%
      ggplot(aes(time, values, col=sector)) +
      geom_line() +
       rcrea::scale_y_crea_zero() +
       ggh4x::facet_grid2(geo ~ glue("{fuel_type}\n({unit})"), scales = "free_y", independent = "y") +
      # facet_wrap(geo ~ glue("{fuel_type} ({unit})"), scales='free_y')) +
        scale_x_date(date_minor_breaks = "1 year") +
      rcrea::theme_crea() +
        theme(panel.grid.minor.x = element_line(color="grey95"),
              panel.grid.major.x = element_line(color="grey90"),
              ))

    ggsave(file.path(diagnostics_folder, 'eurostat_cons.png'),
           width=10, height=min(30,max(4, 1.5*length(iso2s))), bg='white', plot=plt, scale=1.5)
  }
}

diagnostic_co2 <- function(co2_daily, diagnostics_folder="diagnostics"){
  diagnostic_co2_simple(co2_daily, diagnostics_folder)
  diagnostic_co2_benchmark_yearly(co2_daily, diagnostics_folder)
  diagnostic_co2_benchmark_monthly(co2_daily, diagnostics_folder)
  diagnostic_co2_versions(diagnostics_folder=diagnostics_folder)
}


diagnostic_co2_simple <- function(co2_daily, diagnostics_folder="diagnostics"){

  if(!is.null(diagnostics_folder)){
    dir.create(diagnostics_folder, showWarnings = FALSE)
    (plt <- co2_daily %>%
       mutate(across(c(fuel_type, sector), tolower)) %>%
       group_by(sector, fuel_type) %>%
       mutate(CO2_30d = zoo::rollapplyr(value_co2_tonne, 30, mean, fill=NA),
              year=as.factor(year(date)), plotdate=date %>% 'year<-'(2022)) %>%
       filter(year(date)>=2010) %>%
       ggplot(aes(plotdate, CO2_30d/1e6, col=year)) +
       geom_line(size=0.2) +
       facet_wrap(iso2 ~paste(fuel_type, sector), scales='free_y') +
       expand_limits(y=0)  + scale_x_date(expand=c(0,0)) +
       theme_crea() +
       rcrea::scale_y_crea_zero() +
       labs(title="EU CO2 emissions", y='Mt/day, 30-day mean', x=''))

    ggsave(file.path(diagnostics_folder,'EU CO2 emissions.png'), plot=plt, width=8, height=6, bg='white', scale=1)
  }
}

diagnostic_co2_benchmark_yearly <- function(co2_daily, diagnostics_folder="diagnostics"){

  if(!is.null(diagnostics_folder)){
    co2_crea <- co2_daily %>%
      filter(sector=='all',
             fuel_type=='total') %>%
      group_by(iso2, year=year(date)) %>%
      summarise(value=sum(value_co2_tonne)/1e6,
                unit="mt",
                source='CREA CO2 tracker') %>%
      filter(year < 2024) %>%
      ungroup()

    read_benchmark <- function(path){
      # Read and convert all columns that are not in c(iso, `Country/Region`, unit) to numeric
      suppressWarnings(read_csv(path, col_types = cols()) %>%
        filter(grepl('co2', unit, ignore.case=T)) %>%
        mutate_at(vars(-c(iso, `Country/Region`, unit)), as.numeric)) %>%
        mutate(iso2=countrycode::countrycode(iso, "iso3c", "iso2c",
                                             custom_match=c("EUU"="EU"))) %>%
        select(-c(iso)) %>%
        tidyr::gather(key="year", value="value",
                      -c(iso2, `Country/Region`, unit)) %>%
        mutate(year=as.numeric(year)) %>%
        mutate(value=as.numeric(value)) %>%
        filter(!is.na(value))
    }

    get_primap <- function(iso2s, with_mineral=F){

      filepath <- 'data/Guetschow_et_al_2023b-PRIMAP-hist_v2.5_final_15-Oct-2023.csv'
      url <- "https://zenodo.org/records/10006301/files/Guetschow_et_al_2023b-PRIMAP-hist_v2.5_final_15-Oct-2023.csv?download=1"
      if(!file.exists(filepath)){
          dir.create(dirname(filepath), showWarnings = FALSE)
          download.file(url, filepath)
      }

      x <- read_csv(filepath) %>%
        mutate(iso2 = countrycode(`area (ISO3)`, "iso3c", "iso2c", custom_match = c("EU27BX"="EU"))) %>%
        rename(category=`category (IPCC2006_PRIMAP)`,
               scenario=`scenario (PRIMAP-hist)`) %>%
        filter(
          entity=="CO2",
          iso2 %in% iso2s,
          scenario=='HISTCR'
          ) %>%
        filter(category %in% c(1, 2, "2.A")) %>%
        # select all columns that are have number names
        select(matches("iso2|category|\\d+$")) %>%
        tidyr::gather(key = "year", value = "value", -c(iso2, category)) %>%
        mutate(year=as.numeric(year),
               value=value/1e3)

      if(with_mineral){
        x %>%
          group_by(iso2, year) %>%
          filter(category %in% c(1,2)) %>%
          summarise(value=sum(value), .groups = "drop")
      }else{
        x %>%
          group_by(iso2, year) %>%
          summarise(value=sum(value * case_when(category=="2.A"~-1, T ~1)), .groups="drop")
      }
    }

    co2_validate <-
      # Regenerate this data here:
      # https://shorturl.at/anQR1
      bind_rows(
        # read_benchmark('data/ghg-emissions-climatewatch-withindustry.csv') %>% mutate(source='Climate Watch (with industry)'),
        # read_benchmark('data/ghg-emissions-climatewatch.csv') %>% mutate(source='Climate Watch'),
        # read_benchmark('data/ghg-emissions-unfccc-withindustry.csv') %>% mutate(source='UNFCCC (with industry)'),
        # read_benchmark('data/ghg-emissions-unfccc.csv') %>% mutate(source='UNFCCC'),
        # read_benchmark('data/ghg-emissions-gcp.csv') %>% mutate(source='GCP'),
        # read_benchmark('data/ghg-emissions-pik-withindustry.csv') %>% mutate(source='Old PRIMAP-hist v2.5 (Energy and Industry)'),
        # read_benchmark('data/ghg-emissions-pik.csv') %>% mutate(source='PRIMAP-hist v2.5 (Energy alone)')
        get_primap(iso2s=unique(co2_crea$iso2), with_mineral = T) %>% mutate(source='PRIMAP Energy and Industry'),
        get_primap(iso2s=unique(co2_crea$iso2), with_mineral = F) %>% mutate(source='PRIMAP Energy and Industry\n(excl. Mineral industry)')
      ) %>%
      filter(iso2 %in% unique(co2_crea$iso2))


    # Order factor by dates for chart readability
    levels_source <- co2_validate %>%
      filter(year==2020,
             iso2=="EU") %>%
      arrange(-value) %>%
      pull(source)

    (bind_rows(
      co2_crea %>% filter(year >= 1990),
      co2_validate,
      # co2_projected %>% mutate(type='projected')
    ) %>%
        write_csv(file.path(diagnostics_folder, "co2_benchmark.csv")) %>%
        # filter(type=="estimated") %>%
        mutate(
          # source=factor(source, levels=c("CREA", unique(co2_validate$source)))
          source=factor(source, levels=c("CREA CO2 tracker", levels_source))
        ) %>%
        # filter(source=='UNFCCC') %>%
        # filter(year >= 1990) %>%
        ggplot() +
        geom_line(aes(year, value/1e3, col=source,
                      linewidth=source,
                      alpha=source,
                      # linetype=grepl('industry', source)
                      ),
                  position=position_dodge(width=0.1)
                  ) +
        scale_x_continuous(limits=c(min(co2_crea$year), NA)) +
        scale_alpha_manual(values=c(0.9, 1, 1, 1, 1, 1, 1)) +
        scale_linewidth_manual(values=c(1.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        # scale_linetype_manual(values=c('solid', 'dashed')) +
        scale_color_manual(values=unname(rcrea::pal_crea[c("Blue", "Dark.red", "Orange", "Dark.blue", "Purple", "Green", "Red")])) +
        rcrea::theme_crea() +
        # Legend on the top, left align
        # theme(legend.position = "top",
        #       legend.title = element_blank(),
        #       # Left align
        #
        #       ) +

        rcrea::scale_y_crea_zero() +
        ifelse(length(unique(co2_crea$iso2))>1, facet_wrap(~iso2, scales='free_y'), element_blank()) +
        labs(title="CO2 emissions from fossil fuels",
             subtitle="Billion tonne CO2 per year",
             y=NULL,
             x=NULL,
             linewidth="Source",
             linetype="Source",
             alpha="Source",
             color="Source",
             caption="Note: PRIMAP refers to PRIMAP-hist v2.5 and includes non-fossil fuels related emissions.") -> plt)

    plt

    quicksave(file.path(diagnostics_folder, "co2_benchmark.jpg"), plot=plt, width=8, height=4, scale=1, logo=F, dpi=600)
  }
}


diagnostic_co2_benchmark_monthly <- function(co2_daily, diagnostics_folder="diagnostics"){

  #TODO add diagnostics data to package
  if(!is.null(diagnostics_folder)){

  co2_crea <- co2_daily %>%
    filter(fuel_type!='total') %>%
    group_by(iso2, geo, month=floor_date(date, 'month')) %>%
    summarise(value=sum(value_co2_tonne)/1e6/(as.integer(difftime(max(date),min(date)))+1),
              unit="mt/day",
              source='CREA CO2 Tracker',
              .groups="drop") %>%
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
              unit='mt/day',
              .groups="drop") %>%
    ungroup()


    ggplot(data=bind_rows(
      co2_validate %>%
             filter(date %in% co2_crea$date,
                    iso2 %in% co2_crea$iso2,
                    !grepl('Aviation', sector)
             ) %>%
             group_by(date, iso2, source) %>%
             summarise(value=sum(value),
                       .groups="drop"),
      co2_crea  %>%
        filter(date >= min(co2_validate$date))) %>%
        mutate(source=factor(source, levels=c("CREA CO2 Tracker", "Carbon Monitor"))),
               aes(date, value)) +
    # geom_area(data=co2_validate %>%
    #             filter(date %in% co2_crea$date,
    #                    iso2 %in% co2_crea$iso2,
    #                   !grepl('Aviation', sector)
    #                   ),
    #           aes(fill=sector),
    #           alpha=0.5) +
      geom_line(aes(col=source,
                    alpha=source,
                    linewidth=source)) +
      scale_alpha_manual(values=c(0.9, 1, 1, 1, 1, 1, 1)) +
      scale_linewidth_manual(values=c(1.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)) +
      # scale_linetype_manual(values=c('solid', 'dashed')) +
      scale_color_manual(values=unname(rcrea::pal_crea[c("Blue", "Dark.red", "Orange", "Dark.blue", "Purple", "Green", "Red")])) +
    # geom_line(aes(col=source)) +
    #   theme(legend.position = "top",
    #         legend.title = element_blank(),
    #         # Left align
    #
    #   ) +

    # facet_grid(iso2~., scales='free_y') +
      rcrea::theme_crea() +
      # theme(legend.position = "top",
      #       legend.title = element_blank(),
      #       # Left align
      #
      # ) +
      rcrea::scale_y_crea_zero() +
      labs(title="EU CO2 emissions from fossil fuels",
           subtitle="Million tonne CO2 per day",
           y=NULL,
           x=NULL,
           alpha="",
           color="",
           linewidth="",
           fill="Sector",
           caption="Source: CREA analysis.") -> plt

plt

  quicksave(file.path(diagnostics_folder, "co2_benchmark_carbonmonitor.jpg"), plot=plt,
            width=8, height=4, scale=1, logo=F, dpi=600)
            # height=min(30,max(4, 1.5*length(unique(co2_crea$iso2)))),

}
}


#' Compare different versions of our tracker
#'
#' @param iso2s
#' @param versions
#'
#' @return
#' @export
#'
#' @examples
diagnostic_co2_versions <- function(iso2s="EU", versions=c("0.2", "0.3"), diagnostics_folder="diagnostics"){

  # Read the data
  co2 <- lapply(versions, function(version) download_co2_daily(iso2s=iso2s, version=version)) %>%
    bind_rows()

  co2 %>%
    distinct(version, region, fuel)

  # Plot yoy trends
  (co2 %>%
    filter(fuel=="Total") %>%
    group_by(region, year=year(date), version) %>%
    summarise(value=sum(value, na.rm=T), .groups="drop") %>%
    group_by(region, version) %>%
    arrange(year) %>%
    filter(year < 2024) %>%
    mutate(yoy = value / lag(value) - 1) %>%
      filter(!is.na(yoy)) %>%
    ungroup() %>%
    ggplot(aes(year, yoy, fill=factor(version))) +
    geom_col(position="dodge") +
    geom_hline(yintercept=0) +
    geom_text(aes(
      label=paste0(ifelse(yoy>0,"+",""),scales::percent(yoy, accuracy=0.1)),
      vjust=ifelse(yoy>0, -0.9, 1.9)
      ),
              position=position_dodge(width=0.9),
      size=2.5
      ) +
    ifelse(length(unique(co2$region))>1, facet_wrap(~region, scales='free_y'), element_blank()) +
    rcrea::theme_crea() +
    scale_fill_crea_d() +
    scale_y_continuous(labels=scales::percent_format(accuracy=0.1),
                       expand = expansion(mult=0.1)) +
    labs(title="Comparison with previous version",
    subtitle="Year-on-year changes of EU CO2 emissions",
    y=NULL, x=NULL, fill="Version") -> plt)

  if(!is.null(diagnostics_folder)){
    quicksave(file.path(diagnostics_folder, "co2_comparison_versions.jpg"), plot=plt,
              width=8, height=4, scale=1, logo=F, dpi=600)
    # height=min(30,max(4, 1.5*length(unique(co2_crea$iso2)))),

  }

  (co2 %>%
      # filter(fuel!="Total") %>%
      group_by(region, year=year(date), version, fuel) %>%
      summarise(value=sum(value, na.rm=T), .groups="drop") %>%
      group_by(region, fuel, version) %>%
      arrange(year) %>%
      filter(year < 2024) %>%
      mutate(yoy = value / lag(value) - 1) %>%
      filter(!is.na(yoy)) %>%
      ungroup() %>%
      ggplot(aes(year, yoy, fill=factor(version))) +
      geom_col(position="dodge") +
      geom_hline(yintercept=0) +
      geom_text(aes(
        label=paste0(ifelse(yoy>0,"+",""),scales::percent(yoy, accuracy=0.1)),
        vjust=ifelse(yoy>0, -0.9, 1.9)
      ),
      position=position_dodge(width=0.9),
      size=2.5
      ) +
      ifelse(length(unique(co2$region))>1, facet_wrap(~region, scales='free_y'), element_blank()) +
      rcrea::theme_crea() +
      scale_fill_crea_d() +
      scale_y_continuous(labels=scales::percent_format(accuracy=0.1),
                         expand = expansion(mult=0.1)) +
      facet_grid(factor(fuel, levels=c("Total", "Coal", "Oil", "Gas")) ~.) +
      labs(title="Comparison with previous version",
           subtitle="Year-on-year changes of EU CO2 emissions",
           y=NULL, x=NULL, fill="Version") -> plt)

  if(!is.null(diagnostics_folder)){
    quicksave(file.path(diagnostics_folder, "co2_comparison_versions_by_fuel.jpg"), plot=plt,
              width=8, height=7, scale=1, logo=F)
    # height=min(30,max(4, 1.5*length(unique(co2_crea$iso2)))),

  }

}
