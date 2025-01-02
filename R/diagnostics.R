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
      cons_yearly %>% mutate(source='yearly'),
      cons_monthly %>% mutate(source='monthly')) %>%

        filter(grepl('27', geo, T)) %>%
        recode_siec() %>%
        filter(siec %in% .[.$source=='monthly',]$siec) %>%
        group_by(year=lubridate::year(time), siec, source, fuel, sector) %>%
        summarise(values=sum(values)) %>%
        ggplot(aes(year, values, col=siec, linetype=source)) +
        geom_line() +
        facet_grid(fuel~sector, scales='free_y') +
        theme(legend.position = 'bottom') +
        rcrea::scale_y_crea_zero())

    ggsave(file.path(diagnostics_folder,'eurostat_annual_vs_monthly_yearly.png'), plot=plt, width=12, height=6, bg='white')

    (plt <- cons_combined %>%
      filter(grepl('European union', geo, T)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero())

    ggsave(file.path(diagnostics_folder,'eurostat_annual_vs_monthly_monthly.png'), plot=plt, width=8, height=6, bg='white')

    (plt <- cons_combined %>%
      filter(grepl('European union', geo, T)) %>%
      # Complete dates so that there's no weird line in the plot
      complete(time=seq.Date(min(time), max(time), by='month'),
               geo,
               siec,
               fuel,
               sector,
               source,
               fill=list(values=NA)) %>%
      ggplot(aes(time, values, col=siec, linetype=source)) +
      geom_line() +
      facet_grid(fuel~sector, scales='free_y') +
      theme(legend.position='bottom')+
      rcrea::scale_y_crea_zero())

    ggsave(file.path(diagnostics_folder,'eurostat_combined.png'), plot=plt, width=8, height=6, bg='white')
  }

}

diagnostic_eurostat_cons <- function(eurostat_cons, iso2s, diagnostics_folder="diagnostics"){

  # Plot heatmap of consumption by sector
  if(!is.null(diagnostics_folder)){

    dir.create(diagnostics_folder, showWarnings = FALSE)
    plt_data <- eurostat_cons %>%
      filter(is.null(iso2s) | iso2 %in% iso2s) %>%
      group_by(iso2, geo, fuel, sector, unit, time) %>%
      summarise(values=sum(values)) %>%
      # Complete dates
      ungroup() %>%
      complete(time=seq.Date(min(.$time), max(.$time), by='month'),
               nesting(iso2,geo),
               nesting(fuel, unit, sector),
               fill=list(values=NA))

    (ggplot(plt_data, aes(time, values, col=sector)) +
      geom_line() +
       rcrea::scale_y_crea_zero() +
       ggh4x::facet_grid2(geo ~ glue("{fuel}\n({unit})"), scales = "free_y", independent = "y") +
      # facet_wrap(geo ~ glue("{fuel} ({unit})"), scales='free_y')) +
        scale_x_date(date_minor_breaks = "1 year") +
      rcrea::theme_crea() +
        theme(panel.grid.minor.x = element_line(color="grey95"),
              panel.grid.major.x = element_line(color="grey90"),
              ) -> plt)

    ggsave(file.path(diagnostics_folder, 'eurostat_cons.png'),
           width=10, height=min(30, max(6, 1.5*n_distinct(plt_data$iso2))), bg='white', plot=plt, scale=1.5)



    # Check data availability
    (eurostat_cons %>%
        group_by(iso2, sector, fuel) %>%
        summarise(max_date=max(time)) %>%
        ggplot(aes(max_date, iso2)) +
        geom_bar(stat='identity', aes(fill=iso2=="EU")) +
        geom_text(
          data=function(x) filter(x, iso2=="EU"),
          aes(label=max_date),
          size=3,
        ) +
        scale_x_date(limits=c(as.Date("2020-01-01"), NA), oob = scales::squish) +
        facet_wrap(fuel~sector)) -> plt

    plt
    quicksave(file.path(diagnostics_folder, 'eurostat_data_availability.png'),
           width=10, height=8, bg='white', plot=plt, scale=1.5)

  }
}

