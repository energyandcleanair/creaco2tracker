diagnostic_pwr <- function(pwr_generation, diagnostics_folder="diagnostics"){

  #plot by source
  if(!is_null_or_empty(diagnostics_folder)){
    create_dir(diagnostics_folder)

    #add rolling mean
    plt_data <- pwr_generation %>%
      filter(iso2=="EU") %>%
      group_by(region, country, source) %>%
      arrange(date) %>%
      mutate(plotdate = date %>% 'year<-'(2022),
             year=year(date),
             output_mw_rollmean=zoo::rollapplyr(value_mw, 7, mean, fill=NA))

    #output range of values for several years
    plt_range <- plt_data %>% filter(year %in% 2015:2022) %>%
      group_by(region, country, source, plotdate) %>%
      summarise(min=min(output_mw_rollmean), max=max(output_mw_rollmean),
                .groups="drop")

    plt <- plt_data %>%
      filter(date<max(date)-3, year %in% 2023:2024, country=='EU total') %>%
      group_by(country) %>%
      filter(mean(value_mw, na.rm=T)>1e3) %>%
      ggplot(aes(plotdate)) +
      facet_wrap(~source, scales='free_y') +
      geom_ribbon(data=plt_range %>% filter(country=='EU total'),
                  aes(ymin=min/1000, ymax=max/1000), fill=crea_palettes$CREA[2],
                  alpha=0.5) +
      geom_line(aes(y=output_mw_rollmean/1000, col=as.factor(year)), linewidth=0.5) +
      expand_limits(y=0) +
      # scale_x_datetime(date_labels = '%b') +
      labs(title='EU power generation by source',
           subtitle='GW, 7-day mean',
           y='',
           x='', col='', fill='') +
      rcrea::theme_crea_new() +
      rcrea::scale_color_crea_d('dramatic', guide=guide_legend(nrow = 1)) +
      rcrea::scale_fill_crea_d(col.index = 2)

    plt

    ggsave(file.path(diagnostics_folder, 'EU power generation by source.png'),
           width=10, height=8, bg='white', plot=plt)
  }
}


diagnostic_eurostat_cons_yearly_monthly <- function(cons_yearly,
                                                    cons_monthly,
                                                    cons_combined,
                                                    detailed_iso2s=c("EU"),
                                                    diagnostics_folder="diagnostics"){

  if(!is_null_or_empty(diagnostics_folder)){

    create_dir(diagnostics_folder)

    plt_data <- bind_rows(
      cons_yearly %>% mutate(source='yearly'),
      cons_monthly %>% mutate(source='monthly')) %>%
        add_iso2() %>%
        # filter(grepl('27', geo, T)) %>%
        recode_siec() %>%
        filter(siec %in% .[.$source=='monthly',]$siec) %>%
        group_by(iso2, year=lubridate::year(time), siec, source, fuel, sector) %>%
        summarise(values=sum(values)) %>%
        # Scale by iso2, siec for chart to be readable
        group_by(iso2, siec) %>%
        mutate(values=values/max(values)) %>%
        ungroup()

    plt <- plt_data %>%
        ggplot(aes(year, values, col=sector, linetype=source)) +
        geom_line() +
        facet_grid(iso2_to_name(iso2) ~ siec,
                   scales='free_y') +
        theme(legend.position = 'bottom') +
        rcrea::scale_y_crea_zero()

    ggsave(filename=file.path(diagnostics_folder,'eurostat_annual_vs_monthly_yearly.png'),
           plot=plt,
           width=12,
           height=18,
           bg='white',
           scale=1.5
           )

    # Do detailed ones
    for(iso2 in detailed_iso2s){

      plt <- plt_data %>%
        filter(iso2 == !!iso2) %>%
        ggplot(aes(year, values, col=sector, linetype=source)) +
        geom_line() +
        facet_wrap(~ siec,
                   scales='free_y') +
        theme(legend.position = 'bottom') +
        rcrea::scale_y_crea_zero()

      ggsave(filename=file.path(diagnostics_folder, glue('eurostat_annual_vs_monthly_yearly_{tolower(iso2)}.png')),
             plot=plt,
             width=10,
             height=8,
             bg='white'
      )

      (plt <- cons_combined %>%
          filter(iso2 == !!iso2) %>%
          ggplot(aes(time, values, col=siec, linetype=source)) +
          geom_line() +
          facet_grid(fuel~sector, scales='free_y') +
          theme(legend.position='bottom')+
          rcrea::scale_y_crea_zero())

      ggsave(filename=file.path(diagnostics_folder, glue('eurostat_annual_vs_monthly_monthly_{tolower(iso2)}.png')),
             plot=plt,
             width=8,
             height=6,
             bg='white')

      (plt <- cons_combined %>%
          filter(iso2 == !!iso2) %>%
          # Complete dates so that there's no weird line in the plot
          complete(time=seq.Date(min(time), max(time), by='month'),
                   iso2,
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

      ggsave(filename=file.path(diagnostics_folder, glue('eurostat_combined_{tolower(iso2)}.png')),
             plot=plt,
             width=8,
             height=6,
             bg='white')
    }
  }
}

diagnostic_eurostat_cons <- function(eurostat_cons, iso2s=NULL, diagnostics_folder="diagnostics"){

  # Plot heatmap of consumption by sector
  if(!is_null_or_empty(diagnostics_folder)){

    create_dir(diagnostics_folder)
    plt_data <- eurostat_cons %>%
      mutate(geo=countrycode::countrycode(iso2, "iso2c", "country.name", custom_match = c("EU"="EU",
                                                                                          "XK"="Kosovo"))) %>%
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

    ggsave(filename=file.path(diagnostics_folder, 'eurostat_cons.png'),
           width=10,
           height=min(30, max(6, 1.5*n_distinct(plt_data$iso2))),
           bg='white',
           plot=plt,
           scale=1.5)



    # Check data availability
    (eurostat_cons %>%
        filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
        group_by(iso2, sector, fuel) %>%
        summarise(max_date=max(time)) %>%
        ggplot(aes(max_date, iso2)) +
        geom_bar(stat='identity', aes(fill=iso2=="EU"), show.legend = F) +
        geom_text(
          data=function(x) filter(x, iso2=="EU"),
          aes(label=max_date),
          size=3,
        ) +
        rcrea::scale_fill_crea_d() +
        rcrea::theme_crea_new() +
        labs(
          title='[DIAGNOSTIC] Eurostat fossil-fuel consumption data availability',
          x=NULL,
          y=NULL
        ) +
        scale_x_date(limits=c(as.Date("2020-01-01"), NA), oob = scales::squish) +
        facet_wrap(fuel~sector)) -> plt

    plt
    quicksave(file=file.path(diagnostics_folder, 'eurostat_cons_availability.png'),
           width=10,
           height=8,
           bg='white',
           plot=plt,
           scale=1.5)

  }
}

diagnostic_eurostat_indprod <- function(eurostat_indprod, iso2s, diagnostics_folder="diagnostics"){

  # Plot heatmap of consumption by sector
  if(!is_null_or_empty(diagnostics_folder)){

    create_dir(diagnostics_folder)

    # Check data availability
    (eurostat_indprod %>%
        filter(
          nace_r2_code == "B" |
            (stringr::str_length(nace_r2_code) == 3 & substr(nace_r2_code, 1, 1) == "C")
        ) %>%
        filter(
          unit=="Index, 2021=100",
          grepl("Calendar adjusted data", s_adj)
        ) %>%
        filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
        group_by(iso2, nace_r2_code) %>%
        summarise(max_date=max(time)) %>%
        ggplot(aes(max_date, iso2)) +
        geom_bar(stat='identity', aes(fill=iso2=="EU"), show.legend = F) +
        geom_text(
          data=function(x) filter(x, iso2=="EU"),
          aes(label=max_date),
          size=3,
        ) +
        rcrea::scale_fill_crea_d() +
        rcrea::theme_crea_new() +
        labs(
          title='[DIAGNOSTIC] Eurostat industrial production data availability',
          x=NULL,
          y=NULL
        ) +
        scale_x_date(limits=c(as.Date("2020-01-01"), NA), oob = scales::squish) +
        facet_wrap(~nace_r2_code)) -> plt

    plt
    quicksave(file.path(diagnostics_folder, 'eurostat_indprod_availability.png'),
           width=10, height=8, bg='white', plot=plt, scale=1.5)

  }
}

#' How close are EU estimates to the sum of EU countries
#'
#' @param co2
#' @param diagnostics_folder
#'
#' @return
#' @export
#'
#' @examples
diagnose_eu_vs_countries <- function(
  co2_unprojected,
  co2,
  eurostat_cons,
  pwr_generation,
  diagnostics_folder="diagnostics"){

  if(!is_null_or_empty(diagnostics_folder)){
    create_dir(diagnostics_folder)

    # Power demand --------------------------------------------------
    plt_data <- pwr_generation %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
      mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
      filter(date < "2025-01-01") %>%
      filter(date >= "2015-01-01") %>%
      group_by(
        source,
        year=year(date),
        is_eu) %>%
      summarise(value=sum(value_mwh, na.rm=T)/1e6) %>%
      arrange(desc(year))


    plt_data %>%
      group_by(source) %>%
      ggplot(aes(year, value, col=is_eu)) +
      geom_line(show.legend = F) +
      ggrepel::geom_text_repel(
        data = . %>% filter(year==max(year)),
        aes(label=is_eu), nudge_x = 0.5, nudge_y = 0.5,
        show.legend = F,
        # hide segment
        segment.color = NA,
        # vertically aligned
        direction = 'x',
        hjust = 0,
        # bold
        fontface = 'bold',

      ) +
      facet_wrap(~source, scales='free_y')  +
      scale_x_continuous(expand = expansion(add = c(0, 3)),
                         breaks = seq(2015, 2025, 1)
      ) +
      rcrea::scale_y_crea_zero() +
      rcrea::scale_color_crea_d() +
      rcrea::theme_crea_new() +
      labs(
        title='[DIAGNOSTIC] Power demand EU vs sum of EU countries',
        subtitle='Gap is theoretically NOT OK',
        y='TWh per year',
        x=''
      ) -> plt
    quicksave(file.path(diagnostics_folder, 'eu_vs_countries_power.png'),
              plot=plt,
              width=10, height=8, bg='white', scale=1.5, preview=F)



    # Eurostat --------------------------------------------------
    plt_data <- eurostat_cons %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
      mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
      filter(time < "2025-01-01") %>%
      filter(time >= "2015-01-01") %>%
      group_by(
        siec,
        sector,
        time,
        is_eu) %>%
      summarise(value=sum(values, na.rm=T),
                n_iso2=n_distinct(iso2)) %>%
      arrange(desc(time))

    plt_data %>%
      group_by(siec, sector, is_eu) %>%
      ggplot(aes(time, value, col=is_eu, linetype=is_eu)) +
      geom_line(show.legend = F) +
      ggrepel::geom_text_repel(
        data = . %>% filter(time==max(time)),
        aes(label=is_eu), nudge_x = 0.5, nudge_y = 0.5,
        show.legend = F,
        # hide segment
        segment.color = NA,
        # vertically aligned
        direction = 'x',
        hjust = 0,
        # bold
        fontface = 'bold',

      ) +
      facet_wrap(sector~siec, scales='free_y') +
      # scale_x_continuous(expand = expansion(add = c(0, 3)),
      #                    breaks = seq(2015, 2025, 1)
      # ) +
      rcrea::scale_color_crea_d() +
      rcrea::theme_crea_new() +
      labs(
        title='[DIAGNOSTIC] Eurostat data EU vs sum of EU countries',
        subtitle='BEFORE filling missing data - Gap is theoretically OK',
        y='Gt CO2 per year',
        x=''
      ) +
      rcrea::scale_y_crea_zero() -> plt


    quicksave(file.path(diagnostics_folder, 'eu_vs_countries_eurostat.png'),
              plot=plt,
              width=10, height=8, bg='white', scale=1.5, preview=F)




    # CO2 filled ---------------------------------------------------
    plt_data <- co2 %>%
      detotalise_co2() %>%
      filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
      mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
      filter(date < "2025-01-01") %>%
      filter(date >= "2015-01-01") %>%
      group_by(
        fuel,
        sector,
        date=floor_date(date, "month"),
        is_eu,
        estimate
        ) %>%
      summarise(value=sum(value, na.rm=T)/1e9,
                n_iso2=n_distinct(iso2)) %>%
      arrange(desc(date)) %>%
      filter(is_eu=="EU" | n_iso2==27) %>%
      # Remove the projected part
      spread(estimate, value) %>%
      mutate(is_projected=upper!=lower) %>%
      rename(value=central)

    # Add total
    plt_data <- bind_rows(
      plt_data,
      plt_data %>%
        group_by(is_eu, date) %>%
        summarise(value=sum(value, na.rm=T),
                  fuel="total",
                  sector="all",
                  is_projected=any(is_projected)
                  )
    )

    # Add point to have lines
    plt_data %>%
      group_by(sector, fuel, is_eu) %>%
      arrange(date) %>%
      filter(!is_projected & lead(is_projected)) %>%
      mutate(is_projected=T) %>%
      bind_rows(plt_data) -> plt_data



    # Check EU vs countries
    plt_data %>%
      group_by(fuel) %>%
      ggplot(aes(date, value, col=is_eu, linetype=is_projected)) +
      geom_line(show.legend = T) +
      # ggrepel::geom_text_repel(
      #   data = . %>% filter(date==max(date)),
      #   aes(label=is_eu),
      #   # nudge_x = 0.5, nudge_y = 0.5,
      #   show.legend = F,
      #   # hide segment
      #   segment.color = NA,
      #   # vertically aligned
      #   direction = 'x',
      #   hjust = 0,
      #   # bold
      #   fontface = 'bold',
      #
      #   ) +
      facet_wrap(sector~fuel) +
      # scale_x_continuous(expand = expansion(add = c(0, 3)),
      #                    breaks = seq(2015, 2025, 1)
      #                    ) +
      rcrea::scale_color_crea_d() +
      rcrea::theme_crea_new() +
      rcrea::scale_y_crea_zero() +
      labs(
        title='[DIAGNOSTIC] CO2 emissions EU vs sum of EU countries',
        subtitle='AFTER filling missing data - Gap is theoretically NOT OK',
        y='Gt CO2 per year',
        x=''
      ) +
      guides(
        # Hide linetype legend
        linetype = "none",
        color = guide_legend(title = NULL)
      ) -> plt
     quicksave(file.path(diagnostics_folder, 'eu_vs_countries_co2_filled.png'),
              plot=plt,
           width=10, height=8, bg='white', scale=1.8, preview=F)


     # CO2 before projection ---------------------------------------------------
     plt_data <- co2_unprojected %>%
       detotalise_co2() %>%
       filter(iso2 %in% get_eu_iso2s(include_eu = T)) %>%
       {
         n_countries <- n_distinct(.$iso2)
         print(n_countries)
         stopifnot('Expected 27 EU countries + EU'= n_countries == 28)
         .
       } %>%
       mutate(is_eu=case_when(iso2=="EU" ~ "EU", TRUE ~ "EU member states")) %>%
       filter(date < "2025-01-01") %>%
       filter(date >= "2020-01-01") %>%
       group_by(
         fuel,
         sector,
         date=floor_date(date, "month"),
         is_eu) %>%
       summarise(value=sum(value, na.rm=T)/1e9) %>%
       arrange(desc(date))

     # Add total
     plt_data <- bind_rows(
       plt_data,
       plt_data %>%
         group_by(is_eu, date) %>%
         summarise(value=sum(value, na.rm=T),
                   fuel="total",
                   sector="all")
     )

     # Check EU vs countries
     plt_data %>%
       group_by(fuel) %>%
       ggplot(aes(date, value, col=is_eu, linewidth=is_eu, alpha=is_eu)) +
       geom_line(show.legend = F) +
       ggrepel::geom_text_repel(
         data = . %>% filter(date==max(date)),
         aes(label=is_eu), nudge_x = 0.5, nudge_y = 0.5,
         show.legend = F,
         # hide segment
         segment.color = NA,
         # vertically aligned
         direction = 'x',
         hjust = 0,
         # bold
         fontface = 'bold',

       ) +
       facet_wrap(sector~fuel) +
       scale_x_date(date_minor_breaks = "3 month",
                    date_breaks =  "1 year"
                    ) +
       # scale_x_continuous(expand = expansion(add = c(0, 3)),
       #                    breaks = seq(2015, 2025, 1)
       #                    ) +
       rcrea::scale_color_crea_d() +
       rcrea::theme_crea_new() +
       theme(
         panel.grid.minor.x = element_line(color="grey98"),
         panel.grid.major.x = element_line(color="grey90"),
       ) +
       rcrea::scale_y_crea_zero() +
       scale_alpha_manual(values=c(1, 0.8)) +
       scale_linewidth_manual(values=c(1.2, 0.7)) +
       labs(
         title='[DIAGNOSTIC] CO2 emissions EU vs sum of EU countries',
         subtitle='BEFORE filling missing data - Gap is theoretically OK',
         y='Gt CO2 per year',
         x=''
       ) -> plt

     plt
     quicksave(file.path(diagnostics_folder, 'eu_vs_countries_co2.png'),
               plot=plt,
               width=10, height=8, bg='white', scale=1.5, preview=F)
  }
}

