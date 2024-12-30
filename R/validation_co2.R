validate_co2 <- function(co2_daily, folder="diagnostics", region="EU", date_from="1990-01-01") {

  dir.create(folder, FALSE, TRUE)

  # Get validation data once
  validation_data <- get_validation_data(region=unique(co2_daily$iso2))

  # Run all validations with shared validation data
  validate_co2_historical(co2_daily, validation_data, region, folder, date_from)
  validate_co2_timeseries(co2_daily, folder)  # This one doesn't need validation data
  validate_co2_yearly(co2_daily, validation_data, folder)
  validate_co2_monthly(co2_daily, validation_data, folder)
}

# Update the historical validation function signature
validate_co2_historical <- function(co2_daily = NULL,
                                  validation_data,  # New parameter
                                  region = "EU",
                                  folder = "diagnostics",
                                  date_from = "1990-01-01") {
  # Remove the get_validation_data() call since data is passed in
  # Rest of function remains the same
  dir.create(folder, F, T)

  # Load CREA data
  co2_crea <- creahelpers::default_if_null(co2_daily,
                                          download_co2_daily(date_from = date_from, iso2s=region)) %>%
    filter(sector == SECTOR_ALL,
           fuel == FUEL_TOTAL,
           iso2 %in% c(!!region)) %>%
    group_by(iso2, year = year(date)) %>%
    summarise(value = sum(value)/1e6,
              unit = "mt",
              source = 'CREA') %>%
    filter(year < 2025)

  # Load validation data
  co2_validate <- validation_data

  co2_extended <- extend_validation_data(co2_crea = co2_crea,
                                         co2_validate = co2_validate)

  plot_data <- bind_rows(
    co2_crea %>% filter(year >= 1990) %>% mutate(type='estimated'),
    co2_validate %>% mutate(type='estimated'),
    co2_extended %>% mutate(type='projected')
  ) %>%
      write_csv(file.path(folder, "validation.csv")) %>%
      # filter(type=="estimated") %>%
      mutate(
        source=factor(source, levels=c("CREA", unique(co2_validate$source)))
      )

  n_sources <- n_distinct(plot_data$source)
  alphas <- c(0.9, rep(1, n_sources-1))
  linewidths <- c(1.6, rep(0.5, n_sources-1))
  colors <- unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red", "Yellow", "Dark.violet", "Turquoise")])

  ggplot(plot_data) +
      geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source, linetype=type)) +
      scale_x_continuous(limits=c(min(co2_crea$year), NA)) +
      scale_alpha_manual(values=alphas) +
      scale_linewidth_manual(values=linewidths) +
      scale_color_manual(values=colors) +
      rcrea::theme_crea_new() +
      rcrea::scale_y_crea_zero() +
      {
        if(length(unique(co2_crea$iso2)) > 1){
          facet_wrap(~iso2, scales='free_y')
        }
      } +
      labs(title="EU CO2 emissions from fossil fuels",
           subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
           y=NULL,
           x=NULL,
           linewidth="Source",
           linetype=NULL,
           alpha="Source",
           color="Source",
           caption="Source: CREA analysis based on Climate Watch data. Agriculture and LULUCF are not included in this comparison.") -> plt

  plt
  quicksave(file.path(folder, "validation.jpg"), plot=plt)


  # Create a version with hline and vline -----------------------------------
  last_year <- max(co2_crea$year)
  interpolated <- co2_validate %>%
    filter(source %in% c('PIK', 'GCP')) %>%
    group_by(source) %>%
    arrange(year) %>%
    left_join(co2_extended %>%
                filter(source %in% c('PIK', 'GCP'),
                       year==last_year) %>% select(target_value=value, source)) %>%
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
    ggrepel::geom_text_repel(
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
    filter(fuel!=FUEL_TOTAL) %>%
    group_by(iso2, year=year(date), fuel) %>%
    summarise(value=sum(value)/1e6,
              unit="mt",
              source='CREA')

  ggplot(co2_yearly) +
    geom_area(aes(year, value, fill=fuel)) +
    geom_line(data=co2_validate %>% filter(year >= 1990), aes(year, value, col=source)) +
    # geom_line(data=co2_yearly %>% filter(year >= 1990), aes(year, value, col=fuel)) +
    {
      if(length(unique(co2_yearly$iso2)) > 1){
        facet_wrap(~iso2, scales='free_y')
      }
    }

#
#   co2_monthly <- co2_daily %>%
#     filter(fuel!='Total',
#            region=='EU') %>%
#     group_by(date=floor_date(date, 'month'), fuel) %>%
#     summarise(value=sum(value)/1e6,
#               unit="mt",
#               source='CREA')
#
#
#   ggplot(co2_monthly) +
#     geom_area(aes(date, value, fill=fuel)) +
#     geom_line(data=co2_validate %>% filter(year >= 1990), aes(year, value, col=source)) +
#     geom_line(data=co2_yearly %>% filter(year >= 1990), aes(year, value, col=fuel))


# Are we understimating any? ----------------------------------------------
  summary(co2_validate %>% filter(source=='PIK') %>%
    left_join(co2_yearly %>% select(year, fuel, value) %>%
                tidyr::spread(fuel, value)) %>%
    filter(!is.na(oil)) %>%
    lm(value - oil - gas ~ coal + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(oil)) %>%
            lm(value - oil - coal ~ gas + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(oil)) %>%
            lm(value - gas - coal ~ oil + 0, data=.))

  summary(co2_validate %>% filter(source=='GCP') %>%
            left_join(co2_yearly %>% select(year, fuel, value) %>%
                        tidyr::spread(fuel, value)) %>%
            filter(!is.na(oil)) %>%
            lm(value ~ gas + coal + oil + 0, data=.))

}


diagnostic_co2 <- function(co2_daily, diagnostics_folder="diagnostics"){
  diagnostic_co2_simple(co2_daily, diagnostics_folder)
  diagnostic_co2_benchmark_yearly(co2_daily, diagnostics_folder)
  diagnostic_co2_benchmark_monthly(co2_daily, diagnostics_folder)
}


diagnostic_co2_simple <- function(co2_daily, diagnostics_folder="diagnostics"){

  if(!is.null(diagnostics_folder)){
    dir.create(diagnostics_folder, showWarnings = FALSE)

    geos <- unique(co2_daily$geo)

    lapply(geos, function(geo){
      plt_data <- co2_daily %>%
        filter(geo==!!geo) %>%
        mutate(across(c(fuel, sector), tolower)) %>%
        group_by(sector, fuel) %>%
        mutate(CO2_30d = zoo::rollapplyr(value_co2_tonne, 30, mean, fill=NA),
               year=as.factor(year(date)),
               plotdate=date %>% 'year<-'(2022)) %>%
        filter(year(date)>=2015)

      # Generate a custom red gradient palette
      n_years <- length(unique(plt_data$year))
      red_palette <- colorRampPalette(c("pink", "red", "darkred"))(n_years)
      # Replace last value with blue
      red_palette[n_years-1] <- rcrea::pal_crea[["Blue"]]
      red_palette[n_years] <- rcrea::pal_crea[["Dark.blue"]]
      size_values <- seq(0.6, 0.9, length.out = n_years)^4



      (plt <- plt_data %>%
          ggplot(aes(plotdate, CO2_30d/1e6, col=year)) +
          geom_line(aes(size=year)) +
          facet_wrap(~glue("{str_to_title(fuel)} - {str_to_title(sector)}"), scales='free_y') +
          expand_limits(y=0)  + scale_x_date(expand=c(0,0)) +
          # scale_color_viridis_d(direction=-1) +
          scale_color_manual(values=red_palette)+
          scale_size_manual(values=size_values) +
          theme_crea_new() +
          rcrea::scale_y_crea_zero() +
          theme(legend.position = "right") +
          labs(title=glue("{geo} CO2 emissions"),
               subtitle='Million tonne CO2 per day. 30-day moving average',
               caption="Source: CREA analysis based on ENTSOG, ENTSOE, EUROSTAT and ASGI.",
               y=NULL,
               x=NULL,
               color=NULL,
               size=NULL) +
        guides(size=guide_legend(ncol=1), color=guide_legend(ncol=1)))

      filepath <- file.path(diagnostics_folder, glue("co2_{tolower(geo)}_.png"))
      rcrea::quicksave(filepath, plot=plt, width=10, height=7, bg='white', scale=1)
    })


  }
}

diagnostic_co2_benchmark_yearly <- function(co2_daily, diagnostics_folder="diagnostics"){

  if(!is.null(diagnostics_folder)){
    co2_crea <- co2_daily %>%
      filter(sector=='all',
             fuel=='total') %>%
      group_by(iso2, year=year(date)) %>%
      summarise(value=sum(value_co2_tonne)/1e6,
                unit="mt",
                source='CREA CO2 tracker') %>%
      filter(year < 2025) %>%
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

      filepath <- "data/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_13-Sep-2024.csv"
      url <- "https://zenodo.org/records/13752654/files/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_13-Sep-2024.csv?download=1"
      if(!file.exists(filepath)){
          dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
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
        # read_benchmark(get_data_filepath('ghg-emissions-climatewatch-withindustry.csv')) %>% mutate(source='Climate Watch (with industry)'),
        # read_benchmark(get_data_filepath('ghg-emissions-climatewatch.csv')) %>% mutate(source='Climate Watch'),
        # read_benchmark(get_data_filepath('ghg-emissions-unfccc-withindustry.csv')) %>% mutate(source='UNFCCC (with industry)'),
        # read_benchmark(get_data_filepath('ghg-emissions-unfccc.csv')) %>% mutate(source='UNFCCC'),
        # read_benchmark(get_data_filepath('ghg-emissions-gcp.csv')) %>% mutate(source='GCP'),
        # read_benchmark(get_data_filepath('ghg-emissions-pik-withindustry.csv')) %>% mutate(source='Old PRIMAP-hist v2.5 (Energy and Industry)'),
        # read_benchmark(get_data_filepath('ghg-emissions-pik.csv')) %>% mutate(source='PRIMAP-hist v2.5 (Energy alone)')
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

    (
      bind_rows(
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
        rcrea::theme_crea_new() +
        # Legend on the top, left align
        # theme(legend.position = "top",
        #       legend.title = element_blank(),
        #       # Left align
        #
        #       ) +

        rcrea::scale_y_crea_zero() +
        {
          if(length(unique(co2_crea$iso2))>1) facet_wrap(~iso2, scales='free_y')
        } +

        labs(title="CO2 emissions from fossil fuels",
             subtitle="Billion tonne CO2 per year",
             y=NULL,
             x=NULL,
             linewidth="Source",
             linetype="Source",
             alpha="Source",
             color="Source",
             caption="Note: PRIMAP refers to PRIMAP-hist v2.6 and includes non-fossil fuels related emissions.") -> plt)

    plt

    quicksave(file.path(diagnostics_folder, "co2_benchmark.jpg"), plot=plt, width=8, height=4, scale=1, logo=F, dpi=600)
  }
}


diagnostic_co2_benchmark_monthly <- function(co2_daily, diagnostics_folder="diagnostics"){

  #TODO add diagnostics data to package
  if(!is.null(diagnostics_folder)){

  co2_crea <- co2_daily %>%
    filter(fuel!='total') %>%
    group_by(iso2, geo, month=floor_date(date, 'month')) %>%
    summarise(value=sum(value_co2_tonne)/1e6/(as.integer(difftime(max(date),min(date)))+1),
              unit="mt/day",
              source='CREA CO2 Tracker',
              .groups="drop") %>%
    ungroup() %>%
    rename(date=month)



  url <- "https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu"
  filepath <- "data/CM_EU.csv"
  if(!file.exists(filepath)){
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
    download.file(url, filepath)
  }

  co2_carbonmonitor <- read_csv(filepath) %>%
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

      {
        if(length(unique(co2_crea$iso2))>1) facet_wrap(~iso2, scales='free_y')
      } +

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

#' Extend validation data using CREA's year-over-year ratios
#'
#' @param co2_crea Data frame with CREA CO2 data
#' @param co2_validate Data frame with validation data from different sources
#'
#' @return Data frame with projected values for each source
extend_validation_data <- function(co2_crea, co2_validate) {
  # Find projection year range
  earliest_projection_year <- co2_validate %>%
    group_by(source) %>%
    summarise(last_year = max(year)) %>%
    pull(last_year) %>%
    min() + 1

  latest_projection_year <- max(co2_crea$year)

  projection_years <- seq(earliest_projection_year, latest_projection_year)

  # Get YOY ratios from CREA data
  crea_ratios <- co2_crea %>%
    group_by(iso2) %>%
    arrange(year) %>%
    mutate(ratio = value / lag(value)) %>%
    filter(year %in% projection_years) %>%
    select(iso2, year, ratio)

  # For each source, extend from its last available year
  projected_data <- co2_validate %>%
    group_by(source) %>%
    group_modify(function(source_data, group_key) {

      last_year <- max(source_data$year)

      # Get CREA ratios starting from this source's last year
      relevant_ratios <- crea_ratios %>%
        filter(year > last_year) %>%
        group_by(iso2) %>%
        mutate(
          # Calculate cumulative ratios from the source's last year
          cumulative_ratio = cumprod(ratio),
          # Reference year for the projection
          ref_year = last_year
        ) %>%
        ungroup()

      if(nrow(relevant_ratios) == 0) return(data.frame())

      # Project the values forward
      projections <- source_data %>%
        filter(year == last_year) %>%
        select(-year) %>%
        full_join(
          relevant_ratios,
          by = "iso2",
          relationship = "many-to-many"
        ) %>%
        mutate(
          value = value * cumulative_ratio,
          year = year
        ) %>%
        select(iso2, year, value, unit)

      return(projections)
    }) %>%
    ungroup()

  # Include latest available data to ensure continuity in plot
  projected_data <- bind_rows(
    co2_validate %>% group_by(source) %>% filter(year == max(year)) %>% ungroup(),
    projected_data
  )

  return(projected_data)
}

# Example usage:
# projected_data <- extend_validation_data(co2_crea, co2_validate)
#
# plot_data <- bind_rows(
#   co2_crea %>% filter(year >= 1990) %>% mutate(type = 'estimated'),
#   co2_validate %>% mutate(type = 'estimated'),
#   projected_data %>% mutate(type = 'projected')
# )
