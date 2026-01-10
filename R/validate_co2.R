validate_co2 <- function(co2,
                         diagnostics_folder="diagnostics",
                         date_from="1990-01-01") {

  if(is_null_or_empty(diagnostics_folder)){
    message("No diagnostics folder provided. Skipping validation.")
    return()
  }

  create_dir(diagnostics_folder)

  # Get validation data once
  validation_data <- get_validation_data(region=unique(co2$iso2))
  valid_iso2s <- get_valid_countries(co2, validation_data)
  latest_year <- max(lubridate::year(co2$date), na.rm = TRUE)

  # Run all validations with shared validation data
  validate_co2_historical(co2,
                          validation_data,
                          folder = file.path(diagnostics_folder, "co2_historical"),
                          date_from = date_from,
                          year_to = latest_year)

  validate_co2_timeseries(co2,
                          folder = file.path(diagnostics_folder, "co2_timeseries")
                          )

  validate_co2_monthly(co2,
                       folder = file.path(diagnostics_folder, "co2_monthly")
                       )

  validate_co2_transport(co2,
                         folder = file.path(diagnostics_folder, "co2_transport"),
                         year_to = latest_year
                         )
}


# Update the historical validation function signature
validate_co2_historical <- function(co2 = NULL,
                                    validation_data,
                                    iso2s = NULL,
                                    folder = "diagnostics",
                                    date_from = "1990-01-01",
                                    by_country = T,
                                    all_countries = T,
                                    year_to = NULL,
                                    exclude_international_aviation = TRUE
                                    ) {


  # Remove the get_validation_data() call since data is passed in
  # Rest of function remains the same
  dir.create(folder, F, T)

  if(is.null(iso2s)){
    iso2s <- unique(co2$iso2)
  }

  if (is.null(year_to)) {
    year_to <- max(lubridate::year(co2$date), na.rm = TRUE)
  }

  if(exclude_international_aviation){
    co2 <- exclude_international_aviation(co2)
  }

  if(all_countries){

    ##############################
    # Plot 1: ALL FUELS + SECTORS
    ##############################
    # Load CREA data
    co2_crea <- co2 %>%
      filter(fuel == FUEL_TOTAL,
             sector == SECTOR_ALL,
             iso2 %in% iso2s,
             estimate == "central"
      ) %>%
      group_by(iso2, year = year(date)) %>%
      summarise(value = sum(value)/1e6,
                unit = "mt",
                source = 'CREA') %>%
      filter(year <= year_to) %>%
      filter(year >= year(date_from))

    # Load validation data
    co2_validate <- validation_data %>%
      filter(iso2 %in% unique(co2_crea$iso2),
             sector == SECTOR_ALL,
             fuel == FUEL_TOTAL) %>%
      filter(grepl("Global Carbon Budget", source)) %>%
      filter(year <= year_to)


    plot_data <- bind_rows(
      co2_crea,
      co2_validate
    ) %>%
      mutate(
        source=factor(source, levels=c("CREA", unique(co2_validate$source)))
      )

    min_year <- min(co2_crea$year)
    x_breaks <- seq(min_year, year_to, by = 10)
    if (length(x_breaks) == 0) {
      x_breaks <- unique(c(min_year, year_to))
    }

    n_sources <- n_distinct(plot_data$source)
    alphas <- c(0.9, rep(1, n_sources-1))
    linewidths <- c(1.6, rep(0.5, n_sources-1))
    colors <- unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red",
                                       "Dark.purple", "Dark.violet", "Green", "Turquoise")])

    local({
      plt_from_data <- function(plt_data){
        ggplot(plot_data) +
          geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source)) +
          scale_x_continuous(limits=c(min_year, year_to),
                             breaks=x_breaks,
                             expand = expansion(mult = c(0, 0.02))
          ) +
          scale_alpha_manual(values=alphas) +
          scale_linewidth_manual(values=linewidths) +
          scale_color_manual(values=colors) +
          rcrea::theme_crea_new() +
          rcrea::scale_y_crea_zero() +
          {
            if(length(unique(co2_crea$iso2)) > 1){
              facet_wrap(~iso2_to_name(iso2),
                         scales='free_y',
                         ncol=4
              )
            }
          } +
          labs(title=glue("CO2 emissions from fossil fuels{if(exclude_international_aviation) ' (without international aviation)' else ''}"),
               subtitle="Comparison between CREA and Global Carbon Project estimates, in billion tonne CO2 per year",
               y=NULL,
               x=NULL,
               linewidth="Source",
               linetype=NULL,
               alpha="Source",
               color="Source",
               caption="Source: CREA analysis and Global Carbon Budget 2025 (Friedlingstein et al., 2025, ESSD).")
      }

      plt <- plt_from_data(plot_data)
      quicksave(file.path(folder, glue("validation_co2_countries.jpg")),
                plot=plt,
                width=8,
                height=10,
                preview = F,
                logo = F)

      # Create a version for countries matching validation only (for the methodology doc)
      valid_iso2s <- get_valid_countries(co2, validation_data)
      plot_data <- plot_data %>%
        filter(iso2 %in% valid_iso2s)

      plt <- plt_from_data(plot_data)
      quicksave(file.path(folder, glue("validation_co2_countries_valid.jpg")),
                plot=plt,
                width=8,
                height=10,
                preview = F,
                logo = F)
    })
   }

  if(by_country){
    lapply(iso2s, function(iso2){

      ##############################
      # Plot 1: ALL FUELS + SECTORS
      ##############################
      # Load CREA data
      co2_crea <- co2 %>%
        filter(fuel == FUEL_TOTAL,
               sector == SECTOR_ALL,
               iso2 %in% c(!!iso2),
               estimate == "central"
               ) %>%
        group_by(iso2, year = year(date)) %>%
        summarise(value = sum(value)/1e6,
                  unit = "mt",
                  source = 'CREA') %>%
        filter(year <= year_to)

      # Load validation data
      co2_validate <- validation_data %>%
        filter(iso2 %in% unique(co2_crea$iso2),
               sector == SECTOR_ALL,
               fuel == FUEL_TOTAL) %>%
        filter(source != "Carbon Monitor") %>%
        filter(year <= year_to)

      # co2_extended <- extend_validation_data(co2_crea = co2_crea,
      #                                        co2_validate = co2_validate)

      plot_data <- bind_rows(
        co2_crea %>% filter(year >= 1990) %>% mutate(type='estimated'),
        co2_validate %>% mutate(type='estimated')
        # co2_extended %>% mutate(type='projected')
      ) %>%
          filter(year <= year_to) %>%
          write_csv(file.path(folder, "validation.csv")) %>%
          # filter(type=="estimated") %>%
          mutate(
            source=factor(source, levels=c("CREA", unique(co2_validate$source)))
          )

      n_sources <- n_distinct(plot_data$source)
      alphas <- c(0.9, rep(1, n_sources-1))
      linewidths <- c(1.6, rep(0.5, n_sources-1))
      colors <- unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red",
                                         "Dark.purple", "Dark.violet", "Green", "Turquoise")])

      ggplot(plot_data) +
          geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source, linetype=type)) +
          scale_x_continuous(limits=c(min(co2_crea$year), year_to)) +
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
          labs(title=glue("{iso2_to_name(iso2)} CO2 emissions from fossil fuels"),
               subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
               y=NULL,
               x=NULL,
               linewidth="Source",
               linetype=NULL,
               alpha="Source",
               color="Source",
               caption="Source: CREA analysis based on Climate Watch data. Agriculture and LULUCF are not included in this comparison.") -> plt

      quicksave(file.path(folder, glue("validation_co2_{tolower(iso2)}.jpg")),
                plot=plt,
                preview = F)


      ##############################
      # Plot 2: BY FUEL
      ##############################
      # # Validation by fuel data doesn't include international aviation/maritime
      # co2_wo_international <- co2 %>%
      #   group_by(iso2) %>%
      #   # Because transport data is only available after a certain date
      #   # We cut it so that time series is consistent
      #   filter(
      #     any(sector %in% c(SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING,
      #                       SECTOR_TRANSPORT_INTERNATIONAL_AVIATION,
      #                       SECTOR_TRANSPORT_DOMESTIC
      #     ) & !is.na(value)),
      #     date >= min(date[sector %in% c(SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING,
      #                                    SECTOR_TRANSPORT_INTERNATIONAL_AVIATION,
      #                                    SECTOR_TRANSPORT_DOMESTIC
      #     ) & !is.na(value)]),
      #     ! sector %in% c(SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING,
      #                     SECTOR_TRANSPORT_INTERNATIONAL_AVIATION)
      #   ) %>%
      #   ungroup()


      # Load CREA data
      co2_crea <- co2 %>%
        combine_coke_coal() %>%
        filter(
          # sector == SECTOR_ALL,
               # fuel != FUEL_TOTAL,
               iso2 %in% c(!!iso2),
               estimate == "central"
        ) %>%
        group_by(iso2, year = year(date), fuel) %>%
        summarise(value = sum(value, na.rm=T)/1e6,
                  unit = "mt",
                  source = 'CREA') %>%
        filter(year <= year_to)

      # Load validation data
      co2_validate <- validation_data %>%
        filter(iso2 %in% unique(co2_crea$iso2),
               sector == SECTOR_ALL,
               ) %>%
        group_by(source) %>%
        filter(any(fuel != FUEL_TOTAL)) %>%
        filter(year <= year_to)

      # co2_extended <- extend_validation_data(co2_crea = co2_crea,
      #                                        co2_validate = co2_validate)

      plot_data <- bind_rows(
        co2_crea %>% filter(year >= 1990) %>% mutate(type='estimated'),
        co2_validate %>% mutate(type='estimated')
        # co2_extended %>% mutate(type='projected')
      ) %>%
        filter(year <= year_to) %>%
        write_csv(file.path(folder, "validation.csv")) %>%
        # filter(type=="estimated") %>%
        mutate(
          source=factor(source, levels=c("CREA", unique(co2_validate$source)))
        )

      n_sources <- n_distinct(plot_data$source)
      alphas <- c(0.9, rep(1, n_sources-1))
      linewidths <- c(1.6, rep(0.5, n_sources-1))
      colors <- unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red",
                                         "Dark.purple", "Dark.violet", "Green", "Turquoise")])

      ggplot(plot_data) +
        geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source)) +
        scale_x_continuous(limits=c(min(co2_crea$year), year_to)) +
        scale_alpha_manual(values=alphas) +
        scale_linewidth_manual(values=linewidths) +
        scale_color_manual(values=colors) +
        rcrea::theme_crea_new() +
        rcrea::scale_y_crea_zero() +
        facet_wrap(~stringr::str_to_title(fuel), scales='free_y') +
        labs(title=glue("{iso2_to_name(iso2)} CO2 emissions from fossil fuels"),
             subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
             y=NULL,
             x=NULL,
             linewidth="Source",
             linetype=NULL,
             alpha="Source",
             color="Source",
             caption="Source: CREA analysis based on Climate Watch data. Agriculture and LULUCF are not included in this comparison.") -> plt

      quicksave(file.path(folder, glue("validation_co2_{tolower(iso2)}_byfuel.jpg")),
                plot=plt,
                preview = F)

    })
  }


  # Create a version with hline and vline -----------------------------------
  tryCatch({
  if("EU" %in% iso2s){
    last_year <- max(co2_crea$year)
    interpolated <- validation_data %>%
      filter(iso2=="EU") %>%
      filter(source %in% c('PIK', 'GCP')) %>%
      group_by(iso2, source) %>%
      arrange(year) %>%
      filter(!is.na(value)) %>%
      left_join(co2_extended %>%
                  filter(source %in% c('PIK', 'GCP'),
                         year==last_year) %>% select(iso2, target_value=value, source)) %>%
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
          data = co2_extended %>% filter(year==max(year), source %in% c('PIK', 'GCP')),
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
  }}, error=function(e){
    print("Failed to generate interpolated plot. Maybe because estimated values are too low.")
  })

  # Check fuel --------------------------------------------------------------
  # co2_yearly <- co2 %>%
  #   filter(fuel!=FUEL_TOTAL) %>%
  #   group_by(iso2, year=year(date), fuel) %>%
  #   summarise(value=sum(value)/1e6,
  #             unit="mt",
  #             source='CREA')
  #
  # co2_validate_yearly <- validation_data %>%
  #   filter(iso2 %in% unique(co2_yearly$iso2),
  #          sector == SECTOR_ALL,
  #          source=="GCP2"
  #   ) %>%
  #   filter(any(fuel != FUEL_TOTAL)) %>%
  #   group_by(iso2, source, year) %>%
  #   summarise(value=sum(value))
  #
  #
  # ggplot(co2_yearly) +
  #   geom_area(aes(year, value, fill=fuel)) +
  #   geom_line(data=co2_validate_yearly %>% filter(year >= 1990), aes(year, value, col=source)) +
  #   # geom_line(data=co2_yearly %>% filter(year >= 1990), aes(year, value, col=fuel)) +
  #   {
  #     if(length(unique(co2_yearly$iso2)) > 1){
  #       facet_wrap(~iso2, scales='free_y')
  #     }
  #   }
  #
#
#   co2_monthly <- co2 %>%
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
  # summary(co2_validate %>% filter(source=='PIK') %>%
  #   left_join(co2_yearly %>% select(year, fuel, value) %>%
  #               tidyr::spread(fuel, value)) %>%
  #   filter(!is.na(oil)) %>%
  #   lm(value - oil - gas ~ coal + 0, data=.))
  #
  # summary(co2_validate %>% filter(source=='GCP') %>%
  #           left_join(co2_yearly %>% select(year, fuel, value) %>%
  #                       tidyr::spread(fuel, value)) %>%
  #           filter(!is.na(oil)) %>%
  #           lm(value - oil - coal ~ gas + 0, data=.))
  #
  # summary(co2_validate %>% filter(source=='GCP') %>%
  #           left_join(co2_yearly %>% select(year, fuel, value) %>%
  #                       tidyr::spread(fuel, value)) %>%
  #           filter(!is.na(oil)) %>%
  #           lm(value - gas - coal ~ oil + 0, data=.))
  #
  # summary(co2_validate %>% filter(source=='GCP') %>%
  #           left_join(co2_yearly %>% select(year, fuel, value) %>%
  #                       tidyr::spread(fuel, value)) %>%
  #           filter(!is.na(oil)) %>%
  #           lm(value ~ gas + coal + oil + 0, data=.))

}



validate_co2_timeseries <- function(co2, folder="diagnostics"){

  if(!is.null(folder)){
    dir.create(folder, showWarnings = FALSE)

    iso2s <- unique(co2$iso2)
    is_daily <- any(day(co2$date) != 1)

    lapply(iso2s, function(iso2){
      country <- countrycode::countrycode(iso2, "iso2c", "country.name", custom_match = c("EU"="European Union"))
      plt_data <- co2 %>%
        filter(iso2==!!iso2) %>%
        mutate(across(c(fuel, sector), tolower)) %>%
        group_by(sector, fuel, estimate) %>%
        arrange(date) %>%
        {
          if(is_daily){
            # Daily data: calculate rolling monthly average
            mutate(., value = zoo::rollapplyr(value, 30, mean, fill=NA))
          } else {
            # Monthly data: aggregate to monthly sums
            .
          }
        } %>%
        mutate(year=as.factor(year(date)),
               plotdate=date %>% 'year<-'(2022)) %>%
        filter(year(date)>=2015) %>%
        select(plotdate, year, fuel, sector, estimate, value) %>%
        ungroup() %>%
        pivot_wider(names_from=estimate, values_from=value)

      # Generate a custom red gradient palette
      n_years <- length(unique(plt_data$year))
      red_palette <- colorRampPalette(c("pink", "red", "darkred"))(n_years)
      # Replace last value with blue
      red_palette[n_years-1] <- rcrea::pal_crea[["Blue"]]
      red_palette[n_years] <- rcrea::pal_crea[["Dark.blue"]]
      size_values <- seq(0.6, 0.9, length.out = n_years)^4



      (plt <- plt_data %>%
          ggplot(aes(plotdate, central/1e6, col=year, fill=year)) +
          geom_line(aes(size=year)) +
          geom_ribbon(aes(ymin=lower/1e6, ymax=upper/1e6),
                      color = "transparent",
                      alpha=0.3, show.legend = F) +
          facet_wrap(~glue("{str_to_title(fuel)} - {str_to_title(sector)}"), scales='free_y') +
          expand_limits(y=0)  + scale_x_date(expand=c(0,0)) +
          # scale_color_viridis_d(direction=-1) +
          scale_color_manual(values=red_palette)+
          scale_fill_manual(values=red_palette) +
          scale_size_manual(values=size_values) +
          theme_crea_new() +
          rcrea::scale_y_crea_zero() +
          theme(legend.position = "right") +
          labs(title=glue("{country} CO2 emissions"),
               subtitle=paste0('Million tonne CO2 per day', if_else(is_daily, "30-day moving average","")),
               caption="Source: CREA analysis based on ENTSOG, ENTSOE, EUROSTAT and ASGI.",
               y=NULL,
               x=NULL,
               color=NULL,
               size=NULL) +
        guides(size=guide_legend(ncol=1), color=guide_legend(ncol=1)))

      filepath <- file.path(folder, glue("co2_timeseries_byfuel_{tolower(iso2)}_.png"))
      rcrea::quicksave(filepath, plot=plt, width=10, height=7, bg='white', scale=1)
    })
  }
}


validate_co2_monthly <- function(co2, folder="diagnostics"){

  dir.create(folder, showWarnings = FALSE, recursive = T)

  co2_crea <- co2 %>%
    filter(fuel!='total', estimate=="central") %>%
    group_by(iso2, month=floor_date(date, 'month')) %>%
    summarise(value=sum(value)/1e6/(as.integer(difftime(max(date),min(date)))+1),
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

  quicksave(file.path(folder, "co2_benchmark_carbonmonitor_monthly.jpg"), plot=plt,
            width=12, height=8, logo=F, preview=F)

}


validate_co2_transport <- function(co2,
                                   folder="diagnostics",
                                   date_from = "1990-01-01",
                                   year_to = NULL){


  dir.create(folder, showWarnings = FALSE, recursive = T)

  if (is.null(year_to)) {
    year_to <- max(lubridate::year(co2$date), na.rm = TRUE)
  }

  climatewatch <- load_climatewatch_csv("climatewatch-transport") %>%
    mutate(source="Climate Watch")

  eea_filepath <- get_data_filepath("greenhouse_gas emissions from transport in europe.csv")
  eea <- read_csv(eea_filepath, skip=10) %>%
    `names<-`(c('value', 'value_proj1', 'value_proj2', 'year', 'x', 'y')) %>%
    select(year, value) %>%
    filter(!is.na(year),
           year>2010) %>%
    mutate(iso2="EU",
           source="EEA")

  url <- "https://sdi.eea.europa.eu/webdav/datastore/public/eea_t_national-emissions-reported_p_2024_v01_r00/CSV/UNFCCC_v27.csv"
  filepath <- file.path("data", basename(url))
  if(!file.exists(filepath)){
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = T)
    download.file(url, filepath)
  }
  eea_w_international <- read_csv(filepath) %>%
    filter(Country_code=="EUA") %>%
    filter(Sector_code %in% c(
      "1.A.3", # Transport
      "1.D.1" # International bunkers
    )) %>%
    filter(Pollutant_name=="CO2",
           Unit=="Gg") %>%
    group_by(year=as.numeric(Year), iso2="EU") %>%
    summarise(value=sum(emissions)/1e3) %>%
    mutate(source="UNFCCC (w. international aviation and shipping)")


  iso2s <- unique(co2$iso2)

  lapply(iso2s, function(iso2){

    # Load CREA data
    co2_crea_all <- co2 %>%
      filter(
             iso2 == !!iso2,
             sector %in% c(SECTOR_TRANSPORT_DOMESTIC,
                           SECTOR_TRANSPORT_INTERNATIONAL_AVIATION,
                           SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING),
             fuel == FUEL_OIL,
             estimate == "central"
      ) %>%
      group_by(iso2, year = year(date)) %>%
      summarise(value = sum(value)/1e6,
                unit = "mt",
                source = 'CREA') %>%
      filter(year <= year_to)

    co2_crea_domestic <- co2 %>%
      filter(
        iso2 == !!iso2,
        sector %in% c(SECTOR_TRANSPORT_DOMESTIC),
        fuel == FUEL_OIL,
        estimate == "central"
      ) %>%
      group_by(iso2, year = year(date)) %>%
      summarise(value = sum(value)/1e6,
                unit = "mt",
                source = 'CREA (domestic only)') %>%
      filter(year <= year_to)

    # Load validation data
    co2_validate <- bind_rows(climatewatch, eea, eea_w_international) %>%
      filter(iso2 == !!iso2)


    plot_data <- bind_rows(
      co2_crea_all %>% filter(year >= 1990),
      co2_crea_domestic %>% filter(year >= 1990),
      co2_validate
    ) %>%
      filter(year <= year_to) %>%
      # write_csv(file.path(folder, "validation_transport.csv")) %>%
      # filter(type=="estimated") %>%
      mutate(
        source=factor(source, levels=c("CREA", "CREA (domestic only)", unique(co2_validate$source)))
      )

    n_sources <- n_distinct(plot_data$source)
    alphas <- c(0.9, 0.9, rep(1, n_sources-2))
    linewidths <- c(1.6, 1.6, rep(0.5, n_sources-2))
    colors <- unname(rcrea::pal_crea[c("Blue", "Dark.red", "Dark.blue", "Orange", "Red", "Yellow", "Dark.violet", "Turquoise")])

    ggplot(plot_data) +
      geom_line(aes(year, value/1e3, col=source, linewidth=source, alpha=source)) +
      scale_x_continuous(limits=c(min(co2_crea_all$year), NA)) +
      scale_alpha_manual(values=alphas) +
      scale_linewidth_manual(values=linewidths) +
      scale_color_manual(values=colors) +
      rcrea::theme_crea_new() +
      rcrea::scale_y_crea_zero() +
      {
        if(length(unique(plot_data$iso2)) > 1){
          facet_wrap(~iso2, scales='free_y')
        }
      } +
      labs(title=glue("[DIAGNOSTICS] {iso2_to_name(iso2)} CO2 emissions in transport sector"),
           subtitle="Projection of historical sources using CREA CO2 tracker, in billion tonne CO2 per year",
           y=NULL,
           x=NULL,
           linewidth="Source",
           linetype=NULL,
           alpha="Source",
           color="Source",
           caption="Source: CREA analysis based on EUROSTAT and IPCC.") -> plt

    plt
    quicksave(file.path(folder, glue("validation_co2_transport_{tolower(iso2)}.jpg")),
              plot=plt,
              preview = F,
              width=8,
              height=5)
  })

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
compare_co2_versions <- function(iso2s="EU", versions=c("0.2", "0.3"), diagnostics_folder="diagnostics"){

  # Read the data
  co2 <- lapply(versions, function(version) download_co2(iso2s=iso2s, version=version)) %>%
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

  if(!is_null_or_empty(diagnostics_folder)){
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

  if(!is_null_or_empty(diagnostics_folder)){
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


