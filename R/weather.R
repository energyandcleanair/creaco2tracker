#' Get Raw Weather Data from API
#'
#' Fetches HDD and CDD weather data from the CREA API for EU regions.
#'
#' @return A data frame containing raw weather data with columns: date, variable (hdd/cdd),
#'         value, region_id, and other metadata
#' @export
get_weather_raw <- function() {
  creahelpers::api.get("api.energyandcleanair.org/v1/weather", variable="HDD,CDD", region_id="EU") %>%
    mutate(across(variable, tolower))
}

#' Fill Missing Values in Weather Data
#'
#' Takes raw weather data from API and fills missing values using forward fill method.
#' This function handles the data cleaning and gap filling for HDD and CDD data.
#'
#' @param weather_raw A data frame containing raw weather data with columns: date, variable (hdd/cdd),
#'                    value, region_id, and other metadata
#'
#' @return A data frame with filled weather data
#' @export
fill_weather <- function(weather_raw) {
  # Fill na values using forward fill method
  weather_raw %>%
    mutate(date = lubridate::date(date)) %>%
    ungroup() %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = 'day'),
                    tidyr::nesting(variable, unit, region_id, region_type, region_iso2, averaging_period, source, region_name)) %>%
    group_by(variable, unit, region_id, region_type, region_iso2, averaging_period, source, region_name) %>%
    arrange(date) %>%
    fill(value) %>%
    ungroup() ->
    weather_filled

  return(weather_filled)
}

#' Diagnose Weather Data
#'
#' Creates focused weather diagnostics including yearly trends and tilemap visualizations
#' to identify anomalies and missing data in HDD and CDD data.
#'
#' @param weather A data frame containing filled weather data with columns: date, variable (hdd/cdd),
#'                value, region_id, and other metadata
#' @param weather_raw A data frame containing raw weather data (before filling)
#' @param diagnostics_folder Path to save diagnostic plots and outputs
#' @param save_plots Whether to save plots to files (default: TRUE)
#'
#' @return A list containing diagnostic plots and summary statistics
#' @export
diagnose_weather <- function(weather,
                            weather_raw = NULL,
                            diagnostics_folder = 'diagnostics') {


  # Create diagnostics directory if it doesn't exist
  if (!is.null(diagnostics_folder) & !dir.exists(diagnostics_folder)) {
    dir.create(diagnostics_folder, recursive = TRUE)
  }

  # Use centralized quicksave function

  # Ensure date column is properly formatted

  # 1. Running average
  #additional plots
  running_plot_data <- weather %>%
    filter(year(date) >= 2018, region_id=='EU') %>%
    group_by(variable) %>%
    mutate(plotdate = date %>% 'year<-'(2000),
           across(value, zoo::rollapplyr, FUN=mean, width=30, fill=NA, na.rm=T))

  plt <- running_plot_data %>%
    mutate(plotdate=as.Date(plotdate)) %>%
    mutate(year=as.factor(year(date)),
           variable_name=ifelse(variable=='cdd', 'cooling', 'heating')) %>%
    ggplot(aes(plotdate, value, alpha=factor(year(date)), col=variable_name)) +
    facet_wrap(~variable_name, scales='free_y', ncol=1) +
    geom_line(size=1) +
    labs(title='EU average cooling and heating needs',
         subtitle='30-day running average. Population-weighted average for EU-27',
         y='degree-days', x='') +
    theme_crea_new() +
    theme(legend.position = 'right') +
    scale_color_crea_d('change', col.index = c(7,1), guide='none') +
    scale_alpha_discrete(range=c(.33,1),
                         # guide=guide_legend(nrow=1, override.aes = list(alpha=1, color=c('gray66', 'gray33', 'black')), title.position = 'left')
    ) +
    scale_x_date(date_labels = '%b', expand=expansion(mult=.01)) +
    rcrea::scale_y_crea_zero() +
    guides(alpha=guide_legend(title=NULL, title.position = 'right', ncol=1))

  if (!is.null(diagnostics_folder)) {
    quicksave(file.path(diagnostics_folder, 'weather_running_30day.png'),
              plot = plt, width = 10, height = 6)
  }


  # 3. Raw Weather Tilemap (if raw data provided)
  if (!is.null(weather_raw)) {

    # Create tilemap for raw data
    tilemap_raw_plot <- weather_raw %>%
      filter(!is.na(year)) %>%
      ggplot(aes(x = `year<-`(date, 2000), y = year(date), fill = value)) +
      geom_tile(data = ~ subset(., variable=='cdd'), aes(cdd=value)) +
      geom_tile(data = ~ subset(., variable=='hdd'), aes(hdd=value)) +
      scale_x_date(date_labels = '%b', expand = expansion(mult = .01)) +
      facet_wrap(~toupper(variable), scales = 'free', ncol = 1) +
      ggh4x::scale_fill_multi(
        aesthetics = c('hdd', 'cdd'),
        name = list("HDD", "CDD"),
        colours = list(
          hdd = viridis::viridis(100, option = 'plasma'),
          cdd = viridis::viridis(100, option = 'viridis')
        )) +
      labs(title = 'Raw HDD and CDD Data (Before Filling)',
           subtitle = 'Daily values shown as tiles - white indicates missing data',
           x = NULL, y=NULL) +
      theme_minimal()


    if (!is.null(diagnostics_folder)) {
      quicksave(file.path(diagnostics_folder, 'weather_data_availability.png'),
                plot = tilemap_raw_plot, width = 12, height = 8)
    }
  }
}
