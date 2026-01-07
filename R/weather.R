#' Get Weather Data from API (Comprehensive Mother Function)
#'
#' Fetches weather data from the CREA API with support for multiple variables,
#' location types, and caching.
#'
#' @param variable Character vector of weather variables to fetch. Options:
#'   - "HDD" (Heating Degree Days)
#'   - "CDD" (Cooling Degree Days)
#'   - "solar_radiation" (Solar radiation in W/m²)
#'   - "ws50m_daily" (Wind speed at 50m in m/s)
#'   Can be a single variable or comma-separated string (e.g., "HDD,CDD")
#' @param region_type Type of region. Options:
#'   - "country" (default) - For country-level data
#'   - "region" - For EU-wide or aggregated regional data
#'   - "station" - For individual weather stations (used with ws50m_daily)
#' @param region_id Region identifier. Examples:
#'   - "EU" for EU-wide data
#'   - "FR", "DE", etc. for country-specific data
#'   - NULL to get all available regions
#' @param region_iso2 Alternative to region_id, specify country by ISO2 code
#' @param station_source Source of station data (e.g., "gem"). Only used when region_type="station"
#' @param date_from Start date for data retrieval (default: "2015-01-01")
#' @param date_to End date for data retrieval (optional)
#' @param use_local Logical, whether to use localhost:8080 instead of api.energyandcleanair.org (default: FALSE)
#' @param use_cache Logical, whether to use cached data (default: TRUE)
#' @param refresh_cache Logical, whether to refresh the cache (default: !use_cache)
#' @param split_by Split API calls by year to avoid timeouts (default: "year")
#' @param verbose Logical, whether to print verbose output (default: FALSE)
#' @param aggregate_by Optional aggregation field (e.g., "region_iso2" to aggregate stations by country)
#' @param aggregate_fn Optional aggregation function (e.g., "mean" for averaging)
#'
#' @return A data frame containing weather data with columns: date, variable, value, region_id, and other metadata
#'
#' @examples
#' # Get HDD and CDD for EU
#' weather_eu <- get_weather(variable = "HDD,CDD", region_id = "EU")
#'
#' # Get solar radiation for Germany
#' solar_de <- get_weather(variable = "solar_radiation", region_iso2 = "DE", region_type = "country")
#'
#' # Get wind speed from stations for France
#' wind_fr <- get_weather(variable = "ws50m_daily", region_iso2 = "FR",
#'                        region_type = "station", station_source = "gem")
#'
#' # Use local API
#' weather_local <- get_weather(variable = "HDD", region_id = "EU", use_local = TRUE)
#'
#' @export
get_weather <- function(variable = "HDD,CDD",
                        region_type = "country",
                        region_id = NULL,
                        region_iso2 = NULL,
                        station_source = NULL,
                        date_from = "2015-01-01",
                        date_to = NULL,
                        use_local = FALSE,
                        use_cache = TRUE,
                        refresh_cache = !use_cache,
                        split_by = "year",
                        verbose = FALSE,
                        aggregate_by = NULL,
                        aggregate_fn = NULL) {

  # Build base URL
  base_url <- ifelse(use_local, "http://localhost:8080", "https://api.energyandcleanair.org")
  endpoint <- glue::glue("{base_url}/v1/weather")

  # Build parameters list
  params <- list(
    variable = variable,
    region_type = region_type,
    date_from = date_from,
    split_by = split_by,
    use_cache = use_cache,
    refresh_cache = refresh_cache,
    cache_folder = "cache",
    verbose = verbose
  )

  # Add optional parameters
  if (!is.null(region_id)) {
    params$region_id <- region_id
  }

  if (!is.null(region_iso2)) {
    params$region_iso2 <- region_iso2
  }

  if (!is.null(date_to)) {
    params$date_to <- date_to
  }

  # Add station_source if region_type is station
  if (region_type == "station" && !is.null(station_source)) {
    params$station_source <- station_source
  }

  # Add aggregation parameters if provided
  if (!is.null(aggregate_by)) {
    params$aggregate_by <- aggregate_by
  }

  if (!is.null(aggregate_fn)) {
    params$aggregate_fn <- aggregate_fn
  }

  # Call API
  weather_data <- do.call(creahelpers::api.get,
                          c(list(endpoint = endpoint), params))

  # Standardize variable names to lowercase for consistency
  weather_data <- weather_data %>%
    mutate(across(variable, tolower))

  return(weather_data)
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
    filter(year(date) >= 2018) %>%
    group_by(region_id, variable) %>%
    mutate(plotdate = date %>% 'year<-'(2000),
           across(value, zoo::rollapplyr, FUN=mean, width=30, fill=NA, na.rm=T))

  plt <- running_plot_data %>%
    mutate(plotdate=as.Date(plotdate)) %>%
    mutate(year=as.factor(year(date)),
           variable_name=ifelse(variable=='cdd', 'cooling', 'heating')) %>%
    ggplot(aes(plotdate, value, alpha=factor(year(date)), col=variable_name)) +
    facet_grid(region_id~variable_name, scales='free_y') +
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
    guides(alpha=guide_legend(title=NULL, title.position = 'right'))

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
      facet_grid(region_id~toupper(variable), scales = 'free') +
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
