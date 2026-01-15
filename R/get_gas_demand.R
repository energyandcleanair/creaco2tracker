## ---------------------------
##
## Script name: get_gas_demand.R
##
## Purpose of script: Compute apparent gas consumption from ENTSOG data stored in CREA's DB
## and make it ready for upload in CREA's DB.
##
## Author: Hubert Thieriot
##
## Date Created: 2022-12-26
##
## Copyright (c) CREA, 2018
## Email: hubert@energyandcleanair.org
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

#' Get gas demand data with optional Eurostat correction
#'
#' Computes apparent gas consumption from ENTSOG data. Optionally corrects
#' historical data to match Eurostat monthly totals using simple monthly scaling.
#'
#' @param diagnostics_folder Folder path for diagnostics outputs. Set to NULL
#'   to disable diagnostics. Default is "diagnostics/gas_demand".
#' @param verbose Whether to print verbose output. Default is FALSE.
#' @param use_cache Whether to use cached data. Default is FALSE.
#' @param refresh_cache Whether to refresh the cache. Default is FALSE.
#' @param correct_to_eurostat Whether to correct ENTSOG daily data to match
#'   Eurostat monthly totals. Default is FALSE for backward compatibility.
#'
#' @return Tibble with daily gas demand data
#'
#' @details
#' When correct_to_eurostat=TRUE:
#' - For months where Eurostat data is available: ENTSOG daily values are scaled
#'   so that monthly totals match Eurostat exactly.
#' - For recent months without Eurostat: The most recent available monthly
#'   scale factor is carried forward.
#'
#' @export
get_gas_demand <- function(diagnostics_folder='diagnostics/gas_demand',
                           verbose=FALSE,
                           use_cache=FALSE,
                           refresh_cache=FALSE,
                           correct_to_eurostat=FALSE){

  years <- seq(2015, lubridate::year(lubridate::today()))

  create_dir(diagnostics_folder)

  # Get all ENTSOG data once
  entsog_data <- creahelpers::api.get("https://api.russiafossiltracker.com/v0/entsogflow",
                                     date_from=glue("{min(years)}-01-01}"),
                                     date_to=glue("{max(years)}-12-31}"),
                                     type='consumption,distribution,storage,crossborder,production',
                                     split_by='year',
                                     verbose=verbose,
                                     use_cache=use_cache,
                                     refresh_cache=refresh_cache)

  # Estimate with two different methods
  message('Getting gas demand from Consumption + Distribution ENTSOG points')
  consdist <- get_gas_demand_consdist(entsog_data=entsog_data, years=years, verbose=verbose)
  message('Getting gas demand from storage,crossborder,production')
  apparent <- get_gas_demand_apparent(entsog_data=entsog_data, years=years, verbose=verbose)
  message('Getting gas demand from storage,crossborder,production using AGSI for storage')
  apparent_w_agsi <- get_gas_demand_apparent(entsog_data=entsog_data, years=years, use_agsi_for_storage=T, verbose=verbose)


  # Keep the best ones, and only those that match quality criteria
  gas_demand <- keep_best(consumption=bind_rows(consdist,
                                                apparent,
                                                apparent_w_agsi),
            min_comparison_points = 12,
            diagnostics_folder=diagnostics_folder,
            min_r2=0.95,
            max_rrse=0.3)

  # Store raw gas demand for diagnostics
  gas_demand_raw <- gas_demand

  # Apply Eurostat correction if requested
  if (correct_to_eurostat) {
    message("Applying Eurostat correction to gas demand...")
    gas_demand <- .apply_eurostat_gas_correction(
      gas_demand = gas_demand
    )
  }

  # Generate diagnostics
  if (!is.null(diagnostics_folder)) {
    .generate_gas_diagnostics(
      gas_demand_raw = gas_demand_raw,
      gas_demand_corrected = if (correct_to_eurostat) gas_demand else NULL,
      diagnostics_folder = diagnostics_folder
    )
  }

  # Keep the longest one per country
  gas_demand %>%
    select(region_id=iso2, date, value=value_m3) %>%
    mutate(fuel='fossil_gas',
           sector='total',
           data_source=ifelse(correct_to_eurostat, 'crea_eurostat_corrected', 'crea'),
           unit='m3',
           frequency='daily',
           region_type=case_when(region_id=='EU' ~ 'region', T ~ 'iso2'))
}


#' Get national gas demand based on Consumption + Distribution ENTSOG points
#'
#' @param years
#'
#' @return
#' @export
#'
#' @examples
get_gas_demand_consdist <- function(entsog_data, years, verbose=F){

  entsog <- entsog_data %>%
    filter(type %in% c('consumption', 'distribution'))

  consdist <- entsog %>%
    group_by(iso2=destination_iso2, date) %>%
    summarise(value_m3=-sum(value_m3, na.rm=T)) %>%
    arrange(date) %>%

    # Interpolate missing data
    tidyr::complete(date=seq.Date(min(as.Date(date)),
                                  max(as.Date(date)), by='day'),
                    fill=list(value_m3=NA)) %>%
    mutate(value_m3=zoo::na.approx(value_m3)) %>%
    ungroup() %>%

    # Fill with 0 before and after each country's reporting period
    tidyr::complete(date=seq.Date(min(as.Date(entsog$date)),
                                  max(as.Date(entsog$date)), by='day'),
                    fill=list(value_m3=0)) %>%

    mutate(method='consdist')

  return(consdist)
}



#' Get national gas demand based on Net imports + production + storage drawdown
#'
#' @param years
#'
#' @return
#' @export
#'
#' @examples
get_gas_demand_apparent <- function(entsog_data, years, use_agsi_for_storage=F, verbose=F){

  entsog <- entsog_data %>%
    filter(type %in% c('storage', 'crossborder', 'production'))

  # Fill missing data
  entsog <- entsog %>%
    select(destination_iso2, departure_iso2, date, type, value_m3) %>%
    arrange(date) %>%
    group_by(destination_iso2, departure_iso2, type) %>%
    # Interpolate missing data
    tidyr::complete(date=seq.Date(min(as.Date(date)),
                                  max(as.Date(date)), by='day'),
                    fill=list(value_m3=NA)) %>%
    mutate(value_m3=zoo::na.approx(value_m3)) %>%
    ungroup()



  if(use_agsi_for_storage){
    # In some instances, AGSI is better than ENTSOG
    # Well, at least for Austria which has no storage data in ENTSOG
    storage_drawdown <- agsi.get_storage_change(date_from=min(as.Date(entsog$date)),
                                                date_to=max(as.Date(entsog$date)),
                                                iso2=get_eu_iso2s(),
                                                verbose=verbose
                                                )
    entsog <- entsog %>%
      filter(type!='storage') %>%
      bind_rows(
        storage_drawdown %>%
          select(destination_iso2=iso2,
                 date,
                 value_m3) %>%
          mutate(type='storage')
      )
  }


  # We're missing some flows within EU, so we take at the borders only
  # i.e. not doing this: entsog_eu <- entsog %>%
  #   mutate(destination_iso2=destination_region,
  #          departure_iso2=departure_region)
  ex_eu <- c('NO', 'GB', 'UK', 'TR', 'lng',
             'UA', 'RU', 'AZ', 'DZ', 'LY', 'BY', 'MA')
  entsog_eu <- entsog %>%
    mutate(destination_iso2=case_when(destination_iso2 %in% ex_eu ~ 'Others',
                                      T~'EU'),
           departure_iso2=case_when(departure_iso2 %in% ex_eu ~ 'Others',
                                    T~'EU'))

  entsog_all <- bind_rows(entsog, entsog_eu)

  # Implied consumption = net imports + production + storage_drawdown
  imports <- entsog_all %>%
    filter(type %in% c('crossborder')) %>%
    filter(departure_iso2 != destination_iso2) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='imports')

  minus_exports <- entsog_all %>%
    filter(type %in% c('crossborder')) %>%
    filter(departure_iso2 != destination_iso2) %>%
    mutate(tmp=departure_iso2,
           departure_iso2=destination_iso2,
           destination_iso2=tmp,
           value_m3=-value_m3) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='minus_exports')

  net_imports <- bind_rows(imports, minus_exports) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='net_imports')

  storage_drawdown <- entsog_all %>%
    filter(type %in% c('storage')) %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='storage_drawdown')

  production <- entsog_all %>%
    filter(type == 'production') %>%
    group_by(destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    mutate(type='production')

  apparent <- bind_rows(imports, minus_exports, storage_drawdown, production) %>%
    group_by(iso2=destination_iso2, date) %>%
    summarise(across(value_m3, sum)) %>%
    ungroup() %>%
    tidyr::complete(date=seq.Date(min(as.Date(entsog$date)), max(as.Date(entsog$date)), by='day'),
                    fill=list(value_m3=0)) %>%
    mutate(method=ifelse(use_agsi_for_storage, 'apparent_agsi', 'apparent'))

  return(apparent)
}


get_eurostat_gas <- function(years){
  gas_consumption <- eurostat::get_eurostat("nrg_cb_gasm")
  gas_consumption %>%
    filter(unit=='MIO_M3',
           siec=="G3000") %>%
    mutate(type=recode(nrg_bal,
                       IC_OBS='consumption',
                       IPRD='production',
                       IMP='imports',
                       EXP='minus_exports',
                       STK_CHG_MG='storage',
                       .default=NA_character_,
    )) %>%
    filter(!is.na(type)) %>%
    mutate(values=ifelse(type %in% c('minus_exports', 'storage'), -values, values)) %>%
    select(iso2=geo, date=TIME_PERIOD, value_m3=values, type) %>%
    mutate(value_m3=value_m3*1e6,
           unit='m3',
           iso2=recode(iso2, 'UK'='GB', 'EU27_2020'='EU'),
           method='eurostat')
}



keep_best<- function(consumption,
                     diagnostics_folder,
                     min_comparison_points=24,
                     min_r2=0.95, max_rrse=0.4){

  eurostat = get_eurostat_gas() %>% filter(type=='consumption')
  rsq <- function (x, y) cor(x, y) ^ 2
  min_start <- min(consumption$date)
  max_start = max(eurostat$date) - months(min_comparison_points - 1)

  # We test the correlation for several starting dates
  # The older the better, but often it becomes accurate only after a certain date
  date_froms <- seq.Date(as.Date(min_start), as.Date(max_start), by='month')
  consumption_date_to <- consumption %>%
    group_by(iso2, method) %>%
    summarise(date_to=max(date))

  consumption_monthly <- consumption %>%
    filter(date < max(lubridate::floor_date(date, 'month'))) %>%
    group_by(iso2, method, date=lubridate::floor_date(date, 'month')) %>%
    summarise(value_m3=sum(value_m3)) %>%
    ungroup() %>%
    tidyr::complete(date, method, iso2, fill=list(value_m3=0))

  bests <- pbapply::pblapply(date_froms, function(date_from){
    consumption_monthly %>%
      filter(date >= date_from) %>%
      left_join(eurostat %>%
                  rename(value_m3_eurostat=value_m3) %>%
                  select(-c(method))) %>%
      filter(!is.na(value_m3_eurostat)) %>%
      group_by(iso2, method) %>%
      summarise(r2=rsq(value_m3_eurostat, value_m3),
                rrse=Metrics::rrse(value_m3_eurostat, value_m3),
                count=n()) %>%
      mutate(valid=(rrse < max_rrse) & (r2 > min_r2)) %>%
      group_by(iso2) %>%
      top_n(n=1, wt=r2) %>%
      ungroup() %>%
      arrange(desc(r2)) %>%
      mutate(date_from=!!date_from)
  }) %>%
    do.call(bind_rows, .)

  best <- bests %>%
    left_join(consumption_date_to) %>%
    filter(valid, count >= min_comparison_points) %>%
    group_by(iso2) %>%
    arrange(date_from) %>%
    # Avoid initial gaps
    filter(rrse < lead(rrse) + 0.02) %>%
    # Avoid sub-optimal
    filter(rrse < min(rrse) + 0.1) %>%
    # Avoid those that aren't updated
    filter(date_to > max(date_to) - lubridate::days(10)) %>%
    # Take earlier one
    filter(date_from==min(date_from)) %>%
    filter(r2==max(r2)) %>%
    # Sometimes apparent strictly equivalent to apparent_asgi
    distinct(iso2, .keep_all = T)

  best %>%
    filter(valid) %>%
    select(-c(r2, rrse)) %>%
    left_join(consumption) %>%
    ungroup()
}


#' Apply Eurostat correction to gas demand data
#'
#' Scales daily ENTSOG gas demand so that monthly totals match Eurostat.
#' For months without Eurostat data, carries forward the last available ratio.
#'
#' @param gas_demand Daily gas demand data from keep_best
#'
#' @return Corrected gas demand data with same structure
#' @keywords internal
.apply_eurostat_gas_correction <- function(gas_demand) {

  # Get Eurostat monthly consumption data
  eurostat <- get_eurostat_gas() %>%
    filter(type == 'consumption') %>%
    select(iso2, date, eurostat_m3 = value_m3)

  # Aggregate ENTSOG to monthly for comparison
  entsog_monthly <- gas_demand %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      month_date = lubridate::floor_date(date, 'month')
    ) %>%
    group_by(iso2, year, month, month_date) %>%
    summarise(
      entsog_m3 = sum(value_m3, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    )

  # Join with Eurostat and calculate scaling factors
  monthly_factors <- entsog_monthly %>%
    left_join(
      eurostat %>% mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
      ) %>% select(iso2, year, month, eurostat_m3),
      by = c("iso2", "year", "month")
    ) %>%
    mutate(
      ratio = entsog_m3 / eurostat_m3,
      scale_factor = eurostat_m3 / entsog_m3,
      # Handle edge cases
      scale_factor = if_else(
        is.finite(scale_factor) & entsog_m3 > 0,
        scale_factor,
        NA_real_
      )
    )

  # For each country, get the last available scale factor to carry forward
  last_factors <- monthly_factors %>%
    filter(!is.na(scale_factor)) %>%
    group_by(iso2) %>%
    filter(month_date == max(month_date)) %>%
    ungroup() %>%
    select(iso2, last_scale_factor = scale_factor, last_eurostat_month = month_date)

  # Fill forward the scale factor for months without Eurostat data
  monthly_factors <- monthly_factors %>%
    left_join(last_factors, by = "iso2") %>%
    mutate(
      scale_factor_filled = if_else(
        is.na(scale_factor) & month_date > last_eurostat_month,
        last_scale_factor,
        scale_factor
      ),
      is_extrapolated = is.na(scale_factor) & !is.na(scale_factor_filled)
    )

  # Join scale factors back to daily data and apply correction
  result <- gas_demand %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    left_join(
      monthly_factors %>%
        select(iso2, year, month, scale_factor = scale_factor_filled,
               ratio, is_extrapolated),
      by = c("iso2", "year", "month")
    ) %>%
    mutate(
      value_m3_raw = value_m3,
      value_m3 = if_else(
        !is.na(scale_factor),
        value_m3_raw * scale_factor,
        value_m3_raw
      )
    ) %>%
    select(-year, -month)

  return(result)
}


#' Generate diagnostics for gas demand
#'
#' Creates a plot showing Eurostat vs CREA estimate. If correction was applied,
#' also shows the corrected values.
#'
#' @param gas_demand_raw Raw gas demand data (before correction)
#' @param gas_demand_corrected Corrected gas demand data (NULL if no correction)
#' @param diagnostics_folder Folder for diagnostics output
#'
#' @keywords internal
.generate_gas_diagnostics <- function(gas_demand_raw,
                                       gas_demand_corrected,
                                       diagnostics_folder) {

  message("Generating gas demand diagnostics...")
  create_dir(diagnostics_folder)

  # Get Eurostat data
 eurostat <- get_eurostat_gas() %>%
    filter(type == 'consumption')

  # Aggregate raw data to monthly
  raw_monthly <- gas_demand_raw %>%
    mutate(date = lubridate::floor_date(date, 'month')) %>%
    group_by(iso2, date) %>%
    summarise(value_m3 = sum(value_m3, na.rm = TRUE), .groups = "drop") %>%
    mutate(method = "CREA estimate")

  # Start with raw + eurostat
  plot_data <- bind_rows(
    raw_monthly,
    eurostat %>%
      filter(iso2 %in% unique(gas_demand_raw$iso2)) %>%
      select(iso2, date, value_m3) %>%
      mutate(method = "Eurostat")
  )

  # Add corrected if available
  if (!is.null(gas_demand_corrected)) {
    corrected_monthly <- gas_demand_corrected %>%
      mutate(date = lubridate::floor_date(date, 'month')) %>%
      group_by(iso2, date) %>%
      summarise(value_m3 = sum(value_m3, na.rm = TRUE), .groups = "drop") %>%
      mutate(method = "Corrected")

    plot_data <- bind_rows(plot_data, corrected_monthly)
    method_levels <- c("CREA estimate", "Corrected", "Eurostat")
    color_values <- c(
      "CREA estimate" = rcrea::pal_crea[["Dark.blue"]],
      "Corrected" = rcrea::pal_crea[["Turquoise"]],
      "Eurostat" = rcrea::pal_crea[["Red"]]
    )
    size_values <- c("CREA estimate" = 0.3, "Corrected" = 0.5, "Eurostat" = 0.3)
    subtitle <- "Million cubic meters per month. Corrected = scaled to match Eurostat."
  } else {
    method_levels <- c("CREA estimate", "Eurostat")
    color_values <- c(
      "CREA estimate" = rcrea::pal_crea[["Dark.blue"]],
      "Eurostat" = rcrea::pal_crea[["Red"]]
    )
    size_values <- c("CREA estimate" = 0.5, "Eurostat" = 0.3)
    subtitle <- "Million cubic meters per month"
  }

  plot_data <- plot_data %>%
    filter(iso2 %in% unique(gas_demand_raw$iso2)) %>%
    mutate(
      country = countrycode::countrycode(iso2, 'iso2c', 'country.name',
                                          custom_match = c('EU' = 'EU')),
      method = factor(method, levels = method_levels)
    )

  plt <- plot_data %>%
    filter(!is.na(value_m3)) %>%
    ggplot(aes(x = date, y = value_m3 / 1e6, color = method, size = method)) +
    geom_line() +
    facet_wrap(~country, scales = "free_y", ncol = 3) +
    rcrea::scale_y_crea_zero() +
    scale_color_manual(values = color_values) +
    scale_size_manual(values = size_values) +
    labs(
      title = "Fossil gas consumption - CREA estimate vs Eurostat",
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      color = NULL,
      size = NULL,
      caption = "Note: Only the best estimate is shown for each country."
    ) +
    rcrea::theme_crea() +
    theme(legend.position = "bottom") +
    guides(size = guide_legend(nrow = 1),
           color = guide_legend(nrow = 1))

  ggsave(
    file.path(diagnostics_folder, "gas_consumption_estimates.png"),
    plt, width = 10, height = 10, bg = "white"
  )

  message("Gas demand diagnostics saved to: ", diagnostics_folder)
}


