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
#' @param iso2s Character vector of ISO2 country codes to include. If NULL (default),
#'   returns all available countries.
#' @param date_to End date for data. If NULL (default), returns all available dates.
#' @param diagnostics_folder Folder path for diagnostics outputs. Set to NULL
#'   to disable diagnostics. Default is "diagnostics/gas_demand".
#' @param verbose Whether to print verbose output. Default is FALSE.
#' @param use_cache Whether to use cached data. Default is TRUE.
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
get_gas_demand <- function(
  iso2s = NULL,
  date_to = NULL,
  diagnostics_folder = "diagnostics/gas_demand",
  verbose = FALSE,
  use_cache = TRUE,
  correct_to_eurostat = FALSE,
  data_masking = NULL
) {
  years <- seq(2015, lubridate::year(lubridate::today()))

  if (!is.null(diagnostics_folder)) {
    create_dir(diagnostics_folder)
  }

  entsog_data <- gas_data_access_get_entsog_flow(
    years = years,
    use_cache = use_cache,
    verbose = verbose,
    data_masking = data_masking
  )

  agsi_storage_data <- gas_data_access_get_agsi_storage(
    date_from = min(as.Date(entsog_data$date)),
    date_to = max(as.Date(entsog_data$date)),
    iso2 = get_eu_iso2s(),
    verbose = verbose,
    data_masking = data_masking
  )

  # Estimate with two different methods
  message("Getting gas demand from Consumption + Distribution ENTSOG points")
  consdist <- get_gas_demand_consdist(entsog_data = entsog_data, years = years, verbose = verbose)
  message("Getting gas demand from storage,crossborder,production")
  apparent <- get_gas_demand_apparent(entsog_data = entsog_data, years = years, verbose = verbose)
  message("Getting gas demand from storage,crossborder,production using AGSI for storage")
  apparent_w_agsi <- get_gas_demand_apparent(
    entsog_data = entsog_data,
    years = years,
    use_agsi_for_storage = TRUE,
    verbose = verbose,
    agsi_storage_data = agsi_storage_data
  )


  # Keep the best ones, and only those that match quality criteria
  gas_demand <- keep_best(
    consumption = bind_rows(
      consdist,
      apparent,
      apparent_w_agsi
    ),
    min_comparison_points = 12,
    diagnostics_folder = diagnostics_folder,
    min_r2 = 0.95,
    max_rrse = 0.3,
    data_masking = data_masking
  )

  # Store raw gas demand for diagnostics
  gas_demand_raw <- gas_demand

  # Apply Eurostat correction if requested
  if (correct_to_eurostat) {
    message("Applying Eurostat correction to gas demand...")
    gas_demand <- .apply_eurostat_gas_correction(
      gas_demand = gas_demand,
      data_masking = data_masking
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
  result <- gas_demand %>%
    select(iso2, date, value = value_m3) %>%
    mutate(
      fuel = "fossil_gas",
      sector = "total",
      data_source = ifelse(correct_to_eurostat, "crea_eurostat_corrected", "crea"),
      unit = "m3",
      frequency = "daily",
      region_type = case_when(iso2 == "EU" ~ "region", T ~ "iso2")
    )

  # Apply filters if specified
  if (!is.null(iso2s)) {
    result <- result %>% filter(iso2 %in% iso2s)
  }
  if (!is.null(date_to)) {
    result <- result %>% filter(date <= as.Date(date_to))
  }

  return(result)
}


.summarise_masked_sum <- function(x, multiplier = 1) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  multiplier * sum(x, na.rm = TRUE)
}


.interpolate_unobserved_gaps <- function(x, preserve_na = NULL) {
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }

  if (is.null(preserve_na)) {
    preserve_na <- rep(FALSE, length(x))
  }
  preserve_na[is.na(preserve_na)] <- FALSE

  interpolated <- zoo::na.approx(x, na.rm = FALSE)
  interpolated[preserve_na] <- NA_real_
  interpolated
}


#' Get national gas demand based on Consumption + Distribution ENTSOG points
#'
#' @param years
#'
#' @return
#' @export
#'
#' @examples
get_gas_demand_consdist <- function(entsog_data, years, verbose = FALSE) {
  entsog <- entsog_data %>%
    filter(type %in% c("consumption", "distribution"))

  consdist <- entsog %>%
    group_by(iso2 = destination_iso2, date) %>%
    summarise(
      value_m3 = .summarise_masked_sum(value_m3, multiplier = -1), .groups =
        "drop_last"
    ) %>%
    arrange(date) %>%
    mutate(preserve_na = is.na(value_m3)) %>%
    # Interpolate missing data
    tidyr::complete(
      date = seq.Date(min(as.Date(date)),
        max(as.Date(date)),
        by = "day"
      ),
      fill = list(value_m3 = NA_real_, preserve_na = FALSE)
    ) %>%
    mutate(value_m3 = .interpolate_unobserved_gaps(value_m3, preserve_na)) %>%
    select(-preserve_na) %>%
    ungroup() %>%
    mutate(has_source_row = TRUE) %>%
    # Fill with 0 before and after each country's reporting period
    group_by(iso2) %>%
    tidyr::complete(
      date = seq.Date(min(as.Date(entsog$date)),
        max(as.Date(entsog$date)),
        by = "day"
      ),
      fill = list(value_m3 = NA_real_, has_source_row = FALSE)
    ) %>%
    mutate(value_m3 = ifelse(!has_source_row & is.na(value_m3), 0, value_m3)) %>%
    ungroup() %>%
    select(-has_source_row) %>%
    mutate(method = "consdist")

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
get_gas_demand_apparent <- function(
  entsog_data,
  years,
  use_agsi_for_storage = FALSE,
  verbose = FALSE,
  agsi_storage_data = NULL
) {
  entsog <- entsog_data %>%
    filter(type %in% c("storage", "crossborder", "production"))

  # Fill missing data
  entsog <- entsog %>%
    select(destination_iso2, departure_iso2, date, type, value_m3) %>%
    arrange(date) %>%
    group_by(destination_iso2, departure_iso2, type) %>%
    mutate(preserve_na = is.na(value_m3)) %>%
    # Interpolate missing data
    tidyr::complete(
      date = seq.Date(min(as.Date(date)),
        max(as.Date(date)),
        by = "day"
      ),
      fill = list(value_m3 = NA_real_, preserve_na = FALSE)
    ) %>%
    mutate(value_m3 = .interpolate_unobserved_gaps(value_m3, preserve_na)) %>%
    select(-preserve_na) %>%
    ungroup()


  if (use_agsi_for_storage) {
    # In some instances, AGSI is better than ENTSOG
    # Well, at least for Austria which has no storage data in ENTSOG
    if (is.null(agsi_storage_data)) {
      stop("agsi_storage_data is required when use_agsi_for_storage is TRUE")
    }
    storage_drawdown <- agsi_storage_data
    entsog <- entsog %>%
      filter(type != "storage") %>%
      bind_rows(
        storage_drawdown %>%
          select(
            destination_iso2 = iso2,
            date,
            value_m3
          ) %>%
          mutate(type = "storage")
      )
  }


  # We're missing some flows within EU, so we take at the borders only
  # i.e. not doing this: entsog_eu <- entsog %>%
  #   mutate(destination_iso2=destination_region,
  #          departure_iso2=departure_region)
  ex_eu <- c(
    "NO", "GB", "UK", "TR", "lng",
    "UA", "RU", "AZ", "DZ", "LY", "BY", "MA"
  )
  entsog_eu <- entsog %>%
    mutate(
      destination_iso2 = case_when(
        destination_iso2 %in% ex_eu ~ "Others",
        T ~ "EU"
      ),
      departure_iso2 = case_when(
        departure_iso2 %in% ex_eu ~ "Others",
        T ~ "EU"
      )
    )

  entsog_all <- bind_rows(entsog, entsog_eu)

  # Implied consumption = net imports + production + storage_drawdown
  imports <- entsog_all %>%
    filter(type %in% c("crossborder")) %>%
    filter(departure_iso2 != destination_iso2) %>%
    group_by(destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(type = "imports")

  minus_exports <- entsog_all %>%
    filter(type %in% c("crossborder")) %>%
    filter(departure_iso2 != destination_iso2) %>%
    mutate(
      tmp = departure_iso2,
      departure_iso2 = destination_iso2,
      destination_iso2 = tmp,
      value_m3 = -value_m3
    ) %>%
    group_by(destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(type = "minus_exports")

  net_imports <- bind_rows(imports, minus_exports) %>%
    group_by(destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(type = "net_imports")

  storage_drawdown <- entsog_all %>%
    filter(type %in% c("storage")) %>%
    group_by(destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(type = "storage_drawdown")

  production <- entsog_all %>%
    filter(type == "production") %>%
    group_by(destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(type = "production")

  apparent <- bind_rows(imports, minus_exports, storage_drawdown, production) %>%
    group_by(iso2 = destination_iso2, date) %>%
    summarise(value_m3 = .summarise_masked_sum(value_m3), .groups = "drop") %>%
    mutate(has_source_row = TRUE) %>%
    group_by(iso2) %>%
    tidyr::complete(
      date = seq.Date(min(as.Date(entsog$date)), max(as.Date(entsog$date)), by = "day"),
      fill = list(value_m3 = NA_real_, has_source_row = FALSE)
    ) %>%
    mutate(value_m3 = ifelse(!has_source_row & is.na(value_m3), 0, value_m3)) %>%
    ungroup() %>%
    select(-has_source_row) %>%
    mutate(method = ifelse(use_agsi_for_storage, "apparent_agsi", "apparent"))

  return(apparent)
}


get_eurostat_gas <- function(years = NULL, data_masking = NULL) {
  gas_data_access_get_eurostat_monthly_for_correction(data_masking = data_masking)
}


keep_best <- function(
  consumption,
  diagnostics_folder,
  min_comparison_points = 24,
  min_r2 = 0.95, max_rrse = 0.4,
  data_masking = NULL
) {
  eurostat <- get_eurostat_gas(data_masking = data_masking) %>% filter(type == "consumption")
  rsq <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]

    if (length(x) < 2 || stats::sd(x) == 0 || stats::sd(y) == 0) {
      return(NA_real_)
    }

    stats::cor(x, y)^2
  }
  min_start <- min(consumption$date)
  max_start <- max(eurostat$date) - months(min_comparison_points - 1)

  # We test the correlation for several starting dates
  # The older the better, but often it becomes accurate only after a certain date
  date_froms <- seq.Date(as.Date(min_start), as.Date(max_start), by = "month")
  consumption_date_to <- consumption %>%
    group_by(iso2, method) %>%
    summarise(date_to = max(date))

  consumption_monthly <- consumption %>%
    filter(date < max(lubridate::floor_date(date, "month"))) %>%
    group_by(iso2, method, date = lubridate::floor_date(date, "month")) %>%
    summarise(value_m3 = sum(value_m3)) %>%
    ungroup() %>%
    tidyr::complete(date, method, iso2, fill = list(value_m3 = 0))

  bests <- pbapply::pblapply(date_froms, function(date_from) {
    consumption_monthly %>%
      filter(date >= date_from) %>%
      left_join(
        eurostat %>%
          rename(value_m3_eurostat = value_m3) %>%
          select(-c(method))
      ) %>%
      filter(!is.na(value_m3_eurostat)) %>%
      group_by(iso2, method) %>%
      summarise(
        r2 = rsq(value_m3_eurostat, value_m3),
        rrse = Metrics::rrse(value_m3_eurostat, value_m3),
        count = n()
      ) %>%
      mutate(valid = (rrse < max_rrse) & (r2 > min_r2)) %>%
      group_by(iso2) %>%
      top_n(n = 1, wt = r2) %>%
      ungroup() %>%
      arrange(desc(r2)) %>%
      mutate(date_from = !!date_from)
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
    filter(date_from == min(date_from)) %>%
    filter(r2 == max(r2)) %>%
    # Sometimes apparent strictly equivalent to apparent_asgi
    distinct(iso2, .keep_all = TRUE)

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
.apply_eurostat_gas_correction <- function(gas_demand, data_masking = NULL) {
  # Get Eurostat monthly consumption data
  eurostat <- get_eurostat_gas(data_masking = data_masking) %>%
    filter(type == "consumption") %>%
    select(iso2, date, eurostat_m3 = value_m3)

  # Aggregate ENTSOG to monthly for comparison
  entsog_monthly <- gas_demand %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      month_date = lubridate::floor_date(date, "month")
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
      ) %>%
        select(iso2, year, month, eurostat_m3),
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
        select(
          iso2, year, month,
          scale_factor = scale_factor_filled,
          ratio, is_extrapolated
        ),
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
.generate_gas_diagnostics <- function(
  gas_demand_raw,
  gas_demand_corrected,
  diagnostics_folder
) {
  message("Generating gas demand diagnostics...")
  create_dir(diagnostics_folder)

  # Get Eurostat data
  eurostat <- get_eurostat_gas() %>%
    filter(type == "consumption")

  # Aggregate raw data to monthly
  raw_monthly <- gas_demand_raw %>%
    mutate(date = lubridate::floor_date(date, "month")) %>%
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
      mutate(date = lubridate::floor_date(date, "month")) %>%
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
      country = countrycode::countrycode(
        iso2, "iso2c", "country.name",
        custom_match = c("EU" = "EU")
      ),
      method = factor(method, levels = method_levels)
    )

  plt <- plot_data %>%
    filter(!is.na(value_m3)) %>%
    ggplot(aes(x = date, y = value_m3 / 1e6, color = method, linewidth = method)) +
    geom_line() +
    facet_wrap(~country, scales = "free_y", ncol = 3) +
    rcrea::scale_y_crea_zero() +
    scale_color_manual(values = color_values) +
    scale_linewidth_manual(values = size_values) +
    labs(
      title = "Fossil gas consumption - CREA estimate vs Eurostat",
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      color = NULL,
      linewidth = NULL,
      caption = "Note: Only the best estimate is shown for each country."
    ) +
    rcrea::theme_crea() +
    theme(legend.position = "bottom") +
    guides(linewidth = guide_legend(nrow = 1), color = guide_legend(nrow = 1))

  ggsave(
    file.path(diagnostics_folder, "gas_consumption_estimates.png"),
    plt,
    width = 10, height = 10, bg = "white"
  )

  message("Gas demand diagnostics saved to: ", diagnostics_folder)
}
