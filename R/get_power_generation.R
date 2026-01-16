#' Get Power Generation Data Combining ENTSOE and EMBER
#'
#' Retrieves daily power generation data by combining ENTSOE (daily granularity)
#' with EMBER (monthly, more accurate) using a two-tier approach:
#' - Tier 1 (ratio 0.7-1.3): Scale ENTSOE daily data by monthly EMBER/ENTSOE ratio
#' - Tier 2 (ratio <0.7 or >1.3): Use EMBER monthly data, distributed across days
#'   using ENTSOE's daily pattern as a proxy
#'
#' @param iso2s Character vector of ISO2 country codes. Default is "EU" which
#'   fetches all EU countries.
#' @param date_from Start date for data retrieval. Default is "2015-01-01".
#' @param date_to End date for data retrieval. Default is today.
#' @param corrected_sources Character vector of power sources to correct using
#'   EMBER data. Uses ENTSOE naming: "Wind", "Solar", "Fossil Gas", "Hydro".
#'   Default is c("Wind", "Solar", "Fossil Gas", "Hydro"). Other sources from
#'   ENTSOE are passed through unchanged.
#' @param use_cache Whether to use cached data. Default is TRUE.
#' @param tier_threshold Numeric vector of length 2 defining the ratio bounds
#'   for Tier 1 vs Tier 2. Default is c(0.7, 1.3).
#' @param replace_entsoe_others_with_ember Whether to replace ENTSOE "Other"
#'   category with EMBER-only sources (Bioenergy, Other renewables, Other fossil).
#'   When TRUE, removes ENTSOE "Other" and adds these EMBER sources distributed
#'   to daily assuming constant production within each month. Default is TRUE.
#' @param diagnostics_folder Folder path for diagnostics outputs. Set to NULL
#'   to disable diagnostics. Default is "diagnostics/power_generation".
#'
#' @return Tibble with columns:
#'   - iso2: Country code
#'   - date: Date (daily)
#'   - source: Power source (Wind, Solar, Gas)
#'   - value_mwh: Generation in MWh (corrected)
#'   - value_mwh_raw: Original ENTSOE value
#'   - correction_tier: Which correction method was applied (1 or 2)
#'   - correction_ratio: The ENTSOE/EMBER ratio used for correction
#'   - data_source: "entsoe_ember_combined"
#'
#' @details
#' The function addresses known discrepancies between ENTSOE and EMBER data:
#' - ENTSOE provides daily data but has reporting gaps/errors in some countries

#' - EMBER provides monthly data that is more thoroughly validated
#'
#' For Tier 1 countries (reasonable alignment), daily ENTSOE values are scaled
#' so monthly totals match EMBER. For Tier 2 countries (poor alignment), EMBER
#' monthly totals are distributed across days using ENTSOE's relative daily
#' pattern within each month.
#'
#' For dates beyond the last available EMBER month, the most recent month's
#' ratio is used for Tier 1, and ENTSOE pattern with last ratio for Tier 2.
#'
#' @export
get_power_generation <- function(iso2s = get_eu_iso2s(include_eu = TRUE),
                                 date_from = "2015-01-01",
                                 date_to = Sys.Date(),
                                 corrected_sources = c("Wind", "Solar", "Fossil Gas", "Hydro"),
                                 use_cache = TRUE,
                                 tier_threshold = c(0.7, 1.3),
                                 replace_entsoe_others_with_ember = TRUE,
                                 diagnostics_folder = "diagnostics/power_generation") {

  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  # Generate a hash of the parameters for caching
  param_hash <- digest::digest(list(
    iso2s = sort(iso2s),
    date_from = as.character(date_from),
    date_to = as.character(date_to),
    corrected_sources = sort(corrected_sources),
    tier_threshold = tier_threshold,
    replace_entsoe_others_with_ember = replace_entsoe_others_with_ember,
    diagnostics_folder = diagnostics_folder
  ))

  # Create a filename using the hash
  cache_dir <- "cache"
  filepath <- file.path(cache_dir, paste0("power_generation_", param_hash, ".RDS"))

  # Check if the file exists in cache and use_cache is TRUE
  if (use_cache && file.exists(filepath)) {
    message("Loading cached power generation data...")
    return(readRDS(filepath))
  }

  # For tier calculation, we need historical data from 2020+
  # Always fetch at least from 2020-01-01 for proper tier assignment
  tier_calc_start <- as.Date("2020-01-01")
  fetch_from <- min(date_from, tier_calc_start)

  # Fetch ENTSOE daily data
  message("Fetching ENTSOE daily data...")
  entsoe_daily_full <- entsoe.get_power_generation(
    date_from = fetch_from,
    date_to = date_to,
    iso2s = iso2s,
    use_cache = use_cache
  ) %>% filter(iso2 %in% iso2s)

  # Fetch EMBER monthly data
  message("Fetching EMBER monthly data...")
  ember_monthly_raw <- ember.get_power_generation(
    frequency = "monthly",
    iso2s = iso2s,
    use_cache = use_cache
  )

  # Fetch EMBER yearly data to fill gaps where monthly is incomplete
  message("Fetching EMBER yearly data...")
  ember_yearly <- ember.get_power_generation(
    frequency = "yearly",
    iso2s = iso2s,
    use_cache = use_cache
  )

  # Fill gaps in monthly data using yearly data
  ember_monthly <- .fill_ember_monthly_from_yearly(
    ember_monthly = ember_monthly_raw,
    ember_yearly = ember_yearly,
    entsoe_daily = entsoe_daily_full
  )

  # Use full data for tier calculation, but filter for output
  entsoe_daily <- entsoe_daily_full

  # Map ENTSOE source names to EMBER equivalents for comparison
  source_mapping <- tibble::tribble(
    ~entsoe_source,   ~ember_source,
    "Wind",           "Wind",
    "Wind Onshore",   "Wind",
    "Wind Offshore",  "Wind",
    "Solar",          "Solar",
    "Fossil Gas",     "Gas",
    "Hydro",          "Hydro"
  )

  # EMBER-only sources (not in ENTSOE or replacing ENTSOE "Other")
  ember_only_sources <- c("Bioenergy", "Other renewables", "Other fossil")

  # ENTSOE source to exclude when replacing with EMBER
  entsoe_others_to_replace <- "Other"

  # Determine which EMBER sources we need
  ember_sources_needed <- source_mapping %>%
    filter(entsoe_source %in% corrected_sources) %>%
    pull(ember_source) %>%
    unique()

  # Split ENTSOE data into corrected vs pass-through sources
  entsoe_to_correct <- entsoe_daily %>%
    filter(source %in% corrected_sources | source %in% c("Wind Onshore", "Wind Offshore")) %>%
    mutate(
      source_std = case_when(
        source %in% c("Wind Onshore", "Wind Offshore", "Wind") ~ "Wind",
        source == "Fossil Gas" ~ "Gas",
        TRUE ~ source
      )
    )

  entsoe_passthrough <- entsoe_daily %>%
    filter(!(source %in% corrected_sources | source %in% c("Wind Onshore", "Wind Offshore"))) %>%
    filter(source != "Total")  # Don't pass through Total, we'll recalculate

  # If replacing ENTSOE others with EMBER, filter them out from passthrough
  if (replace_entsoe_others_with_ember) {
    entsoe_passthrough <- entsoe_passthrough %>%
      filter(!(source %in% entsoe_others_to_replace))
  }

  # Prepare EMBER monthly for corrected sources
  ember_monthly_for_correction <- ember_monthly %>%
    filter(source %in% ember_sources_needed) %>%
    rename(source_std = source)

  # Calculate monthly scaling factors (using data to be corrected)
  scaling_factors <- .calculate_monthly_scaling_factors(
    entsoe_daily = entsoe_to_correct,
    ember_monthly = ember_monthly_for_correction,
    tier_threshold = tier_threshold
  )

  # Apply corrections to sources that need correction
  result_corrected <- .apply_power_corrections(
    entsoe_daily = entsoe_to_correct,
    ember_monthly = ember_monthly_for_correction,
    scaling_factors = scaling_factors,
    date_from = date_from,
    date_to = date_to
  )

  # Prepare passthrough sources (no correction, just format matching)
  result_passthrough <- entsoe_passthrough %>%
    filter(date >= date_from) %>%
    mutate(
      value_mw_raw = value_mw,
      value_mwh_raw = value_mwh,
      correction_tier = NA_integer_,
      correction_ratio = NA_real_,
      data_source = "entsoe"  # Keep original data_source for passthrough
    ) %>%
    select(
      date, source, data_source, iso2, region, country,
      value_mw, value_mwh, frequency,
      value_mw_raw, value_mwh_raw, correction_tier, correction_ratio
    )

  # Add EMBER-only sources if replacing ENTSOE others
  result_ember_only <- NULL
  if (replace_entsoe_others_with_ember) {
    result_ember_only <- .distribute_ember_to_daily(
      ember_monthly = ember_monthly,
      ember_only_sources = ember_only_sources,
      date_from = date_from,
      date_to = date_to,
      iso2s = iso2s,
      entsoe_daily = entsoe_daily_full  # For getting region/country info
    )
  }

  # Combine corrected, passthrough, and EMBER-only results
  result <- bind_rows(result_corrected, result_passthrough, result_ember_only) %>%
    arrange(iso2, date, source)

  # Recalculate Total if it was in ENTSOE originally
  if (any(entsoe_daily_full$source == "Total", na.rm = TRUE)) {
    total_recalculated <- result %>%
      filter(source != "Total") %>%  # Exclude any existing Total
      group_by(iso2, region, country, date) %>%
      summarise(
        value_mw = sum(value_mw, na.rm = TRUE),
        value_mwh = sum(value_mwh, na.rm = TRUE),
        value_mw_raw = sum(value_mw_raw, na.rm = TRUE),
        value_mwh_raw = sum(value_mwh_raw, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        source = "Total",
        data_source = "entsoe_ember_combined",
        correction_tier = NA_integer_,
        correction_ratio = NA_real_
      ) %>%
      select(
        date, source, data_source, iso2, region, country,
        value_mw, value_mwh,
        value_mw_raw, value_mwh_raw, correction_tier, correction_ratio
      )

    result <- bind_rows(result, total_recalculated) %>%
      arrange(iso2, date, source)
  }

  # Generate diagnostics
  if (!is.null(diagnostics_folder)) {
    .generate_power_diagnostics(
      entsoe_daily = entsoe_to_correct,
      ember_monthly = ember_monthly_for_correction,
      scaling_factors = scaling_factors,
      result = result,  # Pass all results for complete diagnostics
      diagnostics_folder = diagnostics_folder
    )
  }

  # Only select useful columns
  result <- result %>%
    select(iso2, country, region, date, source, value_mw, value_mwh)


  # Check that iso2-fuel-date is unique
  if (any(duplicated(result %>% select(iso2, source, date)))) {
    stop("Duplicate iso2-source-date combinations found in the result")
  }

  # Save the fetched data to cache
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  saveRDS(result, filepath)

  return(result)
}


#' Calculate monthly scaling factors from ENTSOE and EMBER comparison
#' @keywords internal
.calculate_monthly_scaling_factors <- function(entsoe_daily,
                                               ember_monthly,
                                               tier_threshold) {

  # Aggregate ENTSOE to monthly
  entsoe_monthly <- entsoe_daily %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    group_by(iso2, year, month, source_std) %>%
    summarise(
      entsoe_mwh = sum(value_mwh, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    )

  # Prepare EMBER monthly
  ember_monthly_prep <- ember_monthly %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    select(iso2, year, month, source_std, ember_mwh = value_mwh)


  # Only include complete months for ratio calculation
  # This avoids issues with partial months at the end of the date range
  monthly_comparison <- entsoe_monthly %>%
    filter(n_days == lubridate::days_in_month(glue("{year}-{month}-01"))) %>%
    inner_join(ember_monthly_prep, by = c("iso2", "year", "month", "source_std")) %>%
    mutate(
      ratio = entsoe_mwh / ember_mwh,
      ratio = if_else(is.finite(ratio) & ember_mwh > 0, ratio, NA_real_)
    )

  # Determine tier based on median ratio per country/source
  # Use only recent years (2020+) to avoid legacy data quality issues
  tier_assignment <- monthly_comparison %>%
    filter(!is.na(ratio), year >= 2020) %>%
    group_by(iso2, source_std) %>%
    summarise(
      median_ratio = median(ratio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tier = if_else(
        median_ratio >= tier_threshold[1] & median_ratio <= tier_threshold[2],
        1L, 2L
      )
    )

  # Add tier and calculate scaling factor
  scaling_factors <- monthly_comparison %>%
    left_join(tier_assignment %>% select(iso2, source_std, tier),
              by = c("iso2", "source_std")) %>%
    mutate(
      # For Tier 1: scale_factor to multiply ENTSOE by
      # For Tier 2: we'll use EMBER directly, but still track ratio
      scale_factor = if_else(!is.na(ratio) & ratio > 0, 1 / ratio, NA_real_)
    )

  return(scaling_factors)
}


#' Apply power generation corrections based on tier
#' @keywords internal
.apply_power_corrections <- function(entsoe_daily,
                                     ember_monthly,
                                     scaling_factors,
                                     date_from,
                                     date_to) {

  # Get tier assignment for each country/source
  tier_assignment <- scaling_factors %>%
    select(iso2, source_std, tier) %>%
    distinct()

  # Get latest available EMBER month

  last_ember_month <- max(ember_monthly$date, na.rm = TRUE)
  last_ember_year <- year(last_ember_month)
  last_ember_month_num <- month(last_ember_month)

  # Add year/month to daily data
  entsoe_daily <- entsoe_daily %>%
    mutate(
      year = year(date),
      month = month(date)
    )

  # Join with scaling factors
  daily_with_factors <- entsoe_daily %>%
    left_join(
      scaling_factors %>%
        select(iso2, year, month, source_std, ratio, scale_factor, tier, ember_mwh = ember_mwh),
      by = c("iso2", "year", "month", "source_std")
    )

  # For months beyond EMBER coverage, use last available ratio
  last_ratios <- scaling_factors %>%
    filter(year == last_ember_year, month == last_ember_month_num) %>%
    select(iso2, source_std, last_ratio = ratio, last_scale_factor = scale_factor, tier)

  daily_with_factors <- daily_with_factors %>%
    left_join(last_ratios %>% select(-tier), by = c("iso2", "source_std")) %>%
    mutate(
      # Use last ratio for future months
      ratio = coalesce(ratio, last_ratio),
      scale_factor = coalesce(scale_factor, last_scale_factor)
    ) %>%
    select(-last_ratio, -last_scale_factor)

  # Apply Tier 1: Scale ENTSOE by monthly factor
  # Apply Tier 2: Distribute EMBER monthly using ENTSOE daily pattern

  # First, calculate daily share within each month for Tier 2
  # Also track if ENTSOE has valid (non-NA) data for the month
  daily_shares <- entsoe_daily %>%
    group_by(iso2, source_std, year, month) %>%
    mutate(
      monthly_total = sum(value_mwh, na.rm = TRUE),
      n_valid_days = sum(!is.na(value_mwh)),
      # If ENTSOE has valid data, use its pattern; otherwise use equal distribution
      daily_share = case_when(
        monthly_total > 0 & !is.na(value_mwh) ~ value_mwh / monthly_total,
        n_valid_days == 0 ~ 1 / n(),  # All NA: use equal distribution
        TRUE ~ 0  # This day is NA but others aren't - give it 0 share
      ),
      entsoe_all_na = (n_valid_days == 0)
    ) %>%
    ungroup() %>%
    select(iso2, date, source_std, daily_share, monthly_total, entsoe_all_na)

  # Join daily shares
  daily_with_factors <- daily_with_factors %>%
    left_join(
      daily_shares %>% select(iso2, date, source_std, daily_share, entsoe_all_na),
      by = c("iso2", "date", "source_std")
    )

  # Apply corrections

  result <- daily_with_factors %>%
    mutate(
      # Store raw values before correction
      value_mwh_raw = value_mwh,
      value_mw_raw = value_mw,
      # Calculate days in month for constant distribution fallback
      days_in_month = lubridate::days_in_month(date),
      # Apply corrections
      value_mwh = case_when(
        # Tier 1: Scale ENTSOE by monthly factor (only if ENTSOE has data)
        tier == 1 & !is.na(scale_factor) & !is.na(value_mwh_raw) & !entsoe_all_na ~ value_mwh_raw * scale_factor,
        # Tier 1 but ENTSOE is all NA for month: use EMBER with constant distribution
        tier == 1 & !is.na(ember_mwh) & entsoe_all_na ~ ember_mwh / days_in_month,
        # Tier 2: Distribute EMBER monthly by daily share
        tier == 2 & !is.na(ember_mwh) & !is.na(daily_share) & !entsoe_all_na ~ ember_mwh * daily_share,
        # Tier 2 but ENTSOE is all NA for month: use EMBER with constant distribution
        tier == 2 & !is.na(ember_mwh) & entsoe_all_na ~ ember_mwh / days_in_month,
        # Fallback: use raw ENTSOE if no correction available
        TRUE ~ value_mwh_raw
      ),
      # Calculate corrected value_mw (assuming 24 hours per day)
      value_mw = value_mwh / 24,
      # Map source back to ENTSOE naming
      source = case_when(
        source_std == "Gas" ~ "Fossil Gas",
        TRUE ~ source_std
      ),
      data_source = "entsoe_ember_combined",
      correction_tier = tier,
      correction_ratio = ratio
    ) %>%
    # Match ENTSOE output format with additional correction columns
    select(
      date,
      source,
      data_source,
      iso2,
      region,
      country,
      value_mw,
      value_mwh,
      frequency,
      # Additional columns for transparency
      value_mw_raw,
      value_mwh_raw,
      correction_tier,
      correction_ratio
    ) %>%
    # Filter to requested date range
    filter(date >= date_from, date <= date_to)

  return(result)
}


#' Fill gaps in EMBER monthly data using yearly data
#'
#' For countries/sources where monthly data is incomplete or starts later than
#' yearly data, this function distributes yearly totals across months using
#' ENTSOE daily patterns as a proxy.
#'
#' @param ember_monthly EMBER monthly data
#' @param ember_yearly EMBER yearly data
#' @param entsoe_daily ENTSOE daily data (used for distribution pattern)
#'
#' @return EMBER monthly data with gaps filled from yearly
#' @keywords internal
.fill_ember_monthly_from_yearly <- function(ember_monthly, ember_yearly, entsoe_daily) {

  # Identify which year/country/source combinations have monthly data
  monthly_coverage <- ember_monthly %>%
    mutate(year = year(date)) %>%
    group_by(iso2, source, year) %>%
    summarise(
      n_months = n(),
      monthly_mwh = sum(value_mwh, na.rm = TRUE),
      .groups = "drop"
    )

  # Prepare yearly data
  yearly_data <- ember_yearly %>%
    mutate(year = year(date)) %>%
    select(iso2, source, year, yearly_mwh = value_mwh)

  # Identify gaps: years with yearly data but incomplete/missing monthly data
  gaps <- yearly_data %>%
    left_join(monthly_coverage, by = c("iso2", "source", "year")) %>%
    mutate(
      n_months = coalesce(n_months, 0L),
      monthly_mwh = coalesce(monthly_mwh, 0)
    ) %>%
    # Only fill if monthly data is missing or significantly incomplete
    # Consider incomplete if <10 months or monthly sum is <80% of yearly
    filter(
      n_months < 10 |
      (yearly_mwh > 0 & monthly_mwh / yearly_mwh < 0.8)
    )

  if (nrow(gaps) == 0) {
    message("No gaps in EMBER monthly data to fill from yearly")
    return(ember_monthly)
  }

  message(sprintf("Filling %d year/country/source gaps in EMBER monthly from yearly data",
                  nrow(gaps)))

  # Prepare ENTSOE monthly patterns for distribution
  entsoe_monthly_pattern <- entsoe_daily %>%
    mutate(
      year = year(date),
      month = month(date),
      source_std = case_when(
        source %in% c("Wind Onshore", "Wind Offshore", "Wind") ~ "Wind",
        source == "Fossil Gas" ~ "Gas",
        TRUE ~ source
      )
    ) %>%
    group_by(iso2, source_std, year, month) %>%
    summarise(entsoe_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    # Calculate monthly share within each year
    group_by(iso2, source_std, year) %>%
    mutate(
      yearly_total = sum(entsoe_mwh, na.rm = TRUE),
      monthly_share = if_else(yearly_total > 0, entsoe_mwh / yearly_total, 1/12)
    ) %>%
    ungroup() %>%
    select(iso2, source_std, year, month, monthly_share)

  # Generate filled monthly data for gaps
  filled_monthly <- gaps %>%
    # Cross join with months 1-12
    crossing(month = 1:12) %>%
    # Join with ENTSOE pattern
    left_join(
      entsoe_monthly_pattern,
      by = c("iso2", "source" = "source_std", "year", "month")
    ) %>%
    # If no ENTSOE pattern available, use equal distribution
    mutate(monthly_share = coalesce(monthly_share, 1/12)) %>%
    # Calculate monthly value from yearly
    mutate(
      value_mwh = yearly_mwh * monthly_share,
      date = ymd(paste(year, month, "01", sep = "-")),
      filled_from_yearly = TRUE
    ) %>%
    select(iso2, source, date, value_mwh, filled_from_yearly)

  # Remove existing monthly data for gaps (will be replaced)
  gap_keys <- gaps %>%
    select(iso2, source, year)

  ember_monthly_cleaned <- ember_monthly %>%
    mutate(year = year(date)) %>%
    anti_join(gap_keys, by = c("iso2", "source", "year")) %>%
    select(-year) %>%
    mutate(filled_from_yearly = FALSE)

  # Combine original (cleaned) with filled data
  result <- bind_rows(ember_monthly_cleaned, filled_monthly) %>%
    arrange(iso2, source, date)

  n_filled <- sum(result$filled_from_yearly, na.rm = TRUE)
  message(sprintf("Filled %d monthly observations from yearly data", n_filled))

  return(result)
}


#' Distribute EMBER monthly data to daily for EMBER-only sources
#'
#' For sources that only exist in EMBER (Bioenergy, Other renewables, Other fossil),
#' distribute monthly values to daily assuming constant production within each month.
#' For months beyond EMBER coverage, the last available monthly value is carried forward.
#'
#' @param ember_monthly EMBER monthly data
#' @param ember_only_sources Character vector of EMBER source names to distribute
#' @param date_from Start date
#' @param date_to End date
#' @param iso2s Country codes to include
#' @param entsoe_daily ENTSOE daily data (for getting region/country metadata)
#'
#' @return Daily data in the same format as result_corrected/result_passthrough
#' @keywords internal
.distribute_ember_to_daily <- function(ember_monthly,
                                        ember_only_sources,
                                        date_from,
                                        date_to,
                                        iso2s,
                                        entsoe_daily) {

  # Filter EMBER to only the sources we want
  ember_subset <- ember_monthly %>%
    filter(source %in% ember_only_sources, iso2 %in% iso2s)

  if (nrow(ember_subset) == 0) {
    message("No EMBER-only sources found for the requested countries")
    return(NULL)
  }

  # Get region/country mapping from ENTSOE
  country_info <- entsoe_daily %>%
    select(iso2, region, country) %>%
    distinct()

  # Extend EMBER data forward to date_to by carrying forward last monthly value
  # Get the last available month for each iso2/source
  last_ember_values <- ember_subset %>%
    group_by(iso2, source) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(iso2, source, last_value_mwh = value_mwh, last_date = date)

  # Generate months to fill (from last EMBER month + 1 to date_to)
  months_to_fill <- last_ember_values %>%
    rowwise() %>%
    reframe(
      iso2 = iso2,
      source = source,
      last_value_mwh = last_value_mwh,
      date = seq.Date(
        from = lubridate::ceiling_date(last_date, "month"),
        to = lubridate::floor_date(as.Date(date_to), "month"),
        by = "month"
      )
    ) %>%
    filter(date <= date_to) %>%
    rename(value_mwh = last_value_mwh)

  # Combine original EMBER data with extended data
  ember_extended <- bind_rows(
    ember_subset %>% select(iso2, source, date, value_mwh),
    months_to_fill
  ) %>%
    distinct(iso2, source, date, .keep_all = TRUE)

  # Generate daily dates for each month
  # For each monthly value, distribute evenly across days in that month
  daily_data <- ember_extended %>%
    mutate(
      year = year(date),
      month = month(date),
      days_in_month = lubridate::days_in_month(date)
    ) %>%
    # Create daily value (constant within month)
    mutate(
      daily_mwh = value_mwh / days_in_month,
      daily_mw = daily_mwh / 24
    ) %>%
    # Expand to daily rows
    group_by(iso2, source, year, month) %>%
    reframe(
      date = seq.Date(
        from = lubridate::floor_date(date, "month"),
        to = lubridate::ceiling_date(date, "month") - days(1),
        by = "day"
      ),
      value_mwh = daily_mwh,
      value_mw = daily_mw
    ) %>%
    ungroup() %>%
    # Filter to requested date range
    filter(date >= date_from, date <= date_to) %>%
    # Add metadata
    left_join(country_info, by = "iso2") %>%
    mutate(
      value_mw_raw = value_mw,
      value_mwh_raw = value_mwh,
      correction_tier = NA_integer_,
      correction_ratio = NA_real_,
      data_source = "ember",
      frequency = "daily"
    ) %>%
    select(
      date, source, data_source, iso2, region, country,
      value_mw, value_mwh, frequency,
      value_mw_raw, value_mwh_raw, correction_tier, correction_ratio
    )

  message(sprintf("Added %d daily observations from EMBER-only sources (%s)",
                  nrow(daily_data),
                  paste(ember_only_sources, collapse = ", ")))

  return(daily_data)
}


#' Get the tier assignment for each country/source combination
#'
#' Utility function to see which countries/sources are in Tier 1 vs Tier 2
#'
#' @param tier_threshold Numeric vector of length 2 defining ratio bounds
#' @param use_cache Whether to use cached data
#'
#' @return Tibble with tier assignments and median ratios
#'
#' @export
get_power_generation_tiers <- function(tier_threshold = c(0.7, 1.3),
                                       use_cache = TRUE,
                                       tier_calc_start = "2020-01-01") {

  iso2s <- get_eu_iso2s(include_eu = FALSE)

  # Fetch data - use tier_calc_start for tier calculation
  entsoe_daily <- entsoe.get_power_generation(
    date_from = tier_calc_start,
    iso2s = iso2s,
    use_cache = use_cache
  )

  ember_monthly <- ember.get_power_generation(
    frequency = "monthly",
    iso2s = iso2s,
    use_cache = use_cache
  )

  # Standardize
  entsoe_daily <- entsoe_daily %>%
    mutate(
      source_std = case_when(
        source %in% c("Wind Onshore", "Wind Offshore", "Wind") ~ "Wind",
        source == "Fossil Gas" ~ "Gas",
        TRUE ~ source
      )
    ) %>%
    filter(source_std %in% c("Wind", "Solar", "Gas"))

  ember_monthly <- ember_monthly %>%
    filter(source %in% c("Wind", "Solar", "Gas")) %>%
    rename(source_std = source)

  # Calculate factors
  factors <- .calculate_monthly_scaling_factors(
    entsoe_daily = entsoe_daily,
    ember_monthly = ember_monthly,
    tier_threshold = tier_threshold
  )

  # Summarize by country/source
  tier_summary <- factors %>%
    group_by(iso2, source_std, tier) %>%
    summarise(
      median_ratio = median(ratio, na.rm = TRUE),
      mean_ratio = mean(ratio, na.rm = TRUE),
      sd_ratio = sd(ratio, na.rm = TRUE),
      n_months = n(),
      .groups = "drop"
    ) %>%
    arrange(source_std, tier, iso2)

  return(tier_summary)
}


#' Generate diagnostic plots for power generation correction
#' @keywords internal
.generate_power_diagnostics <- function(entsoe_daily,
                                        ember_monthly,
                                        scaling_factors,
                                        result,
                                        diagnostics_folder) {

  message("Generating power generation diagnostics...")
  create_dir(diagnostics_folder)

  # Prepare data for plots
  # Aggregate ENTSOE to monthly for comparison
  entsoe_monthly <- entsoe_daily %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(iso2, year, month, source_std) %>%
    summarise(entsoe_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    mutate(date = ymd(paste(year, month, "01", sep = "-")))

  # Prepare EMBER
  ember_monthly_prep <- ember_monthly %>%
    mutate(year = year(date), month = month(date)) %>%
    select(iso2, year, month, source_std, ember_mwh = value_mwh) %>%
    mutate(date = ymd(paste(year, month, "01", sep = "-")))

  # Corrected
  result_monthly <- result %>%
    group_by(iso2, year=year(date), month=month(date), source_std = case_when(
      source %in% c("Wind Onshore", "Wind Offshore", "Wind") ~ "Wind",
      source == "Fossil Gas" ~ "Gas",
      TRUE ~ source
    )) %>%
    summarise(result_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    mutate(date = ymd(paste(year, month, "01", sep = "-")))

  # Monthly comparison
  monthly_comparison <- entsoe_monthly %>%
    inner_join(ember_monthly_prep, by = c("iso2", "year", "month", "source_std", "date")) %>%
    mutate(
      ratio = entsoe_mwh / ember_mwh,
      ratio = if_else(is.finite(ratio) & ember_mwh > 0, ratio, NA_real_)
    )

  # Yearly comparison
  yearly_comparison <- monthly_comparison %>%
    group_by(iso2, year, source_std) %>%
    summarise(
      entsoe_mwh = sum(entsoe_mwh, na.rm = TRUE),
      ember_mwh = sum(ember_mwh, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ratio = entsoe_mwh / ember_mwh,
      ratio = if_else(is.finite(ratio) & ember_mwh > 0, ratio, NA_real_)
    )

  # Add EU totals
  # eu_monthly <- monthly_comparison %>%
  #   group_by(year, month, date, source_std) %>%
  #   summarise(
  #     entsoe_mwh = sum(entsoe_mwh, na.rm = TRUE),
  #     ember_mwh = sum(ember_mwh, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   mutate(
  #     iso2 = "EU",
  #     ratio = entsoe_mwh / ember_mwh
    # )

  # monthly_comparison <- bind_rows(monthly_comparison, eu_monthly)

  # eu_yearly <- yearly_comparison %>%
  #   group_by(year, source_std) %>%
  #   summarise(
  #     entsoe_mwh = sum(entsoe_mwh, na.rm = TRUE),
  #     ember_mwh = sum(ember_mwh, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   mutate(
  #     iso2 = "EU",
  #     ratio = entsoe_mwh / ember_mwh
  #   )

  # yearly_comparison <- bind_rows(yearly_comparison, eu_yearly)

  # Get tier assignments
  tier_assignment <- scaling_factors %>%
    select(iso2, source_std, tier) %>%
    distinct()

  # Color palette
  source_colors <- c("Wind" = "#377EB8", "Solar" = "#FF7F00", "Gas" = "#4DAF4A")
  datasource_colors <- c("ENTSOE" = "#E41A1C", "EMBER" = "#377EB8")

  # ============================================================================
  # 1. Yearly ratio chart - all countries
  # ============================================================================
  p_yearly_ratio <- yearly_comparison %>%
    ggplot(aes(x = year, y = ratio, color = source_std)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    facet_wrap(~iso2, ncol = 5) +
    scale_y_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
    scale_color_manual(values = source_colors) +
    labs(
      title = "ENTSOE/EMBER Ratio by Country Over Time",
      subtitle = "Horizontal line = perfect agreement (ratio = 1)",
      x = "Year",
      y = "ENTSOE / EMBER Ratio",
      color = "Source"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
    )

  ggsave(file.path(diagnostics_folder, "yearly_ratio_all_countries.png"),
         p_yearly_ratio, width = 16, height = 16)

  # ============================================================================
  # 2. Monthly ratio chart - all countries (recent years)
  # ============================================================================
  p_monthly_ratio <- monthly_comparison %>%
    filter(date >= "2020-01-01") %>%
    ggplot(aes(x = date, y = ratio, color = source_std)) +
    geom_line(linewidth = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    facet_wrap(~iso2, ncol = 5) +
    scale_y_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
    scale_color_manual(values = source_colors) +
    labs(
      title = "ENTSOE/EMBER Monthly Ratio by Country (2020+)",
      subtitle = "Horizontal line = perfect agreement (ratio = 1)",
      x = "Date",
      y = "ENTSOE / EMBER Ratio",
      color = "Source"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
    )

  ggsave(file.path(diagnostics_folder, "monthly_ratio_all_countries.png"),
         p_monthly_ratio, width = 16, height = 16)

  # ============================================================================
  # 3. Yearly comparison by source - ENTSOE vs EMBER
  # ============================================================================
  yearly_plot_data <- yearly_comparison %>%
    select(iso2, year, source_std, entsoe_mwh, ember_mwh) %>%
    pivot_longer(
      cols = c(entsoe_mwh, ember_mwh),
      names_to = "data_source",
      values_to = "value_mwh"
    ) %>%
    mutate(
      data_source = recode(data_source, "entsoe_mwh" = "ENTSOE", "ember_mwh" = "EMBER"),
      value_twh = value_mwh / 1e6
    )

  for (src in c("Wind", "Solar", "Gas", "Hydro")) {
    p_yearly_src <- yearly_plot_data %>%
      filter(source_std == src) %>%
      ggplot(aes(x = year, y = value_twh, color = data_source, linetype = data_source)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      facet_wrap(~iso2, scales = "free_y", ncol = 5) +
      scale_color_manual(values = datasource_colors) +
      labs(
        title = paste0(src, " Generation: ENTSOE vs EMBER by Country (Yearly)"),
        x = "Year",
        y = "Generation (TWh)",
        color = "Data Source",
        linetype = "Data Source"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
      )

    ggsave(file.path(diagnostics_folder, paste0("yearly_", tolower(src), "_all_countries.png")),
           p_yearly_src, width = 16, height = 14)
  }

  # ============================================================================
  # 4. Monthly comparison by source - ENTSOE vs EMBER (recent years)
  # ============================================================================
  monthly_plot_data <- monthly_comparison %>%
    select(iso2, date, year, month, source_std, entsoe_mwh, ember_mwh) %>%
    pivot_longer(
      cols = c(entsoe_mwh, ember_mwh),
      names_to = "data_source",
      values_to = "value_mwh"
    ) %>%
    mutate(
      data_source = recode(data_source, "entsoe_mwh" = "ENTSOE", "ember_mwh" = "EMBER"),
      value_twh = value_mwh / 1e6
    )

  for (src in c("Wind", "Solar", "Gas", "Hydro")) {
    p_monthly_src <- monthly_plot_data %>%
      filter(source_std == src, date >= "2020-01-01") %>%
      ggplot(aes(x = date, y = value_twh, color = data_source)) +
      geom_line(linewidth = 0.5) +
      facet_wrap(~iso2, scales = "free_y", ncol = 5) +
      scale_color_manual(values = datasource_colors) +
      labs(
        title = paste0(src, " Generation: ENTSOE vs EMBER by Country (Monthly, 2020+)"),
        x = "Date",
        y = "Generation (TWh)",
        color = "Data Source"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
      )

    ggsave(file.path(diagnostics_folder, paste0("monthly_", tolower(src), "_all_countries.png")),
           p_monthly_src, width = 16, height = 16)
  }

  # ============================================================================
  # 5. Scaling factors heatmap
  # ============================================================================
  # Get most recent year's ratios
  most_recent_year <- max(yearly_comparison$year[!is.na(yearly_comparison$ratio)])

  heatmap_data <- yearly_comparison %>%
    filter(year == most_recent_year) %>%
    mutate(
      scale_factor = 1 / ratio,
      category = case_when(
        is.na(ratio) ~ "no_data",
        ratio >= 0.95 & ratio <= 1.05 ~ "excellent",
        ratio >= 0.90 & ratio <= 1.10 ~ "good",
        ratio >= 0.70 & ratio <= 1.30 ~ "moderate",
        TRUE ~ "problematic"
      )
    )

  p_heatmap <- heatmap_data %>%
    ggplot(aes(x = source_std, y = reorder(iso2, -scale_factor), fill = scale_factor)) +
    geom_tile() +
    geom_text(aes(label = round(scale_factor, 2)), size = 3) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 1, limits = c(0.5, 2),
      oob = scales::squish,
      name = "Scale Factor"
    ) +
    labs(
      title = paste0("ENTSOE Scaling Factors to Match EMBER (", most_recent_year, ")"),
      subtitle = "Values >1 mean ENTSOE under-reports; <1 means over-reports",
      x = "Source",
      y = "Country"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))

  ggsave(file.path(diagnostics_folder, "scaling_factors_heatmap.png"),
         p_heatmap, width = 8, height = 12)

  # ============================================================================
  # 6. Tier assignment summary
  # ============================================================================
  tier_summary <- heatmap_data %>%
    left_join(tier_assignment, by = c("iso2", "source_std")) %>%
    select(iso2, source = source_std, ratio, scale_factor, category, tier) %>%
    arrange(source, tier, iso2)

  write_csv(tier_summary, file.path(diagnostics_folder, "tier_assignments.csv"))

  # ============================================================================
  # 7. Distribution of ratios
  # ============================================================================
  p_ratio_dist <- monthly_comparison %>%
    filter(!is.na(ratio)) %>%
    ggplot(aes(x = ratio, fill = source_std)) +
    geom_histogram(bins = 50, alpha = 0.7) +
    facet_wrap(~source_std, scales = "free_y") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_fill_manual(values = source_colors) +
    labs(
      title = "Distribution of ENTSOE/EMBER Monthly Ratios",
      x = "Ratio (ENTSOE / EMBER)",
      y = "Count"
    ) +
    theme_minimal() +
    xlim(0.5, 1.5)

  ggsave(file.path(diagnostics_folder, "ratio_distribution_monthly.png"),
         p_ratio_dist, width = 10, height = 5)

  # ============================================================================
  # 8. Summary statistics
  # ============================================================================
  summary_stats <- yearly_comparison %>%
    filter(year == most_recent_year) %>%
    group_by(source_std) %>%
    summarise(
      median_ratio = median(ratio, na.rm = TRUE),
      mean_ratio = mean(ratio, na.rm = TRUE),
      sd_ratio = sd(ratio, na.rm = TRUE),
      n_excellent = sum(ratio >= 0.95 & ratio <= 1.05, na.rm = TRUE),
      n_good = sum(ratio >= 0.90 & ratio <= 1.10 & !(ratio >= 0.95 & ratio <= 1.05), na.rm = TRUE),
      n_moderate = sum(ratio >= 0.70 & ratio <= 1.30 & !(ratio >= 0.90 & ratio <= 1.10), na.rm = TRUE),
      n_problematic = sum(ratio < 0.70 | ratio > 1.30, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(summary_stats, file.path(diagnostics_folder, "summary_statistics.csv"))

  # ============================================================================
  # 9. Result Timeseries
  # ============================================================================
  sources <- unique(entsoe_monthly$source_std)
  for (src in sources) {
    plt <- entsoe_monthly %>%
      left_join(ember_monthly_prep, by = c("iso2", "year", "month", "source_std", "date")) %>%
      left_join(result_monthly, by = c("iso2", "year", "month", "source_std", "date"))  %>%
      tidyr::pivot_longer(
        cols = c(entsoe_mwh, ember_mwh, result_mwh),
        names_to = "data_source",
        values_to = "value_mwh"
      ) %>%
      filter(source_std == src) %>%
      ggplot(aes(x = date, y = value_mwh / 1e6, color = data_source, linetype = data_source)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~iso2, scales = "free_y") +
      labs(
        title = paste0(src, " Generation: ENTSOE vs EMBER vs Corrected by Country (Monthly)"),
        x = NULL,
        y = "Generation (TWh)",
        color = "Data Source",
        linetype = "Data Source"
      )

    ggsave(file.path(diagnostics_folder, paste0("overall_", tolower(src), "_all_countries.png")),
           plt, width = 16, height = 14)

  }

  message("Diagnostics saved to: ", diagnostics_folder)
}
