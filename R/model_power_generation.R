# Power generation: chained ENTSOE -> EMBER-monthly -> EMBER-yearly scaling.
#
# Pipeline (per iso2 x Ember source):
#   1. Aggregate ENTSOE daily -> monthly (drop phantom NA-only sources, flag full
#      months where every day has an observation).
#   2. Compute monthly ratio r_m = ember_monthly / entsoe_monthly.
#   3. Flag outliers (>5 MAD from per-series median); replace with NA.
#   4. ETS-forecast the ratio forward to cover `date_to` (na.interp fills gaps).
#   5. Multiply ENTSOE daily by the monthly ratio.
#   6. Compute yearly ratio r_y = ember_yearly / sum(ember_monthly_for_year).
#   7. MAD + ETS on the yearly ratio (frequency = 1).
#   8. Multiply every day in year Y by the uniform scalar r_y(Y).
#
# Ember-only series (where ENTSOE has too few overlapping observations to
# compute a stable ratio) fall back to uniform daily distribution of the Ember
# monthly value.

# Mapping: ENTSOE source name -> Ember source name.
# Sources omitted from this mapping are dropped (e.g. Hydro Pumped Storage,
# ENTSOE "Other").
ENTSOE_TO_EMBER <- c(
  "Wind"                            = "Wind",
  "Wind Onshore"                    = "Wind",
  "Wind Offshore"                   = "Wind",
  "Solar"                           = "Solar",
  "Fossil Gas"                      = "Gas",
  "Fossil Hard coal"                = "Coal",
  "Fossil Brown coal/Lignite"       = "Coal",
  "Fossil Coal-derived gas"         = "Coal",
  "Fossil Peat"                     = "Coal",
  "Nuclear"                         = "Nuclear",
  "Hydro Run-of-river and poundage" = "Hydro",
  "Hydro Water Reservoir"           = "Hydro",
  "Biomass"                         = "Bioenergy",
  "Fossil Oil"                      = "Other Fossil",
  "Fossil Oil shale"                = "Other Fossil",
  "Geothermal"                      = "Other Renewables",
  "Marine"                          = "Other Renewables",
  "Other renewable"                 = "Other Renewables",
  "Waste"                           = "Other Renewables"
)

# Downstream code (downscale.R, eurostat_gas.R) filters on legacy ENTSOE names.
# Remap Ember names back to legacy on output.
EMBER_TO_OUTPUT <- c(
  "Wind"             = "Wind",
  "Solar"            = "Solar",
  "Gas"              = "Fossil Gas",
  "Coal"             = "Coal",
  "Nuclear"          = "Nuclear",
  "Hydro"            = "Hydro",
  "Bioenergy"        = "Bioenergy",
  "Other Fossil"     = "Other Fossil",
  "Other Renewables" = "Other"
)

# Minimum monthly observations needed to fit ETS. Below this, the monthly
# ratio falls back to last-value carry-forward.
MIN_OBS_ETS_MONTHLY <- 12L

MAD_THRESHOLD <- 5

#' Get power generation data with ENTSOE -> EMBER monthly -> EMBER yearly scaling
#'
#' Daily generation from ENTSOE is corrected in two chained steps so that:
#'   * each calendar month sums to the EMBER monthly value, and
#'   * each calendar year sums to the EMBER yearly value.
#'
#' Per (iso2, Ember source), the monthly ratio (ember/entsoe) is filtered for
#' outliers using MAD and projected forward with ETS so months beyond the last
#' EMBER report still get a robust correction. The yearly ratio
#' (ember_yearly / sum(ember_monthly)) is applied as a uniform per-year scalar.
#'
#' Series with too little ENTSOE overlap for a stable ratio (typically Ember-
#' only sources like Bioenergy in some countries) fall back to uniform daily
#' distribution of the Ember monthly value.
#'
#' @param iso2s ISO2 codes. Default: EU27 + "EU" aggregate.
#' @param date_from First date to return (still pulls earlier history for ETS).
#' @param date_to Last date.
#' @param use_cache Use the on-disk cache.
#' @param mad_threshold Multiplier on MAD for outlier flagging (default 5).
#'   Applied to both the monthly and yearly ratio.
#' @param monthly_extend How to extend the monthly ratio beyond observed Ember
#'   coverage: "ets" (default) fits an ETS forecast, "last" carries forward the
#'   last observed value. The yearly correction always uses "last" (it is
#'   near-constant, so carrying the last observed value forward is robust).
#' @param diagnostics_folder Where to write diagnostic plots/CSVs. NULL disables.
#' @param data_masking One of `DATA_MASKING_NONE` or
#'   `DATA_MASKING_HISTORICAL_DEFAULTS`, or a named masking config list in the
#'   same structure as `get_data_masking_config()`.
#'
#' @return Tibble with columns `iso2, country, region, date, source, value_mw,
#'   value_mwh`. Source uses legacy ENTSOE names (e.g. "Fossil Gas", "Other").
#'
#' @export
get_power_generation <- function(
  iso2s = get_eu_iso2s(include_eu = TRUE),
  date_from = "2015-01-01",
  date_to = Sys.Date(),
  use_cache = TRUE,
  mad_threshold = MAD_THRESHOLD,
  monthly_extend = c("ets", "last"),
  diagnostics_folder = "diagnostics/power_generation",
  data_masking = DATA_MASKING_NONE
) {
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)
  monthly_extend <- match.arg(monthly_extend)
  data_masking <- .resolve_data_masking_config(
    data_masking = data_masking,
    reference_date = date_to
  )

  source_data <- power_data_access_get_sources(
    iso2s = iso2s,
    date_from = date_from,
    date_to = date_to,
    use_cache = use_cache,
    data_masking = data_masking
  )
  entsoe_daily <- source_data$entsoe_daily
  ember_monthly <- source_data$ember_monthly
  ember_yearly <- source_data$ember_yearly

  # Country / region metadata to re-attach to the scaled output.
  country_info <- entsoe_daily %>%
    select(iso2, region, country) %>%
    distinct()

  # ---- Step 1: ENTSOE daily -> monthly ----
  entsoe_monthly <- .aggregate_entsoe_monthly(entsoe_daily)

  # ---- Step 2: monthly ratio with MAD outlier flagging, extended (ETS or last) ----
  ratio_monthly_raw <- .compute_ratio_monthly(entsoe_monthly, ember_monthly)
  ratio_monthly_raw <- .flag_outliers_mad(ratio_monthly_raw, threshold = mad_threshold)
  ratio_monthly_full <- .extend_ratio(
    ratio_monthly_raw,
    target_date = lubridate::floor_date(date_to, "month"),
    frequency   = 12,
    min_obs     = MIN_OBS_ETS_MONTHLY,
    method      = monthly_extend
  )

  # ---- Step 3: apply monthly scaling to ENTSOE daily ----
  scaled_step1 <- .apply_monthly_scaling(entsoe_daily, ratio_monthly_full)

  # ---- Step 4: Ember-only fallback. Triggered when ENTSOE has no meaningful
  # data for an (iso2, source) but Ember does (CY/MT absent from ENTSOE; SI/SK
  # Wind where ENTSOE reports zero but Ember has real generation). Done BEFORE
  # yearly scaling so the yearly correction applies uniformly.
  ember_only_iso2_src <- .find_ember_only_series(
    ember_monthly = ember_monthly,
    scaled_final  = scaled_step1,
    iso2s         = iso2s
  )
  if (nrow(ember_only_iso2_src) > 0) {
    # Drop the all-zero rows for these (iso2, source) pairs to avoid duplicates
    scaled_step1 <- scaled_step1 %>%
      anti_join(ember_only_iso2_src, by = c("iso2", "source"))
    fallback <- .distribute_ember_monthly_to_daily(
      ember_monthly = ember_monthly,
      keys          = ember_only_iso2_src,
      date_from     = date_from,
      date_to       = date_to,
      country_info  = country_info
    )
    scaled_step1 <- bind_rows(scaled_step1, fallback)
  }

  # ---- Step 5: yearly ratio (EMBER monthly -> EMBER yearly).
  # MAD outlier flagging applies; extension is always last-value carry-forward.
  ratio_yearly_raw <- .compute_ratio_yearly(ember_monthly, ember_yearly)
  ratio_yearly_raw <- .flag_outliers_mad(ratio_yearly_raw, threshold = mad_threshold)
  ratio_yearly_full <- .extend_ratio(
    ratio_yearly_raw,
    target_date = as.Date(paste0(year(date_to), "-01-01")),
    frequency   = 1,
    method      = "last"
  )

  # ---- Step 6: apply yearly scaling to everything (ENTSOE-derived AND fallback) ----
  scaled_final <- .apply_yearly_scaling(scaled_step1, ratio_yearly_full)

  # ---- Step 7: re-attach country metadata, remap source names, finalise ----
  result <- scaled_final %>%
    left_join(country_info, by = "iso2") %>%
    mutate(
      source   = recode(source, !!!EMBER_TO_OUTPUT),
      value_mw = value_mwh / 24
    ) %>%
    filter(date >= date_from, date <= date_to) %>%
    select(iso2, country, region, date, source, value_mw, value_mwh) %>%
    arrange(iso2, source, date)

  # Recompute Total
  total <- result %>%
    group_by(iso2, country, region, date) %>%
    summarise(
      value_mw  = sum(value_mw, na.rm = TRUE),
      value_mwh = sum(value_mwh, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(source = "Total")
  result <- bind_rows(result, total) %>% arrange(iso2, source, date)

  # ---- Diagnostics ----
  if (!is.null(diagnostics_folder)) {
    .generate_power_diagnostics(
      entsoe_monthly      = entsoe_monthly,
      ember_monthly       = ember_monthly,
      ember_yearly        = ember_yearly,
      ratio_monthly_full  = ratio_monthly_full,
      ratio_yearly_full   = ratio_yearly_full,
      scaled_final        = scaled_final,
      diagnostics_folder  = diagnostics_folder
    )
  }

  if (any(duplicated(result %>% select(iso2, source, date)))) {
    stop("Duplicate iso2-source-date combinations in get_power_generation() output")
  }

  return(result)
}


# ============================================================================
# Aggregation
# ============================================================================

#' @keywords internal
.aggregate_entsoe_monthly <- function(entsoe_daily) {
  entsoe_daily %>%
    filter(source %in% names(ENTSOE_TO_EMBER)) %>%
    mutate(year = year(date), month = month(date)) %>%
    # Completeness per ORIGINAL source (Wind Onshore separate from Wind Offshore)
    group_by(iso2, source, year, month) %>%
    summarise(
      mwh_orig   = sum(value_mwh, na.rm = TRUE),
      n_days     = sum(!is.na(value_mwh)),
      expected   = lubridate::days_in_month(first(date)),
      full_month = (n_days == expected),
      .groups    = "drop"
    ) %>%
    # Drop phantom rows: entsoe.get_power_generation() complete()s every
    # source x country x date triple even when the source is absent.
    filter(n_days > 0) %>%
    mutate(
      source = recode(source, !!!ENTSOE_TO_EMBER),
      date   = lubridate::make_date(year, month, 1)
    ) %>%
    # Combine sub-sources (Wind Onshore + Wind Offshore -> Wind, etc.)
    group_by(iso2, source, year, month, date) %>%
    summarise(
      entsoe_mwh = sum(mwh_orig, na.rm = TRUE),
      full_month = all(full_month),
      .groups    = "drop"
    )
}


# ============================================================================
# Ratio computation, outlier flagging, ETS extension
# ============================================================================

#' @keywords internal
.compute_ratio_monthly <- function(entsoe_monthly, ember_monthly) {
  ember_prep <- ember_monthly %>%
    select(iso2, source, date, ember_mwh = value_mwh)

  # Allow ember_mwh = 0 (yields ratio = 0 -> scaled = 0, which matches the
  # Ember-reported absence of generation, e.g. SI/SK Wind). Require
  # entsoe_mwh > 0 to avoid division by zero (those cases go through the
  # Ember-only fallback instead).
  entsoe_monthly %>%
    filter(full_month) %>%
    inner_join(ember_prep, by = c("iso2", "source", "date")) %>%
    filter(entsoe_mwh > 0, !is.na(ember_mwh)) %>%
    mutate(ratio = ember_mwh / entsoe_mwh) %>%
    filter(is.finite(ratio)) %>%
    select(iso2, source, date, year, month, entsoe_mwh, ember_mwh, ratio) %>%
    arrange(iso2, source, date)
}


#' @keywords internal
.compute_ratio_yearly <- function(ember_monthly, ember_yearly) {
  monthly_year_sum <- ember_monthly %>%
    mutate(year = year(date)) %>%
    group_by(iso2, source, year) %>%
    summarise(
      monthly_sum = sum(value_mwh, na.rm = TRUE),
      n_months    = n(),
      .groups     = "drop"
    ) %>%
    # Only use years with all 12 months of monthly data for the ratio
    filter(n_months == 12)

  ember_yearly %>%
    mutate(year = year(date)) %>%
    select(iso2, source, year, yearly_mwh = value_mwh) %>%
    inner_join(monthly_year_sum, by = c("iso2", "source", "year")) %>%
    filter(monthly_sum > 0, yearly_mwh > 0) %>%
    mutate(
      ratio = yearly_mwh / monthly_sum,
      date  = lubridate::make_date(year, 1, 1)
    ) %>%
    filter(is.finite(ratio)) %>%
    select(iso2, source, date, year, monthly_sum, yearly_mwh, ratio) %>%
    arrange(iso2, source, year)
}


#' @keywords internal
.flag_outliers_mad <- function(df, threshold = 5) {
  df %>%
    group_by(iso2, source) %>%
    mutate(
      .med       = median(ratio, na.rm = TRUE),
      .mad       = mad(ratio, na.rm = TRUE),
      is_outlier = !is.na(.mad) & .mad > 0 & abs(ratio - .med) > threshold * .mad
    ) %>%
    select(-.med, -.mad) %>%
    ungroup()
}


#' Build a fully-populated ratio series per (iso2, source) from min observed
#' date to `target_date`. Returns a tibble with `date, ratio_obs, ratio_used,
#' type in {observed, interpolated, forecast}, is_outlier`.
#'
#' @param method "ets" fits ETS with na.interp for outliers; "last" uses
#'   last-observation-carried-forward (locf) everywhere. "ets" silently falls
#'   back to "last" when there are fewer than `min_obs` non-outlier
#'   observations.
#' @keywords internal
.extend_ratio <- function(df, target_date, frequency = 12L, min_obs = 12L,
                          method = c("ets", "last")) {
  method <- match.arg(method)
  if (nrow(df) == 0) {
    return(tibble(
      iso2       = character(0),
      source     = character(0),
      date       = as.Date(character(0)),
      ratio_obs  = double(0),
      ratio_used = double(0),
      is_outlier = logical(0),
      type       = character(0)
    ))
  }

  groups <- df %>%
    group_by(iso2, source) %>%
    group_split()

  step <- if (frequency == 12L) "month" else "year"

  res <- pbapply::pblapply(groups, function(g) {
    g <- arrange(g, date)
    iso2_v <- g$iso2[[1]]
    source_v <- g$source[[1]]
    n_obs <- sum(!g$is_outlier, na.rm = TRUE)
    start_dt <- min(g$date)
    last_dt <- max(g$date)
    target <- max(target_date, last_dt)

    # Full date grid from start to target
    grid <- tibble(date = seq.Date(start_dt, target, by = step))
    full <- grid %>%
      left_join(g %>% select(date, ratio_obs = ratio, is_outlier), by = "date") %>%
      mutate(
        is_outlier = coalesce(is_outlier, FALSE),
        ratio_in   = if_else(is_outlier, NA_real_, ratio_obs)
      )

    # ETS needs enough history; otherwise (and when method == "last") use locf
    use_ets <- method == "ets" && n_obs >= min_obs && any(!is.na(full$ratio_in))

    if (!use_ets) {
      # locf: forward-fill, then back-fill leading NAs, default to 1 if all NA
      x <- full$ratio_in
      x <- if (all(is.na(x))) {
        rep(1, length(x))
      } else {
        as.numeric(zoo::na.locf(zoo::na.locf(x, na.rm = FALSE), fromLast = TRUE))
      }
      full <- full %>%
        mutate(
          ratio_used = x,
          type = case_when(
            !is.na(ratio_obs) & !is_outlier ~ "observed",
            date > last_dt ~ "forecast",
            TRUE ~ "interpolated"
          )
        )
    } else {
      # Observed-range only: interpolate outlier-NAs with STL/linear via na.interp
      obs_mask <- full$date <= last_dt
      ratio_obs_range <- full$ratio_in[obs_mask]
      ts_in <- ts(ratio_obs_range,
        start = c(
          year(start_dt),
          if (frequency == 12L) month(start_dt) else 1
        ),
        frequency = frequency
      )
      ts_interp <- tryCatch(forecast::na.interp(ts_in), error = function(e) NULL)
      if (is.null(ts_interp)) {
        # Interpolation failed: keep raw and last-value-carry-forward
        last_val <- tail(ratio_obs_range[!is.na(ratio_obs_range)], 1)
        if (length(last_val) == 0) last_val <- 1
        full$ratio_used <- if_else(is.na(full$ratio_in), last_val, full$ratio_in)
      } else {
        interpolated_vals <- as.numeric(ts_interp)
        h <- sum(!obs_mask)
        forecast_vals <- numeric(0)
        if (h > 0) {
          fit <- tryCatch(forecast::ets(ts_interp), error = function(e) NULL)
          if (!is.null(fit)) {
            fcast <- forecast::forecast(fit, h = h)
            forecast_vals <- as.numeric(fcast$mean)
          } else {
            # ETS failed: carry forward last interpolated value
            forecast_vals <- rep(interpolated_vals[length(interpolated_vals)], h)
          }
        }
        full$ratio_used <- c(interpolated_vals, forecast_vals)
      }
      full <- full %>%
        mutate(type = case_when(
          !is.na(ratio_obs) & !is_outlier ~ "observed",
          date > last_dt ~ "forecast",
          TRUE ~ "interpolated"
        ))
    }

    full %>%
      mutate(iso2 = iso2_v, source = source_v) %>%
      select(iso2, source, date, ratio_obs, ratio_used, is_outlier, type)
  }) %>%
    bind_rows()

  res
}


# ============================================================================
# Apply scaling
# ============================================================================

#' @keywords internal
.apply_monthly_scaling <- function(entsoe_daily, ratio_monthly_full) {
  # Map ENTSOE daily to Ember source, sum sub-sources per (iso2, ember_source, date)
  daily_mapped <- entsoe_daily %>%
    filter(source %in% names(ENTSOE_TO_EMBER)) %>%
    mutate(source = recode(source, !!!ENTSOE_TO_EMBER)) %>%
    group_by(iso2, source, date) %>%
    summarise(value_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    mutate(month_date = lubridate::make_date(year(date), month(date), 1))

  # Prefer the observed ratio (exact Ember monthly match) when available;
  # fall back to the ETS/interpolated value only for months without an Ember
  # observation. The MAD filter is for stabilising the ETS *forecast*, not for
  # overriding real observed Ember monthly values.
  ratio_for_scaling <- ratio_monthly_full %>%
    mutate(ratio_m = coalesce(ratio_obs, ratio_used)) %>%
    select(iso2, source, month_date = date, ratio_m)

  daily_mapped %>%
    left_join(ratio_for_scaling, by = c("iso2", "source", "month_date")) %>%
    # Where the ratio is missing entirely (series too short for ETS),
    # pass through ENTSOE unscaled
    mutate(
      ratio_m   = coalesce(ratio_m, 1),
      value_mwh = value_mwh * ratio_m
    ) %>%
    select(iso2, source, date, value_mwh)
}


#' @keywords internal
.apply_yearly_scaling <- function(scaled_daily, ratio_yearly_full) {
  # Same logic as monthly: prefer observed yearly ratio over the ETS-smoothed
  # value. ratio_used falls back to ETS only when Ember yearly isn't published
  # yet for that year.
  ratio_for_yearly <- ratio_yearly_full %>%
    mutate(ratio_y = coalesce(ratio_obs, ratio_used)) %>%
    select(iso2, source, year_date = date, ratio_y)

  scaled_daily %>%
    mutate(year_date = lubridate::make_date(year(date), 1, 1)) %>%
    left_join(ratio_for_yearly, by = c("iso2", "source", "year_date")) %>%
    mutate(
      ratio_y   = coalesce(ratio_y, 1),
      value_mwh = value_mwh * ratio_y
    ) %>%
    select(iso2, source, date, value_mwh)
}


# ============================================================================
# Ember-only fallback (sources absent or too sparse in ENTSOE)
# ============================================================================

#' Identify (iso2, source) pairs that need the Ember-only fallback because either
#'   (a) the pair is entirely absent from the ENTSOE-scaled output (e.g. CY/MT
#'       missing from ENTSOE, or Bioenergy in countries without Biomass reporting), or
#'   (b) the pair exists in the scaled output but sums to ~0 across the whole
#'       date range while Ember reports meaningful generation (e.g. SI/SK Wind
#'       where ENTSOE reports zero but real wind exists).
#' @keywords internal
.find_ember_only_series <- function(ember_monthly, scaled_final, iso2s) {
  ember_keys <- ember_monthly %>%
    filter(
      iso2 %in% iso2s,
      source %in% names(EMBER_TO_OUTPUT)
    ) %>%
    group_by(iso2, source) %>%
    summarise(ember_total = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    filter(ember_total > 0) %>%
    select(iso2, source)

  scaled_keys_meaningful <- scaled_final %>%
    group_by(iso2, source) %>%
    summarise(scaled_total = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    filter(scaled_total > 0) %>%
    select(iso2, source)

  anti_join(ember_keys, scaled_keys_meaningful, by = c("iso2", "source"))
}


#' For (iso2, source) pairs without ENTSOE coverage, distribute Ember monthly
#' uniformly across days. Last available monthly value is carried forward to
#' `date_to`.
#' @keywords internal
.distribute_ember_monthly_to_daily <- function(ember_monthly, keys, date_from, date_to, country_info) {
  if (nrow(keys) == 0) {
    return(NULL)
  }

  ember_subset <- ember_monthly %>%
    inner_join(keys, by = c("iso2", "source"))

  if (nrow(ember_subset) == 0) {
    return(NULL)
  }

  # Carry last value forward to date_to
  last_values <- ember_subset %>%
    group_by(iso2, source) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(iso2, source, last_value = value_mwh, last_date = date)

  end_month <- lubridate::floor_date(as.Date(date_to), "month")
  extension <- last_values %>%
    group_by(iso2, source) %>%
    group_modify(function(df, .) {
      if (df$last_date >= end_month) {
        return(tibble())
      }
      months <- seq.Date(lubridate::ceiling_date(df$last_date, "month"), end_month, by = "month")
      tibble(date = months, value_mwh = df$last_value)
    }) %>%
    ungroup()

  ember_extended <- bind_rows(
    ember_subset %>% select(iso2, source, date, value_mwh),
    extension
  ) %>%
    distinct(iso2, source, date, .keep_all = TRUE)

  # Distribute monthly value uniformly across days in that month
  ember_extended %>%
    rename(month_start = date) %>%
    mutate(
      month_end     = lubridate::ceiling_date(month_start, "month") - days(1),
      days_in_month = as.integer(month_end - month_start) + 1L,
      daily_mwh     = value_mwh / days_in_month
    ) %>%
    rowwise() %>%
    reframe(
      iso2      = iso2,
      source    = source,
      date      = seq.Date(month_start, month_end, by = "day"),
      value_mwh = daily_mwh
    ) %>%
    ungroup() %>%
    filter(date >= as.Date(date_from), date <= as.Date(date_to)) %>%
    select(iso2, source, date, value_mwh)
}


# ============================================================================
# Diagnostics
# ============================================================================

#' @keywords internal
.generate_power_diagnostics <- function(entsoe_monthly,
                                        ember_monthly,
                                        ember_yearly,
                                        ratio_monthly_full,
                                        ratio_yearly_full,
                                        scaled_final,
                                        diagnostics_folder) {
  message("Generating power generation diagnostics...")
  create_dir(diagnostics_folder)

  sources <- unique(ratio_monthly_full$source)

  source_colors <- c(
    "Wind" = "#377EB8", "Solar" = "#FF7F00", "Gas" = "#4DAF4A",
    "Coal" = "#984EA3", "Nuclear" = "#E41A1C", "Hydro" = "#00BFC4"
  )

  # Helper: prepend the last non-forecast point to the forecast segment so the
  # forecast line is visible even when there's only a single forecast period.
  build_forecast_with_anchor <- function(df) {
    df %>%
      group_by(iso2, source) %>%
      group_modify(function(g, .) {
        fcst <- g %>% filter(type == "forecast")
        if (nrow(fcst) == 0) {
          return(tibble())
        }
        anchor_pool <- g %>% filter(type %in% c("observed", "interpolated"))
        if (nrow(anchor_pool) == 0) {
          return(fcst)
        }
        anchor <- anchor_pool %>% filter(date == max(date))
        bind_rows(anchor, fcst) %>% arrange(date)
      }) %>%
      ungroup()
  }

  # ---- 1. Monthly ratio time series with outliers + forecast ----
  for (src in sources) {
    df_src <- ratio_monthly_full %>% filter(source == src)
    if (nrow(df_src) == 0) next

    fcst_segment <- build_forecast_with_anchor(df_src)

    p <- df_src %>%
      ggplot(aes(x = date)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      # Continuous observed + interpolated line (single stroke, no breaks)
      geom_line(
        data = filter(df_src, type %in% c("observed", "interpolated")),
        aes(y = ratio_used), color = "#2166AC", linewidth = 0.5
      ) +
      # Forecast segment with anchor point
      geom_line(
        data = fcst_segment,
        aes(y = ratio_used), color = "#D6604D", linewidth = 0.7
      ) +
      # Mark interpolated months
      geom_point(
        data = filter(df_src, type == "interpolated"),
        aes(y = ratio_used), color = "#FDB863", size = 0.9
      ) +
      # Mark forecast months
      geom_point(
        data = filter(df_src, type == "forecast"),
        aes(y = ratio_used), color = "#D6604D", size = 0.9
      ) +
      # Outliers (x on the original observed ratio)
      geom_point(
        data = filter(df_src, is_outlier),
        aes(y = ratio_obs), color = "#D6604D", shape = 4, size = 2, stroke = 0.8
      ) +
      facet_wrap(~iso2, scales = "free_y", ncol = 7) +
      labs(
        title = glue("Monthly ratio (EMBER / ENTSOE) - {src}"),
        subtitle = "Blue = observed + interpolated. Red = forecast. Orange dots = interpolated. Red x = outlier.",
        x = NULL, y = "Ember / ENTSOE"
      ) +
      theme_minimal(base_size = 9) +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
      )

    n_countries <- n_distinct(df_src$iso2)
    ggsave(file.path(diagnostics_folder, paste0("monthly_ratio_", tolower(src), ".png")),
      p,
      width = 20, height = 11.25
    )
  }

  # ---- 2. Monthly ratio diagnostics CSV ----
  monthly_summary <- ratio_monthly_full %>%
    group_by(iso2, source) %>%
    summarise(
      n_obs = sum(type == "observed"),
      n_outliers = sum(is_outlier),
      pct_outliers = 100 * n_outliers / pmax(1, n_obs + n_outliers),
      median_ratio = median(ratio_obs, na.rm = TRUE),
      last_obs_month = if (any(type == "observed")) as.character(max(date[type == "observed"])) else NA_character_,
      n_forecast = sum(type == "forecast"),
      .groups = "drop"
    )
  write_csv(monthly_summary, file.path(diagnostics_folder, "monthly_ratio_diagnostics.csv"))

  # ---- 3. Post-monthly residual ----
  # Compared against the TRUE target: Ember_monthly * yearly_correction. Near 0
  # means the pipeline is doing what it should; non-zero only where Ember-only
  # fallback fires, ratios are missing, or ETS extrapolation is in use.
  scaled_monthly <- scaled_final %>%
    mutate(
      year = year(date), month = month(date),
      date_m = lubridate::make_date(year, month, 1)
    ) %>%
    group_by(iso2, source, date_m) %>%
    summarise(scaled_month_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop") %>%
    rename(date = date_m)

  yearly_correction_for_residual <- ratio_yearly_full %>%
    mutate(
      ratio_y = coalesce(ratio_obs, ratio_used),
      year = year(date)
    ) %>%
    select(iso2, source, year, ratio_y)

  post_monthly <- scaled_monthly %>%
    inner_join(ember_monthly %>% select(iso2, source, date, ember_mwh = value_mwh),
      by = c("iso2", "source", "date")
    ) %>%
    mutate(year = year(date)) %>%
    left_join(yearly_correction_for_residual, by = c("iso2", "source", "year")) %>%
    mutate(
      ratio_y = coalesce(ratio_y, 1),
      target_mwh = ember_mwh * ratio_y
    ) %>%
    filter(target_mwh > 0) %>%
    mutate(residual_pct = 100 * (scaled_month_mwh - target_mwh) / target_mwh)

  for (src in sources) {
    df_src <- post_monthly %>% filter(source == src)
    if (nrow(df_src) == 0) next
    p <- df_src %>%
      ggplot(aes(x = date, y = residual_pct)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
      geom_line(color = "#2166AC", linewidth = 0.4) +
      facet_wrap(~iso2, ncol = 7) +
      coord_cartesian(ylim = c(-100, 100)) +
      labs(
        title = glue("Post-monthly residual - {src}"),
        subtitle = paste0(
          "100 * (scaled_monthly_sum - target) / target, ",
          "where target = Ember_monthly * yearly_correction. ",
          "Should be ~0; axis clipped at +/-100%."
        ),
        x = NULL, y = "Residual (%)"
      ) +
      theme_minimal(base_size = 9) +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
      )
    n_countries <- n_distinct(df_src$iso2)
    ggsave(file.path(diagnostics_folder, paste0("post_monthly_residual_", tolower(src), ".png")),
      p,
      width = 20, height = 11.25
    )
  }

  # ---- 4. Yearly ratio with outliers + forecast ----
  for (src in sources) {
    df_src <- ratio_yearly_full %>% filter(source == src)
    if (nrow(df_src) == 0) next

    fcst_segment <- build_forecast_with_anchor(df_src)

    p <- df_src %>%
      ggplot(aes(x = date)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray60", linewidth = 0.3) +
      geom_line(
        data = filter(df_src, type %in% c("observed", "interpolated")),
        aes(y = ratio_used), color = "#2166AC", linewidth = 0.5
      ) +
      geom_line(
        data = fcst_segment,
        aes(y = ratio_used), color = "#D6604D", linewidth = 0.7
      ) +
      geom_point(
        data = filter(df_src, type == "interpolated"),
        aes(y = ratio_used), color = "#FDB863", size = 1.2
      ) +
      geom_point(
        data = filter(df_src, type == "forecast"),
        aes(y = ratio_used), color = "#D6604D", size = 1.2
      ) +
      geom_point(
        data = filter(df_src, is_outlier),
        aes(y = ratio_obs), color = "#D6604D", shape = 4, size = 2.5, stroke = 1
      ) +
      facet_wrap(~iso2, scales = "free_y", ncol = 7) +
      labs(
        title = glue("Yearly ratio (EMBER_yearly / sum(EMBER_monthly)) - {src}"),
        subtitle = "Blue = observed + interpolated. Red = forecast. Orange dots = interpolated. Red x = outlier.",
        x = NULL, y = "Ratio"
      ) +
      theme_minimal(base_size = 9) +
      theme(strip.text = element_text(size = 8, face = "bold"))
    n_countries <- n_distinct(df_src$iso2)
    ggsave(file.path(diagnostics_folder, paste0("yearly_ratio_", tolower(src), ".png")),
      p,
      width = 20, height = 11.25
    )
  }

  yearly_summary <- ratio_yearly_full %>%
    group_by(iso2, source) %>%
    summarise(
      n_obs = sum(type == "observed"),
      n_outliers = sum(is_outlier),
      mean_ratio = mean(ratio_obs, na.rm = TRUE),
      last_obs_yr = if (any(type == "observed")) year(max(date[type == "observed"])) else NA_integer_,
      n_forecast = sum(type == "forecast"),
      .groups = "drop"
    )
  write_csv(yearly_summary, file.path(diagnostics_folder, "yearly_ratio_diagnostics.csv"))

  # ---- 5. Post-yearly residual ----
  scaled_yearly <- scaled_final %>%
    mutate(year = year(date)) %>%
    group_by(iso2, source, year) %>%
    summarise(scaled_year_mwh = sum(value_mwh, na.rm = TRUE), .groups = "drop")

  ember_yearly_prep <- ember_yearly %>%
    mutate(year = year(date)) %>%
    select(iso2, source, year, ember_yearly_mwh = value_mwh)

  post_yearly <- scaled_yearly %>%
    inner_join(ember_yearly_prep, by = c("iso2", "source", "year")) %>%
    filter(ember_yearly_mwh > 0) %>%
    mutate(residual_pct = 100 * (scaled_year_mwh - ember_yearly_mwh) / ember_yearly_mwh)

  for (src in sources) {
    df_src <- post_yearly %>% filter(source == src)
    if (nrow(df_src) == 0) next
    p <- df_src %>%
      ggplot(aes(x = year, y = residual_pct)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
      geom_line(color = "#4DAF4A", linewidth = 0.5) +
      geom_point(color = "#4DAF4A", size = 1) +
      facet_wrap(~iso2, ncol = 7) +
      coord_cartesian(ylim = c(-100, 100)) +
      labs(
        title = glue("Post-yearly residual - {src}"),
        subtitle = paste0(
          "100 * (scaled_year_sum - Ember_yearly) / Ember_yearly. ",
          "Should be near 0; axis clipped at +/-100%."
        ),
        x = NULL, y = "Residual (%)"
      ) +
      theme_minimal(base_size = 9) +
      theme(strip.text = element_text(size = 8, face = "bold"))
    n_countries <- n_distinct(df_src$iso2)
    ggsave(file.path(diagnostics_folder, paste0("post_yearly_residual_", tolower(src), ".png")),
      p,
      width = 20, height = 11.25
    )
  }

  # ---- 6. Final combined scale-factor heatmap (latest fully-observed year) ----
  observed_yearly_dates <- ratio_yearly_full$date[ratio_yearly_full$type == "observed"]
  latest_year <- if (length(observed_yearly_dates) > 0) max(observed_yearly_dates) else NA
  if (!is.na(latest_year)) {
    combined_factor <- ratio_monthly_full %>%
      filter(
        type == "observed",
        lubridate::floor_date(date, "year") == latest_year
      ) %>%
      group_by(iso2, source) %>%
      summarise(monthly_factor = exp(mean(log(ratio_used), na.rm = TRUE)), .groups = "drop") %>%
      inner_join(
        ratio_yearly_full %>% filter(date == latest_year) %>%
          select(iso2, source, yearly_factor = ratio_used),
        by = c("iso2", "source")
      ) %>%
      mutate(combined = monthly_factor * yearly_factor)

    if (nrow(combined_factor) > 0) {
      p_heat <- combined_factor %>%
        ggplot(aes(x = source, y = reorder(iso2, -combined), fill = combined)) +
        geom_tile() +
        geom_text(aes(label = round(combined, 2)), size = 3) +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 1, limits = c(0.5, 2), oob = scales::squish,
          name = "Combined\nscale factor"
        ) +
        labs(
          title = glue("Combined ENTSOE -> Ember scale factor ({year(latest_year)})"),
          subtitle = "Geometric-mean monthly factor x yearly factor",
          x = "Source", y = "Country"
        ) +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))
      ggsave(file.path(diagnostics_folder, "final_scale_heatmap.png"),
        p_heat,
        width = 8, height = 12
      )
    }
  }

  # ---- 7. End-to-end comparison per source ----
  # Restrict to the ENTSOE-covered range so the axis doesn't stretch back to
  # Ember's pre-2015 history.
  plot_start <- min(entsoe_monthly$date, na.rm = TRUE)
  entsoe_for_plot <- entsoe_monthly %>%
    select(iso2, source, date, entsoe_mwh) %>%
    filter(date >= plot_start)
  ember_for_plot <- ember_monthly %>%
    select(iso2, source, date, ember_mwh = value_mwh) %>%
    filter(date >= plot_start)

  # Build "Ember monthly scaled to yearly" target = ember_monthly * r_y(year)
  yearly_correction <- ratio_yearly_full %>%
    mutate(
      ratio_y = coalesce(ratio_obs, ratio_used),
      year = year(date)
    ) %>%
    select(iso2, source, year, ratio_y)

  ember_target_for_plot <- ember_for_plot %>%
    mutate(year = year(date)) %>%
    left_join(yearly_correction, by = c("iso2", "source", "year")) %>%
    mutate(
      ratio_y = coalesce(ratio_y, 1),
      ember_target_mwh = ember_mwh * ratio_y
    ) %>%
    select(iso2, source, date, ember_target_mwh)

  for (src in sources) {
    df <- entsoe_for_plot %>%
      filter(source == src) %>%
      full_join(ember_for_plot %>% filter(source == src), by = c("iso2", "source", "date")) %>%
      full_join(ember_target_for_plot %>% filter(source == src), by = c("iso2", "source", "date")) %>%
      full_join(
        scaled_monthly %>% filter(source == src) %>%
          rename(scaled_mwh = scaled_month_mwh),
        by = c("iso2", "source", "date")
      ) %>%
      pivot_longer(c(entsoe_mwh, ember_mwh, ember_target_mwh, scaled_mwh),
        names_to = "series", values_to = "value_mwh"
      ) %>%
      mutate(
        series = recode(series,
          "entsoe_mwh"       = "ENTSOE (raw)",
          "ember_mwh"        = "Ember monthly",
          "ember_target_mwh" = "Ember * yearly correction (target)",
          "scaled_mwh"       = "Scaled (output)"
        ),
        value_twh = value_mwh / 1e6
      )

    if (nrow(df) == 0) next

    series_levels <- c(
      "ENTSOE (raw)", "Ember monthly",
      "Ember * yearly correction (target)", "Scaled (output)"
    )
    df$series <- factor(df$series, levels = series_levels)

    p <- ggplot(df, aes(
      x = date, y = value_twh, color = series,
      linetype = series, linewidth = series
    )) +
      # Draw bottom-to-top so target & output are visible and ENTSOE dashed on top
      geom_line(data = filter(df, series == "Scaled (output)")) +
      geom_line(data = filter(df, series == "Ember * yearly correction (target)")) +
      geom_line(data = filter(df, series == "Ember monthly")) +
      geom_line(data = filter(df, series == "ENTSOE (raw)")) +
      facet_wrap(~iso2, scales = "free_y", ncol = 7) +
      scale_color_manual(values = c(
        "ENTSOE (raw)"                       = "#444444",
        "Ember monthly"                      = "#2CA02C",
        "Ember * yearly correction (target)" = "#2166AC",
        "Scaled (output)"                    = "#D6604D"
      )) +
      scale_linetype_manual(values = c(
        "ENTSOE (raw)"                       = "dashed",
        "Ember monthly"                      = "dashed",
        "Ember * yearly correction (target)" = "solid",
        "Scaled (output)"                    = "solid"
      )) +
      scale_linewidth_manual(values = c(
        "ENTSOE (raw)"                       = 0.4,
        "Ember monthly"                      = 0.4,
        "Ember * yearly correction (target)" = 0.6,
        "Scaled (output)"                    = 0.5
      )) +
      labs(
        title = glue("Monthly generation: ENTSOE vs Ember vs scaled - {src}"),
        subtitle = paste(
          "Gray dashed = ENTSOE raw. Green dashed = Ember monthly.",
          "Blue solid = Ember * yearly correction (true target). Red = scaled output (should track blue solid)."
        ),
        x = NULL, y = "TWh", color = NULL, linetype = NULL, linewidth = NULL
      ) +
      theme_minimal(base_size = 9) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
      )
    n_countries <- n_distinct(df$iso2)
    ggsave(file.path(diagnostics_folder, paste0("overall_", tolower(src), ".png")),
      p,
      width = 20, height = 11.25
    )
  }

  message("Diagnostics saved to: ", diagnostics_folder)
}
