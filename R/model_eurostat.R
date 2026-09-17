#' Get monthly fossil fuel consumption per country from EUROSTAT energy balance
#' Update on 27 April 2024: Eurostat stopped working with nrg_cb_oil for all countries
#' -> we add a geo=EU27_2020 filter, which will prevent us from updating national numbers for now
#'
#' @param diagnostics_folder
#' @param use_cache
#' @param iso2s
#' @param data_masking One of `DATA_MASKING_NONE` or
#'   `DATA_MASKING_HISTORICAL_DEFAULTS`, or a named masking config list in the
#'   same structure as `get_data_masking_config()`.
#'
#' @return
#' @export
#'
#' @examples
get_eurostat_cons <- function(
  pwr_generation,
  diagnostics_folder = "diagnostics/eurostat",
  use_cache = FALSE,
  iso2s = NULL,
  data_masking = DATA_MASKING_NONE
) {
  data_masking <- .resolve_data_masking_config(
    data_masking = data_masking
  )
  create_dir(diagnostics_folder)


  # Get monthly and yearly data
  cons_sources <- log_timed_stage("eurostat_data_access_get_cons_sources", {
    eurostat_data_access_get_cons_sources(
      use_cache = use_cache,
      data_masking = data_masking
    )
  })
  cons_raw_oil <- cons_sources$oil
  cons_raw_solid <- cons_sources$solid
  cons_raw_gas <- cons_sources$gas
  reported_solid <- cons_raw_solid$monthly
  gap_provenance <- attr(reported_solid, "coal_gap_provenance")
  if (!is.null(gap_provenance) && nrow(gap_provenance) > 0) {
    keys <- c(.coal_monthly_gap_keys(), "time")
    reported_solid <- reported_solid %>% left_join(
      gap_provenance %>% select(all_of(keys), original_value, method), by = keys
    ) %>% mutate(values = if_else(!is.na(method), original_value, values)) %>%
      select(-original_value, -method)
  }
  if (!is_null_or_empty(diagnostics_folder)) {
    readr::write_csv(
      coal_energy_coverage(cons_raw_solid$yearly, reported_solid),
      file.path(diagnostics_folder, "coal_energy_coverage.csv")
    )
  }

  cons_raw_solid$monthly <- fill_raw_coal_annual_backed(
    cons_raw_solid$monthly,
    cons_raw_solid$yearly
  )
  coal_eu_repair_candidates <- attr(
    cons_raw_solid$monthly,
    "coal_eu_repair_candidates"
  )
  write_coal_gap_diagnostics(cons_raw_solid$monthly, diagnostics_folder)

  # Check siec is complete and unique.
  # That all iso2s are included
  check_siec_siec_code <- function(x) {
    y <- distinct(x, siec)
    stopifnot(!any(is.na(y$siec)))
    stopifnot(!any(duplicated(y$siec)))
    stopifnot(!any(is.na(x$iso2)))
  }
  check_siec_siec_code(cons_raw_oil$monthly)
  check_siec_siec_code(cons_raw_oil$yearly)
  check_siec_siec_code(cons_raw_solid$monthly)
  check_siec_siec_code(cons_raw_solid$yearly)
  check_siec_siec_code(cons_raw_gas$monthly)
  check_siec_siec_code(cons_raw_gas$yearly)

  aggregate <- function(x) {
    x %>%
      group_by(iso2, sector, time, unit, siec, fuel) %>%
      summarise(values = sum_or_na(values), .groups = "drop") %>%
      ungroup()
  }

  # Process data
  solid_monthly <- process_solid_monthly(cons_raw_solid$monthly, pwr_generation)
  if (!is_null_or_empty(diagnostics_folder)) {
    readr::write_csv(attr(solid_monthly, "coal_eu_completeness"),
      file.path(diagnostics_folder, "coal_eu_completeness.csv"))
  }
  cons_monthly <- log_timed_stage("process_eurostat_monthly", {
    list(
      oil = process_oil_monthly(cons_raw_oil$monthly),
      solid = solid_monthly %>%
        eurostat_split_solid_elec_others(),
      gas = process_gas_monthly(cons_raw_gas$monthly, pwr_generation = pwr_generation) %>%
        eurostat_split_elec_others()
    ) %>%
      bind_rows() %>%
      aggregate() %>%
      add_iso2() %>%
      select(iso2, sector, time, unit, siec, fuel, values)
  })

  cons_yearly <- log_timed_stage("process_eurostat_yearly", {
    list(
      oil = process_oil_yearly(cons_raw_oil$yearly),
      solid = process_solid_yearly(cons_raw_solid$yearly) %>%
        eurostat_split_solid_elec_others(),
      gas = process_gas_yearly(cons_raw_gas$yearly, pwr_generation = pwr_generation) %>%
        eurostat_split_elec_others()
    ) %>%
      bind_rows() %>%
      aggregate() %>%
      add_iso2() %>%
      select(iso2, sector, time, unit, siec, fuel, values)
  })
  attr(cons_monthly, "coal_reported_monthly") <- reported_solid %>%
    process_solid_monthly(pwr_generation) %>% eurostat_split_solid_elec_others()

  # Check that there is no na value
  if (
    any(!complete.cases(select(cons_monthly, -values))) ||
      any(!complete.cases(select(cons_yearly, -values)))
  ) {
    stop("There are NA values in the data (except in the 'values' column).")
  }

  # Apply seasonal adjustment to convert yearly data to monthly
  cons_yearly_monthly <- log_timed_stage("apply_seasonal_adjustment", {
    apply_seasonal_adjustment(cons_yearly, cons_monthly)
  })
  coal_allocation <- attr(cons_yearly_monthly, "coal_allocation")
  if (!is_null_or_empty(diagnostics_folder) && !is.null(coal_allocation)) {
    readr::write_csv(coal_allocation, file.path(diagnostics_folder, "coal_allocation.csv"))
  }

  # Combine monthly and yearly data with cutoff filtering
  cons_combined <- log_timed_stage("combine_monthly_yearly_with_cutoff", {
    combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly) %>%
      resolve_coal_unallocated_totals()
  })
  coal_unallocated_sector <- attr(cons_combined, "coal_unallocated_sector")
  coal_downstream_completeness <- coal_downstream_completeness(
    cons_monthly,
    cons_combined
  )
  if (!is_null_or_empty(diagnostics_folder)) {
    readr::write_csv(
      coal_downstream_completeness,
      file.path(diagnostics_folder, "coal_downstream_completeness.csv")
    )
    readr::write_csv(
      coal_unallocated_sector,
      file.path(diagnostics_folder, "coal_unallocated_sector.csv")
    )
  }

  log_timed_stage("check_coal_annual_bounds", {
    check_coal_annual_bounds(
      cons_combined = cons_combined,
      cons_yearly = cons_yearly,
      diagnostics_folder = diagnostics_folder
    )
  })

  if (!is_null_or_empty(diagnostics_folder)) {
    # Visual check
    log_timed_stage("diagnostic_eurostat_cons_yearly_monthly", {
      diagnostic_eurostat_cons_yearly_monthly(
        diagnostics_folder = diagnostics_folder,
        cons_yearly = cons_yearly,
        cons_monthly = cons_monthly,
        cons_combined = cons_combined,
        detailed_iso2s = c("BE", "NL", "PT", "SK", "EU", "IE", "DE")
      )
    })
  }

  cons <- cons_combined %>%
    select(-c(source))

  # Add infos
  cons <- cons %>%
    recode_siec()

  # Keep regions of interest
  if (!is.null(iso2s)) {
    cons <- cons %>%
      filter(iso2 %in% iso2s)
  }

  # Remove last incomplete month for each region
  cons <- log_timed_stage("remove_last_incomplete", {
    cons %>%
      remove_last_incomplete()
  })


  # Other diagnostics
  if (!is_null_or_empty(diagnostics_folder)) {
    log_timed_stage("diagnostic_eurostat_cons", {
      diagnostic_eurostat_cons(
        cons,
        iso2s = iso2s,
        diagnostics_folder = diagnostics_folder
      )
    })
  }

  attr(cons, "coal_eu_repair_candidates") <- coal_eu_repair_candidates
  attr(cons, "coal_downstream_completeness") <- coal_downstream_completeness
  attr(cons, "coal_unallocated_sector") <- coal_unallocated_sector
  attr(cons, "coal_allocation") <- coal_allocation
  return(cons)
}

#' Record whether downstream annual data resolves missing monthly coal components
#'
#' @keywords internal
coal_downstream_completeness <- function(cons_monthly, cons_combined) {
  keys <- c("iso2", "sector", "time", "unit", "siec", "fuel")
  monthly <- cons_monthly %>%
    filter(siec %in% COAL_MONTHLY_GAP_FUELS) %>%
    select(all_of(keys), monthly_value = values)
  combined <- cons_combined %>%
    filter(siec %in% COAL_MONTHLY_GAP_FUELS) %>%
    select(all_of(keys), combined_value = values, combined_source = source)

  monthly %>%
    full_join(combined, by = keys) %>%
    mutate(
      status = case_when(
        !is.na(monthly_value) ~ "monthly",
        is.na(monthly_value) & combined_source == "yearly" &
          !is.na(combined_value) ~ "yearly_fallback",
        TRUE ~ "unresolved"
      )
    )
}

#' Reconcile unallocated coal totals with sector observations
#'
#' A coal total can be retained as `unknown` when its electricity split is
#' unavailable. A monthly total takes precedence over annual sector fallbacks.
#' An annual total retains only the residual not covered by reported monthly
#' sectors. These rules preserve the preferred total without double-counting a
#' lower-priority sector split.
#'
#' @keywords internal
resolve_coal_unallocated_totals <- function(x, tolerance = 1e-6) {
  keys <- intersect(names(x), c("iso2", "time", "unit", "siec", "fuel"))
  stats <- x %>%
    filter(siec %in% COAL_MONTHLY_GAP_FUELS) %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      monthly_unknown = if_else(
        any(
          sector == SECTOR_UNKNOWN & source == "monthly" & !is.na(values)
        ),
        sum(values[
          sector == SECTOR_UNKNOWN & source == "monthly" & !is.na(values)
        ]),
        NA_real_
      ),
      annual_unknown = if_else(
        any(
          sector == SECTOR_UNKNOWN & source == "yearly" & !is.na(values)
        ),
        sum(values[
          sector == SECTOR_UNKNOWN & source == "yearly" & !is.na(values)
        ]),
        NA_real_
      ),
      monthly_electricity = if_else(
        any(
          sector == SECTOR_ELEC & source == "monthly" & !is.na(values)
        ),
        sum(values[
          sector == SECTOR_ELEC & source == "monthly" & !is.na(values)
        ]),
        NA_real_
      ),
      monthly_others = if_else(
        any(
          sector == SECTOR_OTHERS & source == "monthly" & !is.na(values)
        ),
        sum(values[
          sector == SECTOR_OTHERS & source == "monthly" & !is.na(values)
        ]),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    filter(!is.na(monthly_unknown) | !is.na(annual_unknown)) %>%
    mutate(
      monthly_components = rowSums(!is.na(pick(
        monthly_electricity, monthly_others
      ))),
      monthly_known = rowSums(pick(
        monthly_electricity, monthly_others
      ), na.rm = TRUE),
      residual = annual_unknown - monthly_known,
      status = case_when(
        !is.na(monthly_unknown) ~ "monthly_total_unallocated",
        monthly_components == 2L ~ "monthly_split_complete",
        residual < -tolerance ~ "negative_residual",
        monthly_components == 0L ~ "annual_total_unallocated",
        TRUE ~ "annual_residual_unallocated"
      )
    )

  if (nrow(stats) == 0) {
    attr(x, "coal_unallocated_sector") <- stats
    return(x)
  }

  result <- x %>%
    left_join(
      stats %>% select(
        all_of(keys), monthly_unknown, annual_unknown,
        monthly_components, residual, status
      ),
      by = keys,
      relationship = "many-to-one"
    ) %>%
    filter(!coalesce(
      status == "monthly_total_unallocated" &
        sector %in% c(SECTOR_ELEC, SECTOR_OTHERS),
      FALSE
    )) %>%
    filter(!coalesce(
      status == "monthly_total_unallocated" &
        sector == SECTOR_UNKNOWN & source == "yearly",
      FALSE
    )) %>%
    filter(!coalesce(
      status == "monthly_split_complete" &
        sector == SECTOR_UNKNOWN & source == "yearly",
      FALSE
    )) %>%
    mutate(
      values = case_when(
        sector == SECTOR_UNKNOWN & source == "yearly" &
          status %in% c(
            "annual_total_unallocated", "annual_residual_unallocated"
          ) ~ pmax(0, residual),
        sector == SECTOR_UNKNOWN & source == "yearly" &
          status == "negative_residual" ~ NA_real_,
        TRUE ~ values
      )
    ) %>%
    filter(!(
      !is.na(annual_unknown) &
        status != "monthly_split_complete" &
        sector %in% c(SECTOR_ELEC, SECTOR_OTHERS) &
        source == "monthly" & is.na(values)
    )) %>%
    select(
      -monthly_unknown, -annual_unknown, -monthly_components, -residual,
      -status
    )

  attr(result, "coal_unallocated_sector") <- stats
  result
}


#' Check monthly coal consumption against annual Eurostat balances
#'
#' The check reports every complete monthly coal series that has a matching
#' annual balance. It warns about differences greater than `relative_tolerance`
#' and writes the comparison to the Eurostat diagnostics directory.
#'
#' @keywords internal
check_coal_annual_bounds <- function(
  cons_combined,
  cons_yearly,
  diagnostics_folder = NULL,
  relative_tolerance = 0.05
) {
  series_keys <- c("iso2", "sector", "unit", "siec", "fuel", "year")
  annual <- cons_yearly %>%
    filter(fuel == FUEL_COAL, !is.na(values)) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(across(all_of(series_keys))) %>%
    summarise(annual_value = sum_or_na(values), .groups = "drop")

  monthly <- cons_combined %>%
    filter(fuel == FUEL_COAL, !is.na(values)) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(across(all_of(series_keys))) %>%
    summarise(
      month_count = n_distinct(time),
      monthly_value = sum(values),
      monthly_source_months = sum(source == "monthly"),
      .groups = "drop"
    ) %>%
    filter(month_count == 12)

  reconciliation <- monthly %>%
    inner_join(annual, by = series_keys) %>%
    mutate(
      difference = monthly_value - annual_value,
      zero_denominator = annual_value == 0,
      relative_difference = if_else(
        zero_denominator,
        NA_real_,
        difference / abs(annual_value)
      ),
      source = case_when(
        monthly_source_months == 12 ~ "monthly",
        monthly_source_months == 0 ~ "yearly",
        TRUE ~ "mixed"
      ),
      within_bounds = case_when(
        zero_denominator ~ abs(difference) <= 1e-6,
        TRUE ~ abs(relative_difference) <= relative_tolerance
      )
    )

  if (!is_null_or_empty(diagnostics_folder)) {
    readr::write_csv(
      reconciliation,
      file.path(diagnostics_folder, "coal_annual_reconciliation.csv")
    )
  }

  out_of_bounds <- reconciliation %>% filter(!within_bounds)
  if (nrow(out_of_bounds) > 0) {
    log_warn(paste0(
      "Coal monthly totals differ from annual balances by more than ",
      round(relative_tolerance * 100), "% for ", nrow(out_of_bounds),
      " complete country-sector series. See coal_annual_reconciliation.csv."
    ))
  }

  reconciliation
}


#' Apply seasonal adjustment to convert yearly data to monthly data
#'
#' This function takes yearly consumption data and applies seasonal adjustment
#' using monthly shares calculated from monthly data to convert it to monthly data.
#'
#' @param cons_yearly Yearly consumption data
#' @param cons_monthly Monthly consumption data used to calculate seasonal patterns
#' @return Monthly data derived from yearly data with seasonal adjustment applied
#' @export
#'
#' @examples
apply_seasonal_adjustment <- function(cons_yearly, cons_monthly) {
  # Check that there is no na value
  if (
    any(!complete.cases(select(cons_monthly, -values))) ||
      any(!complete.cases(select(cons_yearly, -values)))
  ) {
    stop("There are NA values in the data (except in the 'values' column).")
  }

  # Calculate monthly shares for seasonal adjustment
  month_shares <- cons_monthly %>%
    group_by(iso2, sector, siec, unit, fuel, year = lubridate::year(time)) %>%
    mutate(count = n(), complete = count == 12 & all(!is.na(values))) %>%
    filter(complete) %>%
    group_by(sector, siec, unit, iso2, fuel, month = lubridate::month(time)) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
    group_by(sector, siec, unit, iso2, fuel) %>%
    mutate(month_share = values / sum(values, na.rm = TRUE)) %>%
    mutate(
      month_share = replace_na(month_share, 1 / 12),
      month_share = case_when(
        is.infinite(month_share) ~ 1 / 12,
        T ~ month_share
      )
    ) %>%
    select(-c(values))

  # An annual coal total whose sector split is unavailable has sector
  # `unknown`, even when historical monthly years were fully allocated between
  # electricity and other uses. Derive a total-consumption profile from those
  # reported monthly sectors so the known annual total can still be retained.
  coal_total_shares <- cons_monthly %>%
    filter(
      siec %in% COAL_MONTHLY_GAP_FUELS,
      sector %in% c(SECTOR_ELEC, SECTOR_OTHERS, SECTOR_UNKNOWN)
    ) %>%
    group_by(iso2, siec, unit, fuel, time) %>%
    summarise(
      values = case_when(
        any(sector == SECTOR_UNKNOWN & !is.na(values)) ~
          sum(values[sector == SECTOR_UNKNOWN], na.rm = TRUE),
        any(sector == SECTOR_ELEC & !is.na(values)) &
          any(sector == SECTOR_OTHERS & !is.na(values)) ~
          sum(values[sector %in% c(SECTOR_ELEC, SECTOR_OTHERS)], na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    ) %>%
    group_by(iso2, siec, unit, fuel, year = lubridate::year(time)) %>%
    mutate(count = n(), complete = count == 12 & all(!is.na(values))) %>%
    filter(complete) %>%
    group_by(iso2, siec, unit, fuel, month = lubridate::month(time)) %>%
    summarise(values = sum(values), .groups = "drop") %>%
    group_by(iso2, siec, unit, fuel) %>%
    mutate(
      month_share = values / sum(values),
      month_share = case_when(
        is.na(month_share) | is.infinite(month_share) ~ 1 / 12,
        TRUE ~ month_share
      ),
      sector = SECTOR_UNKNOWN
    ) %>%
    select(-values) %>%
    anti_join(
      month_shares %>%
        filter(sector == SECTOR_UNKNOWN) %>%
        distinct(iso2, sector, siec, unit, fuel, month),
      by = c("iso2", "sector", "siec", "unit", "fuel", "month")
    )
  month_shares <- bind_rows(month_shares, coal_total_shares)

  # Validate that monthly shares sum to approximately 1
  if (!all(month_shares %>%
    group_by(sector, siec, unit, iso2, fuel) %>%
    summarise(one = round(sum(month_share), 5), .groups = "drop") %>%
    pull(one) %>%
    unique() == 1)) {
    stop("Wrong monthly shares")
  }

  # Apply monthly adjustment
  cons_yearly_monthly <- cons_yearly %>%
    mutate(year = lubridate::year(time)) %>%
    inner_join(
      month_shares,
      relationship = "many-to-many"
    ) %>%
    arrange(sector, siec, unit, iso2, fuel, time) %>%
    mutate(
      time = as.Date(sprintf("%s-%0d-01", year, month)),
      values = values * month_share
    ) %>%
    select(-c(year, month, month_share))

  coal <- coal_allocate_annual(cons_yearly, cons_monthly)
  result <- bind_rows(
    cons_yearly_monthly %>% filter(!siec %in% COAL_MONTHLY_GAP_FUELS |
      !iso2 %in% get_eu_iso2s(include_eu = TRUE)), coal
  )
  attr(result, "coal_allocation") <- attr(coal, "coal_allocation")
  result
}


#' Some EUROSTAT data has 0 for last month even though it isn't
#' We remove it
#'
#' @param cons_agg
#'
#' @return
#' @export
#'
#' @examples
remove_last_incomplete <- function(cons) {
  max_months <- 6
  cons %>%
    group_by(iso2, sector, unit, siec, fuel) %>%
    arrange(desc(time)) %>%
    mutate(cumsum = cumsum(coalesce(values, 0))) %>%
    filter(
      is.na(values) | cumsum != 0 | max(cumsum) == 0 | row_number() >= max_months
    ) %>%
    ungroup() %>%
    select(-c(cumsum))
}


#' Get industrial production: useful to predict coal use in non-electricity sectors
#' for when data is missing (industrial output data seems to be a couple months ahead)
#'
#' @param diagnostics_folder
#' @param use_cache
#' @param iso2s
#' @param data_masking One of `DATA_MASKING_NONE` or
#'   `DATA_MASKING_HISTORICAL_DEFAULTS`, or a named masking config list in the
#'   same structure as `get_data_masking_config()`.
#'
#' @return
#' @export
#'
#' @examples
get_eurostat_indprod <- function(
  diagnostics_folder = NULL,
  use_cache = FALSE,
  iso2s = NULL,
  data_masking = DATA_MASKING_NONE
) {
  data_masking <- .resolve_data_masking_config(
    data_masking = data_masking
  )
  indprod_raw <- eurostat_data_access_get_indprod(
    use_cache = use_cache,
    iso2s = iso2s,
    data_masking = data_masking
  )


  if (!is_null_or_empty(diagnostics_folder)) {
    diagnostic_eurostat_indprod(
      indprod_raw,
      diagnostics_folder = diagnostics_folder
    )
  }

  return(indprod_raw)
}


#' Combine monthly and yearly data with cutoff filtering
#'
#' This function combines monthly and yearly data, applying cutoff dates to filter
#' out monthly data that is considered unreliable before certain dates.
#'
#' @param cons_yearly_monthly Yearly data converted to monthly using seasonal adjustment
#' @param cons_monthly Original monthly data
#' @return Combined data with cutoff filtering applied
#' @export
#'
#' @examples
combine_monthly_yearly_with_cutoff <- function(cons_yearly_monthly, cons_monthly) {
  # Define cutoff dates for different fuel types
  # Monthly data is quite incomplete/chaotic before ~2020, and sometimes a bit after
  # Look at diagnostic charts for more details
  cutoff_monthly <- tibble(
    siec = c(
      SIEC_NATURAL_GAS,
      SIEC_COKE_OVEN_COKE,
      SIEC_KEROSENE_XBIO,
      SIEC_HARD_COAL
    ),
    cutoff_date = c("2020-01-01", "2019-01-01", "2019-01-01", "2020-01-01"),
    source = "monthly"
  ) %>%
    # Add default cutoff date for all other siec_codes
    tidyr::complete(
      siec = unique(cons_yearly_monthly$siec),
      source,
      fill = list(cutoff_date = "2020-01-01")
    ) %>%
    tidyr::crossing(iso2 = unique(add_iso2(cons_yearly_monthly)$iso2)) %>%
    left_join(cons_yearly_monthly %>% distinct(siec, fuel))

  # Apply country-specific fixes
  cutoff_monthly <- cutoff_monthly %>%
    mutate(
      cutoff_date = case_when(
        # Fuel oil is quite oscillating in Portugal before 2023
        # Risk is that validation then isn't relevant as mostly on yearly data
        iso2 == "PT" & fuel == FUEL_OIL ~ "2023-01-01",
        T ~ cutoff_date
      )
    )

  # Combine monthly and yearly data with cutoff filtering
  cons_combined <- bind_rows(
    cons_yearly_monthly %>% mutate(source = "yearly"),
    cons_monthly %>% mutate(source = "monthly") %>% filter(),
  ) %>%
    # Cut off monthly that didn't look good on charts
    left_join(cutoff_monthly) %>%
    filter(
      is.na(cutoff_date) | time >= cutoff_date
    ) %>%
    select(-c(cutoff_date)) %>%
    group_by(iso2, sector, time, unit, siec, fuel) %>%
    arrange(is.na(values), source) %>% # Prefer usable monthly, then usable yearly.
    slice(1) %>%
    ungroup()

  return(cons_combined)
}
