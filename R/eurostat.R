#' Get monthly fossil fuel consumption per country from EUROSTAT energy balance
#' Update on 27 April 2024: Eurostat stopped working with nrg_cb_oil for all countries
#' -> we add a geo=EU27_2020 filter, which will prevent us from updating national numbers for now
#'
#' @param diagnostics_folder
#' @param use_cache
#' @param iso2s
#'
#' @return
#' @export
#'
#' @examples
get_eurostat_cons <- function(
    pwr_generation,
    diagnostics_folder = "diagnostics/eurostat",
    use_cache = F,
    iso2s = NULL) {

  create_dir(diagnostics_folder)


  # Get monthly and yearly data
  cons_raw_oil <- collect_oil(use_cache = use_cache)
  cons_raw_solid <- collect_solid(use_cache = use_cache)
  cons_raw_gas <- collect_gas(use_cache = use_cache)

  # Check siec and siec_code are full and univoqual
  # That all iso2s are included
  check_siec_siec_code <- function(x) {
    y <- distinct(x, siec, siec_code)
    stopifnot(!any(is.na(y$siec_code)))
    stopifnot(!any(is.na(y$siec)))
    stopifnot(!any(duplicated(y$siec_code)))
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
        group_by(iso2, sector, time, unit, siec_code, fuel) %>%
        summarise_at("values", sum, na.rm = T) %>%
      ungroup()
  }

  # Collect siecs correpondence table
  siecs <- bind_rows(
    cons_raw_oil$yearly,
    cons_raw_solid$yearly,
    cons_raw_gas$yearly
  ) %>%
    ungroup() %>%
    filter(!is.na(siec)) %>%
    select(siec, siec_code) %>%
    distinct()

  # Process data
  cons_monthly <- list(
    oil = process_oil_monthly(cons_raw_oil$monthly),
    solid = process_solid_monthly(cons_raw_solid$monthly, pwr_generation=pwr_generation) %>% eurostat_split_elec_others(),
    gas = process_gas_monthly(cons_raw_gas$monthly, pwr_generation=pwr_generation) %>% eurostat_split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate() %>%
    add_iso2() %>%
    select(iso2, sector, time, unit, siec_code, fuel, values)

  cons_yearly <- list(
    oil = process_oil_yearly(cons_raw_oil$yearly),
    solid = process_solid_yearly(cons_raw_solid$yearly) %>% eurostat_split_elec_others(),
    gas = process_gas_yearly(cons_raw_gas$yearly, pwr_generation=pwr_generation) %>% eurostat_split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate() %>%
    add_iso2() %>%
    select(iso2, sector, time, unit, siec_code, fuel, values)

  # Check that there is no na value
  if (any(!complete.cases(cons_monthly)) | any(!complete.cases(cons_yearly))) {
    stop("There are NA values in the data (except in the 'values' column).")
  }

  # Apply seasonal adjustment to convert yearly data to monthly
  cons_yearly_monthly <- apply_seasonal_adjustment(cons_yearly, cons_monthly)

  # Combine monthly and yearly data with cutoff filtering
  cons_combined <- combine_monthly_yearly_with_cutoff(cons_yearly_monthly, cons_monthly)

  if(!is_null_or_empty(diagnostics_folder)){
    # Visual check
    diagnostic_eurostat_cons_yearly_monthly(
      diagnostics_folder = diagnostics_folder,
      cons_yearly = cons_yearly %>% left_join(siecs),
      cons_monthly = cons_monthly %>% left_join(siecs),
      cons_combined = cons_combined %>% left_join(siecs),
      detailed_iso2s = c("BE", "NL", "PT", "SK", "EU", "IE")
    )
  }

  cons <- cons_combined %>%
    group_by(iso2, sector, time, unit, siec_code, fuel) %>%
    arrange(source) %>%
    slice(1) %>%
    ungroup() %>%
    select(-c(source)) %>%
    left_join(siecs)

  # Add infos
  cons <- cons %>%
    recode_siec()

  # Keep regions of interest
  if (!is.null(iso2s)) {
    cons <- cons %>%
      filter(iso2 %in% iso2s)
  }

  # Remove last incomplete month for each region
  cons <- cons %>%
    remove_last_incomplete()


  # Other diagnostics
  if(!is_null_or_empty(diagnostics_folder)){
    diagnostic_eurostat_cons(cons,
                             iso2s=iso2s,
                             diagnostics_folder = diagnostics_folder)
  }


  return(cons)
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
  if (any(!complete.cases(cons_monthly)) | any(!complete.cases(cons_yearly))) {
    stop("There are NA values in the data (except in the 'values' column).")
  }

  # Calculate monthly shares for seasonal adjustment
  month_shares <- cons_monthly %>%
    group_by(iso2, sector, siec_code, unit, fuel, year = lubridate::year(time)) %>%
    mutate(count = n()) %>%
    filter(count == 12) %>%
    group_by(sector, siec_code, unit, iso2, fuel, month = lubridate::month(time)) %>%
    summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
    group_by(sector, siec_code, unit, iso2, fuel) %>%
    mutate(month_share = values / sum(values, na.rm = T)) %>%
    mutate(
      month_share = replace_na(month_share, 1 / 12),
      month_share = case_when(
        is.infinite(month_share) ~ 1 / 12,
        T ~ month_share
      )
    ) %>%
    select(-c(values))

  # Validate that monthly shares sum to approximately 1
  if (!all(month_shares %>%
    group_by(sector, siec_code, unit, iso2, fuel) %>%
    summarise(one = round(sum(month_share), 5), .groups = "drop") %>%
    pull(one) %>%
    unique() == 1)) {
    stop("Wrong monthly shares")
  }

  # Apply monthly adjustment
  cons_yearly_monthly <- cons_yearly %>%
    mutate(year = lubridate::year(time)) %>%
    inner_join(month_shares,
      relationship = "many-to-many"
    ) %>%
    arrange(sector, siec_code, unit, iso2, fuel, time) %>%
    mutate(
      time = as.Date(sprintf("%s-%0d-01", year, month)),
      values = values * month_share
    ) %>%
    select(-c(year, month, month_share))

  return(cons_yearly_monthly)
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
    group_by(iso2, sector, unit, siec_code, fuel) %>%
    arrange(desc(time)) %>%
    mutate(cumsum = cumsum(values)) %>%
    filter(cumsum != 0 | max(cumsum) == 0 | row_number() >= max_months) %>%
    ungroup() %>%
    select(-c(cumsum))
}


#' Get industrial production: useful to predict coal use in non-electricity sectors
#' for when data is missing (industrial output data seems to be a couple months ahead)
#'
#' @param diagnostics_folder
#' @param use_cache
#' @param iso2s
#'
#' @return
#' @export
#'
#' @examples
get_eurostat_indprod <- function(diagnostics_folder = NULL,
                                 use_cache = F,
                                 iso2s = NULL) {

  indprod_raw <- get_eurostat_from_code(code = "sts_inpr_m", use_cache = use_cache) %>%
    add_iso2()


  if(!is_null_or_empty(diagnostics_folder)){
    diagnostic_eurostat_indprod(indprod_raw,
                                diagnostics_folder = diagnostics_folder)
  }

  return(indprod_raw)
}


get_eurostat_from_code <- function(code, iso2s=NULL, use_cache = T, filters = NULL) {

  # Create a digest of iso2s and filters
  create_dir("cache")
  digest <- digest::digest(list(iso2s, filters))
  filepath <- file.path("cache", glue("eurostat_{code}_{digest}.rds"))

  if (use_cache & file.exists(filepath)) {
    return(readRDS(filepath))
  }

  if(!is.null(iso2s)){
    filters$geo <- iso2s
  }

  raw <- eurostat::get_eurostat(code, filters = filters, keepFlags=T)
  keep_code <- intersect(names(raw), c("nrg_bal", "siec", "nace_r2", "prod_nrg"))
  if (length(keep_code) == 0) keep_code <- NULL
  raw %>%
    eurostat::label_eurostat(code = keep_code, fix_duplicated = T) %>% # Keep nrg_bal code as well
    dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD"))) %T>% # Column changed with new EUROSTAT version
    saveRDS(filepath)
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
    siec_code=c(SIEC_NATURAL_GAS,
                SIEC_COKE_OVEN_COKE,
                SIEC_KEROSENE_XBIO,
                SIEC_HARD_COAL
                ),
    cutoff_date=c("2020-01-01", "2019-01-01", "2019-01-01", "2020-01-01"),
    source="monthly"
  ) %>%
    # Add default cutoff date for all other siec_codes
    tidyr::complete(siec_code=unique(cons_yearly_monthly$siec_code),
                    source,
                    fill=list(cutoff_date="2020-01-01")
                    ) %>%
    tidyr::crossing(iso2=unique(add_iso2(cons_yearly_monthly)$iso2)) %>%
    left_join(cons_yearly_monthly %>% distinct(siec_code, fuel))

  # Apply country-specific fixes
  cutoff_monthly <- cutoff_monthly %>%
    mutate(
      cutoff_date = case_when(
        # Fuel oil is quite oscillating in Portugal before 2023
        # Risk is that validation then isn't relevant as mostly on yearly data
        iso2 == "PT" & fuel==FUEL_OIL ~ "2023-01-01",
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
    group_by(iso2, sector, time, unit, siec_code, fuel) %>%
    arrange(source) %>% # monthly < yearly
    slice(1) %>%
    ungroup()

  return(cons_combined)
}

