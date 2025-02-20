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
    pwr_demand,
    diagnostics_folder = "diagnostics/eurostat",
    use_cache = F,
    iso2s = NULL) {

  create_dir(diagnostics_folder)


  # Get monthly and yearly data
  cons_raw_oil <- collect_oil(use_cache = use_cache)
  cons_raw_solid <- collect_solid(use_cache = use_cache)
  cons_raw_gas <- collect_gas(use_cache = use_cache)

  # Check siec and siec_code are full and univoqual
  check_siec_siec_code <- function(x) {
    y <- distinct(x, siec, siec_code)
    stopifnot(!any(is.na(y$siec_code)))
    stopifnot(!any(is.na(y$siec)))
    stopifnot(!any(duplicated(y$siec_code)))
    stopifnot(!any(duplicated(y$siec)))
  }
  check_siec_siec_code(cons_raw_oil$monthly)
  check_siec_siec_code(cons_raw_oil$yearly)
  check_siec_siec_code(cons_raw_solid$monthly)
  check_siec_siec_code(cons_raw_solid$yearly)
  check_siec_siec_code(cons_raw_gas$monthly)
  check_siec_siec_code(cons_raw_gas$yearly)


  split_elec_others <- function(x) {
    x %>%
      mutate(values = values * ifelse(sector == SECTOR_ELEC, -1, 1)) %>%
      group_by(geo, time, unit, siec, siec_code, fuel) %>%
      summarise(values=sum(values, na.rm = T),
                   n=sum(!is.na(values))
                   ) %>%
      mutate(sector = SECTOR_OTHERS) %>%
      bind_rows(x %>% filter(sector == SECTOR_ELEC))
  }

  aggregate <- function(x) {
    x %>%
        group_by(geo, sector, time, unit, siec, siec_code, fuel) %>%
        summarise_at("values", sum, na.rm = T) %>%
      ungroup()
  }

  # Process data
  cons_monthly <- list(
    oil = process_oil_monthly(cons_raw_oil$monthly),
    solid = process_solid_monthly(cons_raw_solid$monthly, pwr_demand=pwr_demand), # %>% split_elec_others(),
    gas = process_gas_monthly(cons_raw_gas$monthly, pwr_demand=pwr_demand) %>% split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate()

  cons_yearly <- list(
    solid = process_solid_yearly(cons_raw_solid$yearly), # %>% split_elec_others(),
    oil = process_oil_yearly(cons_raw_oil$yearly),
    gas = process_gas_yearly(cons_raw_gas$yearly, pwr_demand=pwr_demand) %>% split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate()

  # Seasonal adjusment
  month_shares <- cons_monthly %>%
      group_by(sector, siec, siec_code, unit, geo, fuel, year = lubridate::year(time)) %>%
      mutate(count = n()) %>%
      filter(count == 12) %>%
      group_by(sector, siec, siec_code, unit, geo, fuel, month = lubridate::month(time)) %>%
      summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
      group_by(sector, siec, siec_code, unit, geo, fuel) %>%
      mutate(month_share = values / sum(values, na.rm = T)) %>%
      mutate(
        month_share = replace_na(month_share, 1 / 12),
        month_share = case_when(
          is.infinite(month_share) ~ 1 / 12,
          T ~ month_share
        )
      ) %>%
      select(-c(values))

  # Check ~1
  if (!all(month_shares %>%
    group_by(sector, siec, siec_code, unit, geo, fuel) %>%
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
      arrange(sector, siec, siec_code, unit, geo, fuel, time) %>%
      mutate(
        time = as.Date(sprintf("%s-%0d-01", year, month)),
        values = values * month_share
      ) %>%
      select(-c(year, month, month_share))

  # Keep monthly when both are available
  # unless specified otherwise after looking at diagnostic charts
  cutoff_monthly <- tibble(
    siec_code=c(SIEC_NATURAL_GAS, SIEC_COKE_OVEN_COKE, SIEC_KEROSENE_XBIO),
    # sector=c("others", "others"),
    cutoff_date=c("2020-01-01", "2019-01-01", "2019-01-01"),
    source="monthly"
  )

  cons_combined <- bind_rows(
    cons_yearly_monthly %>% mutate(source = "yearly"),
    cons_monthly %>% mutate(source = "monthly"),
  ) %>%
    # Cut off monthly that didn't look good on charts
    left_join(cutoff_monthly) %>%
    filter(
      is.na(cutoff_date) | time >= cutoff_date
    ) %>%
    select(-c(cutoff_date)) %>%
    group_by(geo, sector, time, unit, siec, siec_code, fuel) %>%
    arrange(source) %>% # monthly < yearly
    slice(1) %>%
    ungroup()

  if(!is.null(diagnostics_folder)){
    # Visual check
    diagnostic_eurostat_cons_yearly_monthly(
      diagnostics_folder = diagnostics_folder,
      cons_yearly = cons_yearly,
      cons_monthly = cons_monthly,
      cons_combined = cons_combined
    )
  }

  cons <- cons_combined %>%
    group_by(geo, sector, time, unit, siec, fuel) %>%
    arrange(source) %>%
    slice(1) %>%
    ungroup() %>%
    select(-c(source))

  # Add infos
  cons <- cons %>%
    add_iso2() %>%
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
  if(!is.null(diagnostics_folder)){
    diagnostic_eurostat_cons(cons,
                             iso2s=iso2s,
                             diagnostics_folder = diagnostics_folder)
  }


  return(cons)
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
    group_by(geo, sector, unit, siec, fuel) %>%
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


  if(!is.null(diagnostics_folder)){
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

