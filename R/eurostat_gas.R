#' Collect gas consumption data from EUROSTAT
#'
#' @param use_cache Whether to use cached data
#' @return Raw gas consumption data from EUROSTAT
#' @export
collect_gas <- function(use_cache = FALSE) {
  # Monthly data
  consumption_codes_monthly <- c("nrg_cb_gasm")
  cons_monthly_raw <- get_eurostat_from_code(
    code = consumption_codes_monthly,
    use_cache = use_cache
  )

  # Yearly data - with specific filters for gas
  gas_nrg_bal_yearly <- c(
    "IC_OBS", # Inland consumption - observed
    "FC_NE", # Final consumption - non-energy use
    "TI_EHG_MAPE_E", # Transformation input - electricity and heat generation - main activity producers
    # Monthly data doesn't have distinction between elec and heat only. We then include it to ensuire continuity
    # even though this is technically innacurate
    "TI_EHG_MAPH_E", # Transformation input - electricity and heat generation - HEAT ONLY
    "TI_EHG_MAPCHP_E" # Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use
  )

  cons_yearly_raw <- get_eurostat_from_code(
    code = "nrg_cb_gas",
    use_cache = use_cache,
    filters = list(nrg_bal = gas_nrg_bal_yearly)
  )

  # Add Final consumption - non energy use to gas monthly data
  cons_monthly_raw <- add_gas_non_energy(
    cons_monthly_raw = cons_monthly_raw,
    cons_yearly_raw = cons_yearly_raw
  )

  # Add missing 2019 elec data for gas
  cons_monthly_raw <- fill_ng_elec_eu27(cons_monthly_raw = cons_monthly_raw)

  list(
    monthly = cons_monthly_raw,
    yearly = cons_yearly_raw
  )
}


 process_gas <- function(x, pwr_demand) {
    # Monthly data only valid from 2014, way too low before
    x <- x %>%
      filter(freq != "Monthly" | time >= "2015-01-01")


    x_all <- x %>%
      filter(
        nrg_bal %in% c(
          "Inland consumption - observed",
          "Final consumption - non-energy use"
        ),
        grepl("Terajoule", unit),
        siec == "Natural gas"
      ) %>%
      group_by(across(-c(nrg_bal, nrg_bal_code, values))) %>%
      summarise(
        values = sum(values * case_when(
          nrg_bal == "Inland consumption - observed" ~ 1,
          T ~ -1
        )),
        .groups = "drop"
      ) %>%
      mutate(
        sector = SECTOR_ALL,
        fuel = FUEL_GAS
      )

    # Get months with fossil gas generation
    # to remove EUROSTAT saying 0 while it is not (e.g. monthly Ireland in 2014)
    has_gas_generation <- pwr_demand %>%
      filter(source == "Fossil Gas") %>%
      group_by(iso2, time = floor_date(date, "month")) %>%
      summarise(value_mwh = sum(value_mwh, na.rm = T)) %>%
      mutate(
        value_mwh_threshold = quantile(value_mwh[value_mwh > 0], 0.1), # Could be due to stock changes
        has_gas_generation = value_mwh > value_mwh_threshold
      ) %>%
      ungroup() %>%
      tidyr::complete(iso2, time = unique(x$time), fill = list(has_gas_generation = FALSE, value_mwh = NA)) %>%
      select(iso2, time, has_gas_generation) %>%
      mutate(freq = "Monthly")


    x_elec <- x %>%
      filter(
        (freq == "Monthly" & nrg_bal == "Transformation input - electricity and heat generation - main activity producers") |
          (freq == "Annual" & nrg_bal %in% c(
            "Transformation input - electricity and heat generation - main activity producer electricity only - energy use",
            "Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use"
          )),
        grepl("Terajoule", unit),
        siec == "Natural gas"
      ) %>%
      mutate(
        sector = SECTOR_ELEC,
        fuel = FUEL_GAS
      ) %>%
      filter(time >= min(x_all$time)) %>%
      # Remove months with 0 if there is gas generation
      add_iso2() %>%
      left_join(has_gas_generation) %>%
      arrange(time) %>%
      filter(
        !(freq == 'Monthly' & has_gas_generation & values == 0)
      )

    bind_rows(
      x_all,
      x_elec
    )
  }

  process_gas_monthly <- function(x, pwr_demand) {
    process_gas(x, pwr_demand)
  }

  process_gas_yearly <- function(x, pwr_demand) {
    process_gas(x, pwr_demand)
  }


#' Monthly data is not as detailed as yearly one.
#' Most importantly, non-energy use
#' is available in yearly but not in monthly data.
#'
#' We use yearly ratio 'Gross inland deliveries - energy use' / 'Gross inland deliveries - observed'
#' to update monthly data
#'
#' A more accurate way would be to predict this ratio using industrial production index (e.g. fertiliser, petrochemicals)
#' For a later version.
#'
#' @param cons_yearly_raw
#' @param cons_monthly_raw
#'
#' @return
#' @export
#'
#' @examples
add_gas_non_energy <- function(cons_monthly_raw, cons_yearly_raw) {

  shares <- cons_yearly_raw %>%
    filter(time >= "1990-01-01") %>%
    filter(siec == "Natural gas") %>%
    filter(grepl("Terajoule", unit)) %>%
    filter(nrg_bal %in% c(
      "Inland consumption - observed",
      "Final consumption - non-energy use"
    )) %>%
    mutate(year = year(time)) %>%
    select(nrg_bal_code, siec, geo, year, values) %>%
    tidyr::spread(nrg_bal_code, values) %>%
    mutate(share_non_energy = tidyr::replace_na(FC_NE / IC_OBS, 0)) %>%
    select(-c(FC_NE, IC_OBS))

  shares %>%
    filter(geo=="Belgium") %>%
    ggplot(aes(year, share_non_energy)) +
    geom_line()+
    rcrea::scale_y_crea_zero()

  # Project til now
  years <- unique(year(cons_monthly_raw$time))
  shares_filled <- shares %>%
    tidyr::complete(year = years, geo, siec) %>%
    group_by(geo, siec) %>%
    arrange(year) %>%
    tidyr::fill(share_non_energy) %>%
    ungroup()


  cons_monthly_raw_non_energy <- cons_monthly_raw %>%
    filter(nrg_bal %in% c("Inland consumption - observed")) %>%
    filter(siec == "Natural gas") %>%
    mutate(year = year(time)) %>%
    inner_join(shares_filled) %>%
    mutate(
      nrg_bal = "Final consumption - non-energy use",
      nrg_bal_code = "FC_NE",
      values = values * share_non_energy
    ) %>%
    select(-c(share_non_energy, year))

  return(bind_rows(
    cons_monthly_raw,
    cons_monthly_raw_non_energy
  ))
}


#' NG for Elec for EU27 is missing in 2019, simply because CY 0 data is missing...
#'
#' @param cons_monthly_raw
#'
#' @return
#' @export
#'
#' @examples
fill_ng_elec_eu27 <- function(cons_monthly_raw) {
  nrg_bal_elec <- "Transformation input - electricity and heat generation - main activity producers"

  eu27_ng_elec_new <- cons_monthly_raw %>%
    add_iso2() %>%
    filter(nrg_bal == nrg_bal_elec) %>%
    filter(iso2 %in% get_eu_iso2s()) %>%
    group_by(unit, time) %>%
    summarise(values = sum(values, na.rm = T), .groups = "drop")

  eu27_ng_elec_old <- cons_monthly_raw %>%
    add_iso2() %>%
    filter(nrg_bal == nrg_bal_elec) %>%
    filter(iso2 == "EU")

  # ggplot(bind_rows(
  #   eu27_ng_elec_new %>% mutate(source = "new"),
  #   eu27_ng_elec_old %>% mutate(source = "old"),
  # )) +
  #   geom_line(aes(time, values, col = source, linetype = source)) +
  #   facet_wrap(~unit)


  eu27_ng_elec <- eu27_ng_elec_old %>%
    group_by(unit) %>%
    tidyr::complete(
      tidyr::nesting(time = seq.Date(min(time), max(time), by = "month")),
      tidyr::nesting(nrg_bal_code, nrg_bal, siec, freq),
      tidyr::nesting(geo, iso2)
    ) %>%
    left_join(eu27_ng_elec_new %>%
      select(unit, time, values_filler = values)) %>%
    mutate(values = coalesce(values, values_filler)) %>%
    select(-c(values_filler)) %>%
    ungroup()


  bind_rows(
    cons_monthly_raw %>% add_iso2() %>% filter(nrg_bal != nrg_bal_elec | iso2 != "EU"),
    eu27_ng_elec
  )
}
