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
get_eurostat_cons <- function(diagnostics_folder = "diagnostics",
                              use_cache = F,
                              iso2s = NULL) {
  consumption_codes_monthly <- c("nrg_cb_sffm", "nrg_cb_oilm", "nrg_cb_gasm")
  cons_monthly_raw <- consumption_codes_monthly %>%
    lapply(get_eurostat_from_code, use_cache = use_cache)
  names(cons_monthly_raw) <- c(FUEL_COAL, FUEL_OIL, FUEL_GAS)

  consumption_codes_yearly <- c("nrg_cb_sff", "nrg_cb_gas")
  cons_yearly_raw <- consumption_codes_yearly %>%
    lapply(get_eurostat_from_code, use_cache = use_cache)
  names(cons_yearly_raw) <- c("coal", "gas")

  cons_yearly_raw$oil <- lapply(
    c("O4600", "O4100_TOT_4200-4500"),
    function(x) {
      get_eurostat_from_code(
        code = "nrg_cb_oil",
        use_cache = use_cache,
        filters = list(siec = x)
      )
    }
  ) %>%
    bind_rows()

  # Add Final consumption - non energy use to gas monthly data
  cons_monthly_raw$gas <- add_gas_non_energy(
    cons_monthly_raw_gas = cons_monthly_raw$gas,
    cons_yearly_raw_gas = cons_yearly_raw$gas
  )

  # Add missing 2019 elec data for gas
  cons_monthly_raw$gas <- fill_ng_elec_eu27(cons_monthly_raw_gas = cons_monthly_raw$gas)

  process_coal_monthly <- function(x) {
    # This one is a bit tricky: for certain months/regions,
    # EUROSTAT has gross inland deliveries data but no transformation/consumption data
    # We should make sure to filter out these months so that it's not considered months without coal
    # consumption
    trans_cons <- x %>%
      filter(
        (grepl("Transformation input|Final consumption.*(industry sector$|other sectors$)", nrg_bal)) |
          (nrg_bal == "Final consumption - industry sector - iron and steel" & siec == "Coke oven coke")
      ) %>%
      mutate(
        sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_OTHERS),
        fuel = ifelse(siec == "Coke oven coke", FUEL_COKE, FUEL_COAL)
      ) %>%
      # Date valid only from 2014
      filter(time >= "2014-01-01")

    gross_inland_deliv <- x %>%
      filter(
        grepl("Gross inland deliveries", nrg_bal),
        time >= "2014-01-01"
      ) %>%
      group_by(geo, time, siec) %>%
      summarise(value_siec_gid = mean(values, na.rm = T)) # mean of observed and calculated

    valid <- trans_cons %>%
      group_by(geo, time, siec, sector) %>%
      summarise(value_siec_sector = sum(values, na.rm = T)) %>%
      mutate(value_siec = sum(value_siec_sector)) %>%
      left_join(gross_inland_deliv, by = c("geo", "time", "siec")
      )  %>%
      mutate(ratio_siec = value_siec / value_siec_gid) %>%
      #############################################################
      # This is the key criteria: Need value, and not too far from
      #############################################################
      mutate(valid = value_siec_sector > 0 | value_siec_gid == 0 | ratio_siec > 0.7)


    # Plot heat map of validity
    ggplot(valid) +
      geom_tile(aes(time, geo, fill = valid), color=NA) +
      facet_wrap(sector~siec) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "grey", "NA" = "black")) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()
      )

    # Return only valid records
    trans_cons %>%
      inner_join(
        valid %>%
          filter(valid) %>%
          select(geo, time, siec, sector)
      )
  }

  process_coal_yearly <- function(x) {
    x %>%
      filter(nrg_bal %in% c(
        "Transformation input - energy use",
        "Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use",
        "Transformation input - electricity and heat generation - main activity producer electricity only - energy use",
        "Final consumption - energy use"
      )) %>%
      mutate(sector = ifelse(grepl("electricity", nrg_bal), SECTOR_ELEC, SECTOR_ALL),
             fuel = FUEL_COAL)
  }


  process_oil <- function(x) {
    # Gross inland deliveries - energy use' is not available anymore starting from 2023
    # This is the one we need though...
    # we estimate GID_ENERGY = GID - Gross delivery to refinery, it worked pretty well
    mult <- x %>%
      ungroup() %>%
      filter(
        (nrg_bal %in% c(
          "Gross inland deliveries - observed",
          "Gross deliveries to petrochemical industry"
        ) & siec == "Oil products") |
          (grepl("Direct use", nrg_bal) & grepl("Crude oil, NGL", siec))
      ) %>%
      distinct(siec_code, nrg_bal_code) %>%
      mutate(factor = case_when(
        nrg_bal_code == "GD_PI" ~ -1,
        T ~ 1
      ))

    if (nrow(mult) != 3) stop("Please fix process_oil")

    x %>%
      inner_join(mult) %>%
      mutate(
        values = values * factor,
        sector = SECTOR_ALL,
        fuel = FUEL_OIL
      ) %>%
      select(-c(factor)) %>%
      arrange(desc(time)) %>%
      filter(!is.na(values))
  }

  process_oil_monthly <- function(x) {
    process_oil(x)
  }

  process_oil_yearly <- function(x) {
    process_oil(x)
  }

  process_gas <- function(x) {
    # Monthly data only valid from 2014, way too low before
    x <- x %>%
      filter(freq != "Monthly" | time >= "2014-01-01")


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
      filter(time >= min(x_all$time))

    bind_rows(
      x_all,
      x_elec
    )
  }

  process_gas_monthly <- function(x) {
    process_gas(x)
  }

  process_gas_yearly <- function(x) {
    process_gas(x)
  }

  split_elec_others <- function(x) {
    x %>%
      mutate(values = values * ifelse(sector == SECTOR_ELEC, -1, 1)) %>%
      group_by(geo, time, unit, siec, fuel) %>%
      summarise_at("values", sum, na.rm = T) %>%
      mutate(sector = SECTOR_OTHERS) %>%
      bind_rows(x %>% filter(sector == SECTOR_ELEC))
  }

  aggregate <- function(x) {
    x %>%
        group_by(geo, sector, time, unit, siec, fuel) %>%
        summarise_at("values", sum, na.rm = T) %>%
      ungroup()
  }

  cons_monthly <- list(
    coal = process_coal_monthly(cons_monthly_raw$coal),
    oil = process_oil_monthly(cons_monthly_raw$oil),
    gas = process_gas_monthly(cons_monthly_raw$gas) %>% split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate()

  cons_yearly <- list(
    coal = process_coal_yearly(cons_yearly_raw$coal) %>% split_elec_others(),
    oil = process_oil_yearly(cons_yearly_raw$oil),
    gas = process_gas_yearly(cons_yearly_raw$gas) %>% split_elec_others()
  ) %>%
    bind_rows() %>%
    aggregate()

  # Seasonal adjusment
  month_shares <- cons_monthly %>%
      group_by(sector, siec, unit, geo, fuel, year = lubridate::year(time)) %>%
      mutate(count = n()) %>%
      filter(count == 12) %>%
      group_by(sector, siec, unit, geo, fuel, month = lubridate::month(time)) %>%
      summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
      group_by(sector, siec, unit, geo, fuel) %>%
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
    group_by(sector, siec, unit, geo, fuel) %>%
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
      arrange(sector, siec, unit, geo, fuel, time) %>%
      mutate(
        time = as.Date(sprintf("%s-%0d-01", year, month)),
        values = values * month_share
      ) %>%
      select(-c(year, month, month_share))

  # Combine
  cons_combined <- bind_rows(
    cons_yearly_monthly %>% mutate(source = "yearly"),
    cons_monthly %>% mutate(source = "monthly"),
  ) %>%
    ungroup()

  # Visual check
  diagnostic_eurostat_cons_yearly_monthly(
    cons_yearly = cons_yearly,
    cons_monthly = cons_monthly,
    cons_combined = cons_combined
  )

  # Keep monthly when both are available
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

  return(cons)
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
add_gas_non_energy <- function(cons_monthly_raw_gas, cons_yearly_raw_gas) {
  shares <- cons_yearly_raw_gas %>%
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

  # Project til now
  years <- unique(year(cons_monthly_raw_gas$time))
  shares_filled <- shares %>%
    tidyr::complete(year = years, geo, siec) %>%
    group_by(geo, siec) %>%
    arrange(year) %>%
    fill(share_non_energy) %>%
    ungroup()


  cons_monthly_raw_gas_non_energy <- cons_monthly_raw_gas %>%
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
    cons_monthly_raw_gas,
    cons_monthly_raw_gas_non_energy
  ))
}


#' NG for Elec for EU27 is missing in 2019, simply because CY 0 data is missing...
#'
#' @param cons_monthly_raw_gas
#'
#' @return
#' @export
#'
#' @examples
fill_ng_elec_eu27 <- function(cons_monthly_raw_gas) {
  nrg_bal_elec <- "Transformation input - electricity and heat generation - main activity producers"

  eu27_ng_elec_new <- cons_monthly_raw_gas %>%
    add_iso2() %>%
    filter(nrg_bal == nrg_bal_elec) %>%
    filter(iso2 %in% get_eu_iso2s()) %>%
    group_by(unit, time) %>%
    summarise(values = sum(values, na.rm = T), .groups = "drop")

  eu27_ng_elec_old <- cons_monthly_raw_gas %>%
    add_iso2() %>%
    filter(nrg_bal == nrg_bal_elec) %>%
    filter(iso2 == "EU")

  ggplot(bind_rows(
    eu27_ng_elec_new %>% mutate(source = "new"),
    eu27_ng_elec_old %>% mutate(source = "old"),
  )) +
    geom_line(aes(time, values, col = source, linetype = source)) +
    facet_wrap(~unit)


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
    cons_monthly_raw_gas %>% add_iso2() %>% filter(nrg_bal != nrg_bal_elec | iso2 != "EU"),
    eu27_ng_elec
  )
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
    ungroup()
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
get_eurostat_indprod <- function(diagnostics_folder = "diagnostics",
                                 use_cache = F,
                                 iso2s = NULL) {
  indprod_raw <- get_eurostat_from_code(code = "sts_inpr_m", use_cache = use_cache) %>%
    add_iso2()
  return(indprod_raw)
}


get_eurostat_from_code <- function(code, use_cache = T, filters = NULL) {
  filepath <- file.path("cache", paste0("eurostat_", code, ".RDS"))
  dir.create("cache", F, T)

  if (use_cache & file.exists(filepath)) {
    return(readRDS(filepath))
  }


  raw <- eurostat::get_eurostat(code, filters = filters)
  keep_code <- intersect(names(raw), c("nrg_bal", "siec", "nace_r2"))
  if (length(keep_code) == 0) keep_code <- NULL
  raw %>%
    eurostat::label_eurostat(code = keep_code, fix_duplicated = T) %>% # Keep nrg_bal code as well
    dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD"))) %T>% # Column changed with new EUROSTAT version
    saveRDS(filepath)
}
