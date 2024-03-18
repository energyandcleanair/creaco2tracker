

#' Get monthly fossil fuel consumption per country from EUROSTAT energy balance
#'
#' @param diagnostics_folder
#' @param use_cache
#' @param iso2s
#'
#' @return
#' @export
#'
#' @examples
get_eurostat_cons <- function(diagnostics_folder='diagnostics', use_cache=F, iso2s=NULL){

  consumption_codes_monthly = c("nrg_cb_sffm", "nrg_cb_oilm", "nrg_cb_gasm")
  cons_monthly_raw <- consumption_codes_monthly %>% lapply(get_eurostat_from_code, use_cache=use_cache)
  names(cons_monthly_raw) <- c('coal', 'oil', 'gas')

  consumption_codes_yearly = c("nrg_cb_sff", "nrg_cb_oil", "nrg_cb_gas")
  cons_yearly_raw <- consumption_codes_yearly %>% lapply(get_eurostat_from_code, use_cache=use_cache)
  names(cons_yearly_raw) <- c('coal', 'oil', 'gas')

  # Add Gross Inland - energy use to oil monthly data
  cons_monthly_raw$oil <- add_oil_gross_inland_energy(cons_monthly_raw_oil = cons_monthly_raw$oil,
                                                      cons_yearly_raw_oil = cons_yearly_raw$oil)

  # Add Final consumption - non energy use to gas monthly data
  cons_monthly_raw$gas <- add_gas_non_energy(cons_monthly_raw_gas = cons_monthly_raw$gas,
                                             cons_yearly_raw_gas = cons_yearly_raw$gas)

  # Add missing 2019 elec data for gas
  cons_monthly_raw$gas <- fill_ng_elec_eu27(cons_monthly_raw_gas = cons_monthly_raw$gas)

  filter_coal_monthly <- function(x){
    x %>%
      filter(
        (grepl('Transformation input|Final consumption.*(industry sector$|other sectors$)', nrg_bal)) |
          (nrg_bal=='Final consumption - industry sector - iron and steel' & siec=='Coke oven coke')) %>%
      mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'others')) %>%
      # Date valid only from 2014
      filter(time >= "2014-01-01")
  }

  filter_coal_yearly <- function(x){
    x %>%
      filter(nrg_bal %in% c(
        'Transformation input - energy use',
        'Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use',
        'Transformation input - electricity and heat generation - main activity producer electricity only - energy use',
        'Final consumption - energy use')) %>%
      mutate(sector=ifelse(grepl('electricity', nrg_bal), 'electricity', 'all'))
  }

  filter_oil_monthly <- function(x){
    x %>%
      filter(
        (nrg_bal == 'Gross inland deliveries - energy use' & siec=='Oil products') |
          (grepl('Direct use', nrg_bal) & grepl('Crude oil, NGL', siec))) %>%
      mutate(sector='all')
  }

  filter_oil_yearly <- function(x){
    x %>%
      filter(
        (nrg_bal == 'Gross inland deliveries - energy use' & siec=='Oil products') |
          (grepl('Direct use', nrg_bal) & grepl('Crude oil, NGL.*hydrocarbons$', siec))) %>%
      mutate(sector='all')
  }

  filter_gas <- function(x){

    # Monthly data only valid from 2014, way too low before
    x <- x %>%
      filter(freq != "Monthly" | time >= '2014-01-01')

    x %>%
      filter(nrg_bal %in% c('Inland consumption - observed',
                            'Final consumption - non-energy use'),
             grepl('Terajoule', unit),
             grepl('27', geo),
             siec=='Natural gas') %>%
      group_by(year=year(time), nrg_bal) %>%
      summarise(values=sum(values), .groups="drop") %>%
      ggplot() +
      geom_line(aes(year, values, col=nrg_bal))

    x_all <- x %>%
      filter(nrg_bal %in% c('Inland consumption - observed',
                            'Final consumption - non-energy use'),
             grepl('Terajoule', unit),
             siec=='Natural gas') %>%
      group_by(across(-c(nrg_bal, nrg_bal_code, values))) %>%
      summarise(values=sum(values * case_when(nrg_bal=='Inland consumption - observed' ~ 1,
                                              T ~ -1)),
                .groups="drop") %>%
      mutate(sector='all')

    x_elec <- x %>%
      filter(
        (freq == "Monthly" & nrg_bal == 'Transformation input - electricity and heat generation - main activity producers') |
          (freq == "Annual" & nrg_bal %in% c(
            'Transformation input - electricity and heat generation - main activity producer electricity only - energy use',
            'Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use')),
        grepl('Terajoule', unit),
        siec=='Natural gas') %>%
      mutate(sector='electricity') %>%
      filter(time >= min(x_all$time))

    bind_rows(
      x_all,
      x_elec
    )
  }

  filter_gas_monthly <- function(x){
    filter_gas(x)
  }

  filter_gas_yearly <- function(x){
    filter_gas(x)
  }

  split_elec_others <- function(x){
    x %>%
      mutate(values=values*ifelse(sector=='electricity', -1, 1)) %>%
      group_by(geo, time, unit, siec) %>%
      summarise_at('values', sum, na.rm=T) %>%
      mutate(sector='others') %>%
      bind_rows(x %>% filter(sector=='electricity'))
  }

  aggregate <- function(x){
    lapply(names(x), function(commodity){
      x[[commodity]] %>%
        group_by(geo, sector, time, unit, siec) %>%
        summarise_at('values', sum, na.rm=T) %>%
        mutate(fuel_type=commodity)
    }) %>%
      `names<-`(names(x))
  }

  cons_monthly <- list(
    coal = filter_coal_monthly(cons_monthly_raw$coal),
    oil = filter_oil_monthly(cons_monthly_raw$oil),
    gas = filter_gas_monthly(cons_monthly_raw$gas) %>% split_elec_others()
  ) %>%
    aggregate()

  cons_yearly <- list(
    coal = filter_coal_yearly(cons_yearly_raw$coal) %>% split_elec_others(),
    oil = filter_oil_yearly(cons_yearly_raw$oil),
    gas = filter_gas_yearly(cons_yearly_raw$gas) %>% split_elec_others()
  ) %>%
    aggregate()

  # Seasonal adjusment
  month_shares <- lapply(cons_monthly, function(cons){

    month_shares <- cons %>%
      group_by(sector, siec, unit, geo, fuel_type, year=lubridate::year(time)) %>%
      mutate(count=n()) %>%
      filter(count==12) %>%
      group_by(sector, siec, unit, geo, fuel_type, month=lubridate::month(time)) %>%
      summarise(values = sum(values, na.rm=T), .groups="drop") %>%
      group_by(sector, siec, unit, geo, fuel_type) %>%
      mutate(month_share = values / sum(values, na.rm=T)) %>%
      mutate(month_share = replace_na(month_share, 1/12),
             month_share = case_when(is.infinite(month_share) ~ 1/12,
                                     T ~ month_share)
      ) %>%
      select(-c(values))

    # Check ~1
    if(!all(month_shares %>%
            group_by(sector, siec, unit, geo, fuel_type) %>%
            summarise(one=round(sum(month_share), 5), .groups="drop") %>%
            pull(one) %>%
            unique() == 1)){stop('Wrong monthly shares')}

    return(month_shares)
  })


  # Apply monthly adjustment
  cons_yearly_monthly <- lapply(names(cons_yearly), function(commodity){
    cons_yearly[[commodity]] %>%
      mutate(year=lubridate::year(time)) %>%
      inner_join(month_shares[[commodity]],
                 relationship = "many-to-many") %>%
      arrange(sector, siec, unit, geo, fuel_type, time) %>%
      mutate(time=as.Date(sprintf('%s-%0d-01', year, month)),
             values=values * month_share)   %>%
      select(-c(year, month, month_share))
  }) %>%
    `names<-`(names(cons_yearly))

  # Combine
  cons_combined <- bind_rows(
    do.call(bind_rows, cons_yearly_monthly) %>% mutate(source='yearly'),
    do.call(bind_rows, cons_monthly) %>% mutate(source='monthly'),
  ) %>%
    ungroup()

  # Visual check
  diagnostic_eurostat_cons_yearly_monthly(
    cons_yearly = cons_yearly,
    cons_monthly = cons_monthly,
    cons_combined = cons_combined
  )

  # Keep monthly when both are available
  cons <- cons_combined  %>%
    group_by(geo, sector, time, unit, siec, fuel_type) %>%
    arrange(source) %>%
    slice(1) %>%
    ungroup() %>%
    select(-c(source))

  # Add infos
  cons <- cons %>%
    add_iso2() %>%
    recode_siec()

  # Keep regions of interest
  if(!is.null(iso2s)){
    cons <- cons %>%
      filter(iso2 %in% iso2s)
  }

  # Remove last incomplete month for each region
  cons <- cons %>%
    remove_last_incomplete()

  return(cons)
}


#' Monthly data is not as detailed as yearly one.
#' Most importantly, Gross inland deliveries - energy use which is what we're after
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
add_oil_gross_inland_energy <- function(cons_monthly_raw_oil, cons_yearly_raw_oil){

  shares <- cons_yearly_raw_oil %>%
    filter(time >= '2000-01-01') %>%
    filter(siec=='Oil products') %>%
    filter(nrg_bal %in% c('Gross inland deliveries - observed',
                          'Gross inland deliveries - energy use')) %>%
    mutate(year=year(time)) %>%
    select(nrg_bal_code, siec, unit, geo, year, values) %>%
    tidyr::spread(nrg_bal_code, values) %>%
    mutate(share_energy=tidyr::replace_na(GID_E/GID_OBS,0)) %>%
    select(-c(GID_E, GID_OBS))

  # Project til now
  years <- unique(year(cons_monthly_raw_oil$time))
  shares_filled <- shares %>%
    tidyr::complete(year=years, geo, unit, siec) %>%
    group_by(geo, unit, siec) %>%
    arrange(year) %>%
    fill(share_energy) %>%
    ungroup()


  cons_monthly_raw_oil_energy <- cons_monthly_raw_oil %>%
    filter(nrg_bal %in% c('Gross inland deliveries - observed')) %>%
    filter(siec=='Oil products') %>%
    mutate(year=year(time)) %>%
    inner_join(shares_filled) %>%
    mutate(
      nrg_bal='Gross inland deliveries - energy use',
      nrg_bal_code="GID_E",
      values = values * share_energy
    ) %>%
    select(-c(share_energy, year))

  return(bind_rows(
    cons_monthly_raw_oil,
    cons_monthly_raw_oil_energy
  ))
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
add_gas_non_energy <- function(cons_monthly_raw_gas, cons_yearly_raw_gas){

  shares <- cons_yearly_raw_gas %>%
    filter(time >= '1990-01-01') %>%
    filter(siec=='Natural gas') %>%
    filter(grepl('Terajoule', unit)) %>%
    filter(nrg_bal %in% c('Inland consumption - observed',
                          'Final consumption - non-energy use')) %>%
    mutate(year=year(time)) %>%
    select(nrg_bal_code, siec, geo, year, values) %>%
    tidyr::spread(nrg_bal_code, values) %>%
    mutate(share_non_energy=tidyr::replace_na(FC_NE/IC_OBS,0)) %>%
    select(-c(FC_NE, IC_OBS))

  # Project til now
  years <- unique(year(cons_monthly_raw_gas$time))
  shares_filled <- shares %>%
    tidyr::complete(year=years, geo, siec) %>%
    group_by(geo, siec) %>%
    arrange(year) %>%
    fill(share_non_energy) %>%
    ungroup()


  cons_monthly_raw_gas_non_energy <- cons_monthly_raw_gas %>%
    filter(nrg_bal %in% c('Inland consumption - observed')) %>%
    filter(siec=='Natural gas') %>%
    mutate(year=year(time)) %>%
    inner_join(shares_filled) %>%
    mutate(
      nrg_bal='Final consumption - non-energy use',
      nrg_bal_code="FC_NE",
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
fill_ng_elec_eu27 <- function(cons_monthly_raw_gas){

  nrg_bal_elec <- 'Transformation input - electricity and heat generation - main activity producers'

  eu27_ng_elec_new <- cons_monthly_raw_gas %>%
    add_iso2() %>%
    filter(nrg_bal==nrg_bal_elec) %>%
    filter(iso2 %in% get_eu_iso2s()) %>%
    group_by(unit, time) %>%
    summarise(values=sum(values, na.rm=T), .groups="drop")

  eu27_ng_elec_old <- cons_monthly_raw_gas %>%
    add_iso2() %>%
    filter(nrg_bal==nrg_bal_elec) %>%
    filter(iso2 == "EU")

  ggplot(bind_rows(
    eu27_ng_elec_new %>% mutate(source='new'),
    eu27_ng_elec_old %>% mutate(source='old'),
  )) +
    geom_line(aes(time, values, col=source, linetype=source)) + facet_wrap(~unit)


  eu27_ng_elec <- eu27_ng_elec_old %>%
    group_by(unit) %>%
    tidyr::complete(tidyr::nesting(time=seq.Date(min(time), max(time), by="month")),
                    tidyr::nesting(nrg_bal_code, nrg_bal, siec, freq),
                    tidyr::nesting(geo, iso2)
    ) %>%
    left_join(eu27_ng_elec_new %>%
                select(unit, time, values_filler=values)) %>%
    mutate(values=coalesce(values, values_filler)) %>%
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
remove_last_incomplete <- function(cons){

  max_months <- 6
  cons %>%
    group_by(geo, sector, unit, siec, fuel_type) %>%
    arrange(desc(time)) %>%
    mutate(cumsum=cumsum(values)) %>%
    filter(cumsum!=0 | max(cumsum)==0 | row_number() >= max_months) %>%
    ungroup()
}


get_eurostat_from_code <- function(code, use_cache=T){

  filepath <- file.path('cache', paste0('eurostat_', code, '.RDS'))
  dir.create("cache", F, T)

  if(use_cache & file.exists(filepath)){
    return(readRDS(filepath))
  }
  eurostat::get_eurostat(code) %>%
    eurostat::label_eurostat(code="nrg_bal") %>% # Keep nrg_bal code as well
    dplyr::rename(time=TIME_PERIOD) %T>%# Column changed with new EUROSTAT version
    saveRDS(filepath)
}

