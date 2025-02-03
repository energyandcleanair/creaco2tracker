get_eu_iso2s <- function(eurostat=F, include_eu=F){
  cl <- countrycode::codelist
  iso2s <- cl$iso2c[which(cl$eu28=="EU" & cl$iso2c != "GB")]
  if(eurostat) iso2s[iso2s=="GR"]="EL"

  if(include_eu) iso2s <- c(iso2s, "EU")
  iso2s
}

recode_siec <- function(x){
  x %>%
    mutate(siec = recode(
      siec,
      "Crude oil, NGL, refinery feedstocks, additives and oxygenates and other hydrocarbons" = "Crude oil",
      "Oil shale and oil sands" = "Oil shale"))
}

add_iso2 <- function(x, country_col="geo"){
  x %>%
    mutate(iso2=countrycode::countrycode(!!sym(country_col), "country.name", "iso2c",
                                         custom_match = c("European Union - 27 countries (from 2020)"="EU",
                                                          "EU27 & UK"="EU28",
                                                          "Kosovo*"="XK")))
}

add_missing_cols <- function(df, cols){
  for(col in cols){
    if(!col %in% names(df)){
      df[[col]] <- NA
    }
  }
  df
}

iso2_to_name <- function(x){
  countrycode(x, "iso2c", "country.name", custom_match = c("EU" = "EU"))
}


split_gas_to_elec_others <- function(co2) {

  # Quick return if no work needed
  gas_data <- co2 %>% filter(fuel == 'gas')
  gas_sectors <- unique(gas_data$sector)
  if(setequal(gas_sectors, c(SECTOR_ELEC, SECTOR_OTHERS))) {
    return(co2)
  }

  group_cols <- intersect(names(co2), c("iso2", "geo", "date", "fuel", "estimate", "unit"))

  # Process gas data using pivot operations
  gas_result <- gas_data %>%
    pivot_wider(
      id_cols = all_of(group_cols),
      names_from = sector,
      values_from = value,
      values_fill = NA
    ) %>%
    add_missing_cols(c("all", "others", "electricity")) %>%
    mutate(
      others = coalesce(others, all - electricity),  # calculate others value
      electricity = coalesce(electricity, all - others)  # calculate electricity value
    ) %>%
    select(-all) %>%
    pivot_longer(
      cols = c(electricity, others),
      names_to = "sector",
      values_to = "value"
    )

  # Combine results
  bind_rows(
    gas_result,
    co2 %>% filter(fuel != 'gas')
  )
}


split_gas_to_elec_all <- function(co2){

  # Quick return if no work needed
  gas_data <- co2 %>% filter(fuel == 'gas')
  gas_sectors <- unique(gas_data$sector)

  group_cols <- intersect(names(co2), c("iso2", "geo", "date", "fuel", "estimate", "unit"))

  # Process gas data using pivot operations
  gas_result <- gas_data %>%
    pivot_wider(
      id_cols = all_of(group_cols),
      names_from = sector,
      values_from = value,
      values_fill = NA
    ) %>%
    add_missing_cols(c("all", "others", "electricity")) %>%
    mutate(
      all = coalesce(all, others + electricity),
      electricity = coalesce(electricity, all - others)
    ) %>%
    select(-others) %>%
    pivot_longer(
      cols = c(electricity, all),
      names_to = "sector",
      values_to = "value"
    )

  # Combine results
  bind_rows(
    gas_result,
    co2 %>% filter(fuel != 'gas')
  )
}

#' This computes total co2, while properly aggregating uncertainty
#'
#' @param co2 dataframe with columns iso2, geo, date, fuel, estimate, value
#' @return dataframe with summed CO2 values and properly combined uncertainties
#' @export
add_total_co2 <- function(co2){
  # First handle non-total fuels
  co2 %>%
    filter(fuel != 'total') %>%
    group_by(iso2, geo, date) %>%
    summarise(
      # First calculate central value and std dev
      central_value = sum(value[estimate == "central"], na.rm = TRUE),
      std_dev = sqrt(sum(
        # Convert confidence intervals to standard deviations
        (value[estimate == "upper"] - value[estimate == "central"])^2
      )),
      .groups = "drop"
    ) %>%
    # Create three rows for each group with the different estimates
    tidyr::crossing(estimate = c("central", "lower", "upper")) %>%
    mutate(
      value = case_when(
        estimate == "central" ~ central_value,
        estimate == "lower" ~ central_value - std_dev,
        estimate == "upper" ~ central_value + std_dev
      ),
      fuel = 'total',
      sector = SECTOR_ALL
    ) %>%
    select(-central_value, -std_dev) %>%
    bind_rows(co2 %>% filter(fuel != 'total')) %>%
    ungroup()
}

combine_coke_coal <- function(co2){

  group_by_cols <- intersect(names(co2), c("iso2", "geo", "region", "date", "fuel", "sector", "unit"))
  co2 %>%
    filter(fuel %in% c(FUEL_COAL, FUEL_COKE)) %>%
    mutate(fuel=FUEL_COAL) %>%
    group_by_at(group_by_cols) %>%
    summarise(
      # First calculate central value and std dev
      central_value = sum(value[estimate == "central"], na.rm = TRUE),
      std_dev = sqrt(sum(
        # Convert confidence intervals to standard deviations
        (value[estimate == "upper"] - value[estimate == "central"])^2
      )),
      .groups = "drop"
    ) %>%
    # Create three rows for each group with the different estimates
    tidyr::crossing(estimate = c("central", "lower", "upper")) %>%
    mutate(
      value = case_when(
        estimate == "central" ~ central_value,
        estimate == "lower" ~ central_value - std_dev,
        estimate == "upper" ~ central_value + std_dev
      )
    ) %>%
    select(-central_value, -std_dev) %>%
    bind_rows(
      co2 %>% filter(! fuel %in% c(FUEL_COAL, FUEL_COKE))
    ) %>%
    ungroup()
}

format_co2_for_db <- function(co2_daily, cut_tail_days=3){

  co2_daily %>%
    ungroup() %>%
    # Combine coal and coke
    combine_coke_coal() %>%
    mutate(across(c(fuel, sector), stringr::str_to_title),
           frequency='daily',
           region=iso2,
           version=as.character(packageVersion("creaco2tracker"))) %>%
    select(region, date, fuel, sector, unit, frequency, version, estimate, value) %>%
    pivot_wider(names_from=estimate, values_from=value, names_prefix="value_") %>%
    rename(value=value_central)
}

check_no_duplicate <- function(x){
  group_by_cols <- intersect(names(x), c("iso2", "geo", "date", "fuel", "sector", "estimate"))
  # Check no duplicate
  stopifnot(1 == x %>% group_by_at(group_by_cols) %>% summarise(n=n(), .groups="drop") %>% pull(n) %>% max())
}

#' Check that CO2 can basically be summed
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
check_no_double_counting <- function(x){

  stopifnot("there is double counting"=
    x %>%
      group_by(fuel) %>%
      summarise(
        no_double_counting = !any(sector==SECTOR_ALL & n_distinct(sector)>1)
      ) %>%
      pull(no_double_counting) %>%
      all()
  )
}
