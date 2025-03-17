get_co2_from_eurostat_cons <- function(eurostat_cons,
                                       diagnostics_folder="diagnostics",
                                       keep_siec=F,
                                       ncv_source="iea"){

  group_by_cols <- c("iso2", "geo", "date"="time", "fuel", "sector", "unit")
  if(keep_siec){
    group_by_cols <- c(group_by_cols, "siec_code")
  }

  # Choose NCV method based on source parameter
  add_ncv_fn <- switch(ncv_source,
                       "iea" = add_ncv_iea,
                       "ipcc" = add_ncv_ipcc,
                       add_ncv_iea) # Default to IEA if invalid source

  eurostat_cons %>%
    add_ncv_fn(diagnostics_folder = diagnostics_folder) %>%
    add_emission_factor() %>%
    mutate(value_co2_tonne=
             case_when(unit=='Thousand tonnes' ~ values * ncv_kjkg / 1000 * co2_factor_t_per_TJ,
                       unit=='Terajoule (gross calorific value - GCV)' & fuel==FUEL_GAS ~ values * ncv_gcv_gas * co2_factor_t_per_TJ
             )) %>%
    filter(!is.na(value_co2_tonne)) %>%
    group_by_at(group_by_cols) %>%
    summarise(value = sum(value_co2_tonne, na.rm=T),
              n_siec=n_distinct(siec_code),
              unit='t',
              .groups="drop"
    ) %>%
    # Remove those with only partial data
    ungroup()
}

#' Get emission factors from IPCC
#'
#' @returns
#' @export
#'
#' @examples
get_ipcc_emission_factors <- function() {
  # Emission factors in tCO2/TJ from https://www.ipcc-nggip.iges.or.jp/EFDB/find_ef.php
  tibble::tribble(
    ~siec_code,                 ~co2_factor_t_per_TJ,
    SIEC_HARD_COAL,             92.8,                 # EFID=110620
    SIEC_BROWN_COAL,            113.1,                # EFID=123085
    SIEC_BROWN_COAL_BRIQUETTES, 99,                   # EFID=123073
    # SIEC_PEAT ("P1000"),      117.766,              # EFID=122005 (Peat)
    # SIEC_OIL_SHALE ("S2000"), 108,                  # Oil shale
    SIEC_OIL_PRODUCTS,          20 * 44 / 12,         # EFID=110669 Other petroleum products
    SIEC_FUEL_OIL,              77.7,                 # EFID=121579
    SIEC_HEATING_GASOIL,        20 * 44 / 12,         # EFID=17174 = 73.33
    SIEC_MOTOR_GASOLINE_XBIO,   72.1,                 # EFID=18667 (Motor gasoline)
    SIEC_ROAD_DIESEL,           72.1,                 # EFID=18919
    SIEC_CRUDE_OIL,             73,                   # EFID=110603
    SIEC_NATURAL_GAS,           55.74,                # Average of EFID123092-123095
    SIEC_COKE_OVEN_GAS,         41.2,                 # EFID=122159
    SIEC_COKE_OVEN_COKE,        113,                  # EFID=110624
    SIEC_GASOIL_DIESEL,         72.1,                 # EFID=18919
    SIEC_KEROSENE_XBIO,         72.69,                # Taken from EEA
    SIEC_AVIATION_GASOLINE,     70.55,                # Taken from EEA
  )
}

add_emission_factor <- function(x){
  # Get emission factors from IPCC
  emission_factors <- get_ipcc_emission_factors()

  # Join emission factors to the dataset (many-to-one join)
  x %>%
    left_join(emission_factors, by = "siec_code",
              relationship = "many-to-one") %>%
    {
      # Check that all rows have emission factors
      stopifnot(all(!is.na(.$co2_factor_t_per_TJ)))
      # Check that we didn't duplicate rows
      # Technically done with relationship above
      stopifnot(nrow(.) == nrow(x))
      .
    }
}
