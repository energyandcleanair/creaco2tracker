get_co2_from_eurostat_cons <- function(
  eurostat_cons,
  diagnostics_folder = "diagnostics",
  keep_siec = FALSE,
  ncv_source = "iea",
  use_cache = TRUE
) {
  group_by_cols <- c("iso2", "date" = "time", "fuel", "sector", "unit")
  if (keep_siec) {
    group_by_cols <- c(group_by_cols, "siec")
  }

  # Choose NCV method based on source parameter
  add_ncv_fn <- switch(ncv_source,
    "iea" = add_ncv_iea,
    "iea_shared" = add_ncv_iea_shared,
    "ipcc" = add_ncv_ipcc,
    add_ncv_iea
  ) # Default to IEA if invalid source

  eurostat_cons %>%
    add_ncv_fn(diagnostics_folder = diagnostics_folder, use_cache = use_cache) %>%
    add_emission_factor() %>%
    mutate(
      value_co2_tonne =
        case_when(
          unit == EUROSTAT_UNIT_THOUSAND_TONNES ~ values * ncv_kjkg / 1000 * co2_factor_t_per_TJ,
          unit == EUROSTAT_UNIT_TJ_GCV & fuel == FUEL_GAS ~ values *
            ncv_gcv_gas * co2_factor_t_per_TJ
        )
    ) %>%
    filter(!is.na(value_co2_tonne)) %>%
    group_by_at(group_by_cols) %>%
    summarise(
      value = sum(value_co2_tonne, na.rm = TRUE),
      unit = "t",
      .groups = "drop"
    )
}

add_emission_factor <- function(x) {
  # Get emission factors from IPCC
  emission_factors <- get_ipcc_emission_factors()

  # Join emission factors to the dataset (many-to-one join)
  x %>%
    left_join(
      emission_factors,
      by = "siec",
      relationship = "many-to-one"
    ) %>%
    {
      # Check that all rows have emission factors
      stopifnot(all(!is.na(.$co2_factor_t_per_TJ)))
      # Check that we didn't duplicate rows
      # Technically done with relationship above
      stopifnot(nrow(.) == nrow(x))
      .
    }
}
