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

  repair_candidates <- attr(eurostat_cons, "coal_eu_repair_candidates")
  converted <- eurostat_cons %>%
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
    filter(unit %in% c(EUROSTAT_UNIT_THOUSAND_TONNES, EUROSTAT_UNIT_TJ_GCV)) %>%
    apply_verified_coal_eu_repairs(repair_candidates)
  repair_diagnostics <- attr(converted, "coal_eu_emissions_repairs")
  if (!is_null_or_empty(diagnostics_folder) && !is.null(repair_diagnostics)) {
    readr::write_csv(
      repair_diagnostics,
      file.path(diagnostics_folder, "coal_eu_emissions_repairs.csv")
    )
  }

  separate_keys <- attr(eurostat_cons, "coal_separate_projection")
  separate <- converted[0, ]
  if (!is.null(separate_keys) && nrow(separate_keys) > 0) {
    keys <- c("iso2", "siec", "unit", "fuel")
    separate <- converted %>% semi_join(separate_keys, by = keys)
    converted <- converted %>% anti_join(separate_keys, by = keys)
    if (!is_null_or_empty(diagnostics_folder)) {
      readr::write_csv(separate,
        file.path(diagnostics_folder, "coal_separate_emissions.csv"))
    }
  }
  aggregate_converted <- function(data) data %>%
    group_by_at(group_by_cols) %>%
    summarise(
      value = if (any(is.na(value_co2_tonne))) NA_real_ else sum(value_co2_tonne),
      unit = "t",
      .groups = "drop"
    )
  result <- aggregate_converted(converted)
  attr(result, "coal_separate_projection") <- aggregate_converted(separate)
  result
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
