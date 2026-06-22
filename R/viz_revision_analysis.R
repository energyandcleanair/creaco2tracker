plot_get_co2_revision_analysis_validation <- function(
  revision_comparison,
  vintage_co2,
  reference_vintage_co2,
  output_folder,
  reference_vintage_month,
  target_months,
  vintage_dates,
  vintage_months,
  data_masking_lags,
  data_masking_publication_months,
  near_zero_reference_threshold_tonnes_co2 = 100000,
  top_n = 15,
  trend_min_n = 6,
  width = 10,
  height = 6,
  dpi = 300
) {
  tables_dir <- file.path(output_folder, "tables")
  summary_dir <- file.path(output_folder, "charts", "summary")
  details_dir <- file.path(output_folder, "charts", "details")

  create_dir(tables_dir)
  create_dir(summary_dir)
  create_dir(details_dir)

  source_availability <- .get_co2_revision_analysis_source_availability(
    vintage_dates = vintage_dates,
    vintage_months = vintage_months,
    target_months = target_months,
    lags = data_masking_lags,
    publication_months = data_masking_publication_months
  )
  comparison_internal <- .build_get_co2_revision_analysis_vintage_revision_comparison(
    revision_comparison = revision_comparison,
    source_availability = source_availability,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  comparison_candidates <- .get_co2_revision_analysis_component_candidates(
    vintage_co2 = vintage_co2,
    reference_vintage_co2 = reference_vintage_co2
  )

  .validate_get_co2_revision_analysis_comparison(
    comparison_internal = comparison_internal,
    comparison_candidates = comparison_candidates,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )

  vintage_revision_comparison <- .get_co2_revision_analysis_public_comparison(
    comparison_internal
  )
  debug_revision_summary <- .summarise_get_co2_revision_analysis_debug_summary(
    comparison_internal = comparison_internal,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  revision_outliers <- .build_get_co2_revision_analysis_outlier_table(comparison_internal)
  country_summary <- .summarise_get_co2_revision_analysis_country_overall(
    comparison_internal = comparison_internal,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  country_component_summary <- .summarise_get_co2_revision_analysis_country_component_overall(
    comparison_internal = comparison_internal,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  lag_summary <- .summarise_get_co2_revision_analysis_plot_summary(
    comparison_internal = comparison_internal,
    grouping_vars = c("aggregation_level", "lag_bucket"),
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  stage_summary <- .summarise_get_co2_revision_analysis_plot_summary(
    comparison_internal = comparison_internal,
    grouping_vars = c("aggregation_level", "data_maturity_stage"),
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )
  trend_direction_agreement <- .summarise_get_co2_revision_analysis_trend_direction_agreement(
    comparison_internal = comparison_internal,
    country_component_summary = country_component_summary,
    top_n = top_n,
    trend_min_n = trend_min_n
  )

  summary_plot_paths <- .plot_get_co2_revision_analysis_summary_charts(
    lag_summary = lag_summary,
    stage_summary = stage_summary,
    country_summary = country_summary,
    country_component_summary = country_component_summary,
    trend_direction_agreement = trend_direction_agreement,
    comparison_internal = comparison_internal,
    output_dir = summary_dir,
    width = width,
    height = height,
    dpi = dpi,
    top_n = top_n
  )
  .plot_get_co2_revision_analysis_detail_charts(
    comparison_internal = comparison_internal,
    country_summary = country_summary,
    country_component_summary = country_component_summary,
    output_dir = details_dir,
    width = width,
    height = height,
    dpi = dpi
  )

  readr::write_csv(
    vintage_revision_comparison,
    file.path(tables_dir, "vintage_revision_comparison.csv")
  )
  readr::write_csv(
    debug_revision_summary,
    file.path(tables_dir, "debug_revision_summary.csv")
  )
  readr::write_csv(
    revision_outliers,
    file.path(tables_dir, "revision_outliers.csv")
  )

  list(
    vintage_revision_comparison = vintage_revision_comparison,
    debug_revision_summary = debug_revision_summary,
    revision_outliers = revision_outliers,
    summary_plot_paths = summary_plot_paths
  )
}


.get_co2_revision_analysis_source_availability <- function(
  vintage_dates,
  vintage_months,
  target_months,
  lags,
  publication_months
) {
  cadence_map <- .default_source_publication_cadence()
  source_meta <- tibble(
    source_name = names(cadence_map),
    cadence = unname(cadence_map),
    cadence_class = if_else(cadence == "year", "annual", "monthly")
  )

  bind_rows(lapply(seq_along(vintage_dates), function(i) {
    cfg <- data_masking_as_of(
      vintage_dates[[i]],
      lags = lags,
      publication_months = publication_months
    )
    source_cutoffs <- bind_rows(lapply(source_meta$source_name, function(source_name) {
      rules <- .rule_list_from_config(cfg[[source_name]])
      unavailable_from <- NA_character_
      if (length(rules) > 0) {
        unavailable_from <- if (!is.null(rules[[1]]$date_from)) {
          rules[[1]]$date_from
        } else {
          NA_character_
        }
      }

      tibble(
        source_name = source_name,
        unavailable_from = as.Date(unavailable_from)
      )
    })) %>%
      left_join(source_meta, by = "source_name")

    tidyr::crossing(
      vintage_month = as.Date(vintage_months[[i]]),
      target_month = as.Date(target_months),
      source_cutoffs
    ) %>%
      mutate(
        target_month_end = lubridate::ceiling_date(target_month, "month") - 1,
        target_year = lubridate::floor_date(target_month, "year"),
        is_available = case_when(
          is.na(unavailable_from) ~ TRUE,
          cadence == "year" ~ target_year < unavailable_from,
          TRUE ~ target_month_end < unavailable_from
        )
      ) %>%
      group_by(vintage_month, target_month, cadence_class) %>%
      summarise(
        available_sources = sum(is_available),
        total_sources = n(),
        .groups = "drop"
      ) %>%
      mutate(
        column_prefix = if_else(cadence_class == "monthly", "monthly", "annual")
      ) %>%
      select(-cadence_class) %>%
      tidyr::pivot_wider(
        names_from = column_prefix,
        values_from = c(available_sources, total_sources),
        names_glue = "{.value}_{column_prefix}"
      ) %>%
      mutate(
        monthly_availability_share = if_else(
          total_sources_monthly > 0,
          available_sources_monthly / total_sources_monthly,
          NA_real_
        ),
        annual_availability_share = if_else(
          total_sources_annual > 0,
          available_sources_annual / total_sources_annual,
          NA_real_
        ),
        data_maturity_stage = case_when(
          monthly_availability_share == 0 ~ "monthly_missing",
          monthly_availability_share < 1 ~ "monthly_partial",
          monthly_availability_share == 1 & annual_availability_share == 0 ~
            "monthly_complete_annual_missing",
          monthly_availability_share == 1 & annual_availability_share < 1 ~
            "monthly_complete_annual_partial",
          monthly_availability_share == 1 & annual_availability_share == 1 ~
            "monthly_complete_annual_complete",
          TRUE ~ NA_character_
        )
      ) %>%
      rename(
        available_monthly_sources = available_sources_monthly,
        total_monthly_sources = total_sources_monthly,
        available_annual_sources = available_sources_annual,
        total_annual_sources = total_sources_annual
      ) %>%
      select(
        vintage_month,
        target_month,
        data_maturity_stage,
        monthly_availability_share,
        annual_availability_share,
        available_monthly_sources,
        total_monthly_sources,
        available_annual_sources,
        total_annual_sources
      )
  })) %>%
    arrange(vintage_month, target_month)
}


.build_get_co2_revision_analysis_vintage_revision_comparison <- function(
  revision_comparison,
  source_availability,
  near_zero_reference_threshold_tonnes_co2
) {
  component_flags <- revision_comparison %>%
    filter(
      estimate == "central",
      iso2 != "EU",
      !(fuel == FUEL_TOTAL & sector == SECTOR_ALL)
    ) %>%
    transmute(
      country = iso2,
      component = case_when(
        fuel == FUEL_TOTAL ~ .get_co2_revision_analysis_sector_label(sector),
        TRUE ~ .get_co2_revision_analysis_component_label(fuel, sector)
      ),
      estimate_value = vintage_value,
      reference_estimate = reference_vintage_value
    ) %>%
    filter(!is.na(estimate_value), !is.na(reference_estimate)) %>%
    group_by(country, component) %>%
    summarise(
      is_always_zero_country_component = all(
        estimate_value == 0 & reference_estimate == 0
      ),
      .groups = "drop"
    )

  revision_comparison %>%
    filter(estimate == "central") %>%
    transmute(
      reference_vintage = as.Date(reference_vintage_month),
      vintage_month = as.Date(vintage_month),
      target_month = as.Date(target_month),
      lag_months = as.integer(estimation_lag),
      country = iso2,
      fuel = fuel,
      sector = sector,
      estimate = vintage_value,
      reference_estimate = reference_vintage_value,
      revision = revision,
      absolute_revision = absolute_revision
    ) %>%
    filter(!is.na(estimate), !is.na(reference_estimate)) %>%
    mutate(
      aggregation_level = case_when(
        country == "EU" & fuel == FUEL_TOTAL & sector == SECTOR_ALL ~ "total",
        country != "EU" & fuel == FUEL_TOTAL & sector == SECTOR_ALL ~ "country",
        country != "EU" ~ "component",
        TRUE ~ NA_character_
      ),
      component = case_when(
        aggregation_level != "component" ~ NA_character_,
        fuel == FUEL_TOTAL ~ .get_co2_revision_analysis_sector_label(sector),
        TRUE ~ .get_co2_revision_analysis_component_label(fuel, sector)
      ),
      component_id = case_when(
        aggregation_level != "component" ~ NA_character_,
        fuel == FUEL_TOTAL ~ .get_co2_revision_analysis_sector_id(sector),
        TRUE ~ .get_co2_revision_analysis_component_id(fuel, sector)
      ),
      component_type = case_when(
        aggregation_level != "component" ~ NA_character_,
        fuel == FUEL_TOTAL ~ "sector",
        TRUE ~ "sector_plus_commodity"
      )
    ) %>%
    filter(!is.na(aggregation_level)) %>%
    left_join(source_availability, by = c("vintage_month", "target_month")) %>%
    left_join(component_flags, by = c("country", "component")) %>%
    mutate(
      is_always_zero_country_component = replace_na(
        is_always_zero_country_component,
        FALSE
      ),
      is_near_zero_reference = abs(reference_estimate) <
        near_zero_reference_threshold_tonnes_co2,
      relative_revision = case_when(
        reference_estimate == 0 ~ NA_real_,
        is_near_zero_reference ~ NA_real_,
        TRUE ~ revision / reference_estimate
      ),
      lag_bucket = .get_co2_revision_analysis_lag_bucket(lag_months)
    ) %>%
    arrange(aggregation_level, country, component, vintage_month, target_month)
}


.validate_get_co2_revision_analysis_comparison <- function(
  comparison_internal,
  comparison_candidates,
  near_zero_reference_threshold_tonnes_co2
) {
  if (any(comparison_internal$lag_months < 0, na.rm = TRUE)) {
    stop("Revision-analysis comparison contains negative lag_months.")
  }

  if (any(comparison_internal$target_month > comparison_internal$vintage_month, na.rm = TRUE)) {
    stop("Revision-analysis comparison includes target months unavailable in a vintage.")
  }

  if (any(is.na(comparison_internal$estimate) | is.na(comparison_internal$reference_estimate))) {
    stop("Revision-analysis comparison must exclude missing estimates.")
  }

  negative_reference_rows <- comparison_internal %>%
    filter(reference_estimate < 0)
  if (nrow(negative_reference_rows) > 0) {
    log_warn(glue::glue(
      "Found {nrow(negative_reference_rows)} comparison rows with negative ",
      "reference estimates."
    ))
  }

  near_zero_rows <- comparison_internal %>%
    filter(
      abs(reference_estimate) < near_zero_reference_threshold_tonnes_co2,
      !is.na(relative_revision)
    )
  if (nrow(near_zero_rows) > 0) {
    stop("Near-zero reference estimates must have suppressed relative_revision values.")
  }

  total_reconciliation <- comparison_internal %>%
    filter(aggregation_level %in% c("total", "country")) %>%
    group_by(vintage_month, target_month) %>%
    summarise(
      total_estimate = sum(estimate[aggregation_level == "total"], na.rm = TRUE),
      country_estimate = sum(estimate[aggregation_level == "country"], na.rm = TRUE),
      total_reference = sum(reference_estimate[aggregation_level == "total"], na.rm = TRUE),
      country_reference = sum(reference_estimate[aggregation_level == "country"], na.rm = TRUE),
      .groups = "drop"
    )
  if (any(abs(total_reconciliation$total_estimate - total_reconciliation$country_estimate) > 1e-6)) {
    warning("EU total estimates do not reconcile with the sum of country estimates.")
  }
  if (any(abs(total_reconciliation$total_reference - total_reconciliation$country_reference) > 1e-6)) {
    warning("EU total reference estimates do not reconcile with the sum of country references.")
  }

  country_reconciliation <- comparison_internal %>%
    filter(aggregation_level %in% c("country", "component")) %>%
    group_by(vintage_month, target_month, country) %>%
    summarise(
      country_estimate = sum(estimate[aggregation_level == "country"], na.rm = TRUE),
      component_estimate = sum(estimate[aggregation_level == "component"], na.rm = TRUE),
      country_reference = sum(reference_estimate[aggregation_level == "country"], na.rm = TRUE),
      component_reference = sum(
        reference_estimate[aggregation_level == "component"],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  if (any(abs(country_reconciliation$country_estimate - country_reconciliation$component_estimate) > 1e-6)) {
    warning("Country estimates do not reconcile with the sum of component estimates.")
  }
  if (any(abs(country_reconciliation$country_reference - country_reconciliation$component_reference) > 1e-6)) {
    warning("Country reference estimates do not reconcile with the sum of component references.")
  }

  candidate_without_comparison <- comparison_candidates %>%
    anti_join(
      comparison_internal %>%
        filter(aggregation_level == "component") %>%
        distinct(country, component, component_id),
      by = c("country", "component", "component_id")
    )
  if (nrow(candidate_without_comparison) > 0) {
    log_info(glue::glue(
      "Skipping {nrow(candidate_without_comparison)} country-component pairs ",
      "without comparable revisions."
    ))
  }
}


.get_co2_revision_analysis_public_comparison <- function(comparison_internal) {
  comparison_internal %>%
    select(
      reference_vintage,
      vintage_month,
      target_month,
      lag_months,
      lag_bucket,
      aggregation_level,
      country,
      component,
      component_type,
      estimate,
      reference_estimate,
      revision,
      absolute_revision,
      relative_revision,
      data_maturity_stage,
      monthly_availability_share,
      annual_availability_share,
      available_monthly_sources,
      total_monthly_sources,
      available_annual_sources,
      total_annual_sources,
      is_always_zero_country_component,
      is_near_zero_reference
    )
}


.summarise_get_co2_revision_analysis_debug_summary <- function(
  comparison_internal,
  near_zero_reference_threshold_tonnes_co2
) {
  bind_rows(
    .summarise_get_co2_revision_analysis_debug_level(
      comparison_internal = comparison_internal %>% filter(aggregation_level == "total"),
      grouping_vars = c("aggregation_level", "lag_bucket", "data_maturity_stage"),
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    ) %>%
      mutate(country = "EU", component = NA_character_) %>%
      select(
        aggregation_level,
        country,
        component,
        lag_bucket,
        data_maturity_stage,
        n_observations,
        sum_reference,
        net_revision,
        gross_revision,
        gross_revision_share,
        weighted_absolute_revision_pct,
        signed_revision_pct,
        median_abs_revision_pct,
        p90_abs_revision_pct,
        max_abs_revision_pct,
        rmse
      ),
    .summarise_get_co2_revision_analysis_debug_level(
      comparison_internal = comparison_internal %>% filter(aggregation_level == "country"),
      grouping_vars = c("aggregation_level", "country", "lag_bucket", "data_maturity_stage"),
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    ) %>%
      mutate(component = NA_character_) %>%
      select(
        aggregation_level,
        country,
        component,
        lag_bucket,
        data_maturity_stage,
        n_observations,
        sum_reference,
        net_revision,
        gross_revision,
        gross_revision_share,
        weighted_absolute_revision_pct,
        signed_revision_pct,
        median_abs_revision_pct,
        p90_abs_revision_pct,
        max_abs_revision_pct,
        rmse
      ),
    .summarise_get_co2_revision_analysis_debug_level(
      comparison_internal = comparison_internal %>%
        filter(
          aggregation_level == "component",
          !is_always_zero_country_component
        ),
      grouping_vars = c(
        "aggregation_level",
        "country",
        "component",
        "component_id",
        "lag_bucket",
        "data_maturity_stage"
      ),
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    ) %>%
      select(
        aggregation_level,
        country,
        component,
        lag_bucket,
        data_maturity_stage,
        n_observations,
        sum_reference,
        net_revision,
        gross_revision,
        gross_revision_share,
        weighted_absolute_revision_pct,
        signed_revision_pct,
        median_abs_revision_pct,
        p90_abs_revision_pct,
        max_abs_revision_pct,
        rmse
      )
  ) %>%
    arrange(aggregation_level, country, component, lag_bucket, data_maturity_stage)
}


.summarise_get_co2_revision_analysis_debug_level <- function(
  comparison_internal,
  grouping_vars,
  near_zero_reference_threshold_tonnes_co2
) {
  if (nrow(comparison_internal) == 0) {
    return(tibble(
      aggregation_level = character(),
      country = character(),
      component = character(),
      component_id = character(),
      lag_bucket = character(),
      data_maturity_stage = character(),
      n_observations = integer(),
      sum_reference = numeric(),
      net_revision = numeric(),
      gross_revision = numeric(),
      weighted_absolute_revision_pct = numeric(),
      signed_revision_pct = numeric(),
      median_abs_revision_pct = numeric(),
      p90_abs_revision_pct = numeric(),
      max_abs_revision_pct = numeric(),
      rmse = numeric(),
      gross_revision_share = numeric()
    ))
  }

  summary <- comparison_internal %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_modify(~ .get_co2_revision_analysis_metric_row(
      .x,
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    )) %>%
    ungroup()

  share_grouping_vars <- intersect(
    c("aggregation_level", "lag_bucket", "data_maturity_stage"),
    names(summary)
  )
  summary %>%
    group_by(across(all_of(share_grouping_vars))) %>%
    group_modify(function(.x, .y) {
      gross_total <- sum(.x$gross_revision, na.rm = TRUE)
      .x$gross_revision_share <- if (gross_total > 0) {
        .x$gross_revision / gross_total
      } else {
        NA_real_
      }
      .x
    }) %>%
    ungroup()
}


.get_co2_revision_analysis_metric_row <- function(
  data,
  near_zero_reference_threshold_tonnes_co2
) {
  safe_percentage_rows <- data %>%
    filter(
      !is_near_zero_reference,
      !is.na(relative_revision),
      reference_estimate > 0
    )
  safe_sum_reference <- sum(safe_percentage_rows$reference_estimate, na.rm = TRUE)
  safe_denominator <- is.finite(safe_sum_reference) &&
    safe_sum_reference > near_zero_reference_threshold_tonnes_co2

  tibble(
    n_observations = nrow(data),
    sum_reference = sum(data$reference_estimate, na.rm = TRUE),
    net_revision = sum(data$revision, na.rm = TRUE),
    gross_revision = sum(data$absolute_revision, na.rm = TRUE),
    weighted_absolute_revision_pct = if (safe_denominator) {
      sum(safe_percentage_rows$absolute_revision, na.rm = TRUE) / safe_sum_reference
    } else {
      NA_real_
    },
    signed_revision_pct = if (safe_denominator) {
      sum(safe_percentage_rows$revision, na.rm = TRUE) / safe_sum_reference
    } else {
      NA_real_
    },
    median_abs_revision_pct = .get_co2_revision_analysis_safe_quantile(
      abs(safe_percentage_rows$relative_revision),
      probs = 0.5
    ),
    p90_abs_revision_pct = .get_co2_revision_analysis_safe_quantile(
      abs(safe_percentage_rows$relative_revision),
      probs = 0.9
    ),
    max_abs_revision_pct = .get_co2_revision_analysis_safe_max(
      abs(safe_percentage_rows$relative_revision)
    ),
    rmse = sqrt(mean(data$revision^2, na.rm = TRUE))
  )
}


.summarise_get_co2_revision_analysis_country_overall <- function(
  comparison_internal,
  near_zero_reference_threshold_tonnes_co2
) {
  .summarise_get_co2_revision_analysis_debug_level(
    comparison_internal = comparison_internal %>% filter(aggregation_level == "country"),
    grouping_vars = c("aggregation_level", "country"),
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  ) %>%
    select(-aggregation_level) %>%
    arrange(desc(gross_revision_share), desc(gross_revision), country)
}


.summarise_get_co2_revision_analysis_country_component_overall <- function(
  comparison_internal,
  near_zero_reference_threshold_tonnes_co2
) {
  comparison_internal %>%
    filter(
      aggregation_level == "component",
      !is_always_zero_country_component
    ) %>%
    group_by(country, component, component_id, is_always_zero_country_component) %>%
    group_modify(~ .get_co2_revision_analysis_metric_row(
      .x,
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    )) %>%
    ungroup() %>%
    {
      gross_total <- sum(.$gross_revision, na.rm = TRUE)
      mutate(
        .,
        gross_revision_share = if (gross_total > 0) {
          gross_revision / gross_total
        } else {
          NA_real_
        }
      )
    } %>%
    arrange(desc(gross_revision_share), desc(gross_revision), country, component)
}


.summarise_get_co2_revision_analysis_plot_summary <- function(
  comparison_internal,
  grouping_vars,
  near_zero_reference_threshold_tonnes_co2
) {
  comparison_internal %>%
    filter(
      aggregation_level != "component" | !is_always_zero_country_component
    ) %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_modify(~ .get_co2_revision_analysis_metric_row(
      .x,
      near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
    )) %>%
    ungroup()
}


.build_get_co2_revision_analysis_outlier_table <- function(comparison_internal) {
  comparison_internal %>%
    arrange(desc(absolute_revision), desc(abs(reference_estimate)), vintage_month, target_month) %>%
    select(
      vintage_month,
      target_month,
      country,
      component,
      reference_estimate,
      estimate,
      revision,
      absolute_revision,
      relative_revision,
      lag_months,
      lag_bucket,
      data_maturity_stage,
      monthly_availability_share,
      annual_availability_share,
      is_near_zero_reference
    )
}


.summarise_get_co2_revision_analysis_trend_direction_agreement <- function(
  comparison_internal,
  country_component_summary,
  top_n,
  trend_min_n
) {
  top_country_components <- country_component_summary %>%
    slice_head(n = top_n) %>%
    select(country, component, component_id)

  selected_data <- bind_rows(
    comparison_internal %>% filter(aggregation_level == "total"),
    comparison_internal %>% filter(aggregation_level == "country"),
    comparison_internal %>%
      filter(aggregation_level == "component") %>%
      semi_join(top_country_components, by = c("country", "component", "component_id"))
  )

  bind_rows(
    .summarise_get_co2_revision_analysis_direction_changes(
      selected_data = selected_data,
      step_months = 1,
      trend_type = "month_on_month"
    ),
    .summarise_get_co2_revision_analysis_direction_changes(
      selected_data = selected_data,
      step_months = 12,
      trend_type = "year_on_year"
    )
  ) %>%
    group_by(aggregation_level, lag_bucket, trend_type) %>%
    summarise(
      n_observations = n(),
      direction_agreement_pct = mean(is_direction_match),
      .groups = "drop"
    ) %>%
    filter(n_observations >= trend_min_n) %>%
    arrange(aggregation_level, lag_bucket, trend_type)
}


.summarise_get_co2_revision_analysis_direction_changes <- function(
  selected_data,
  step_months,
  trend_type
) {
  prev_data <- selected_data %>%
    transmute(
      aggregation_level,
      country,
      component,
      component_id,
      vintage_month,
      target_month = .get_co2_revision_analysis_shift_months(target_month, step_months),
      prev_estimate = estimate,
      prev_reference_estimate = reference_estimate
    )

  selected_data %>%
    left_join(
      prev_data,
      by = c(
        "aggregation_level",
        "country",
        "component",
        "component_id",
        "vintage_month",
        "target_month"
      )
    ) %>%
    mutate(
      vintage_change = estimate - prev_estimate,
      reference_change = reference_estimate - prev_reference_estimate,
      is_direction_match = sign(vintage_change) == sign(reference_change)
    ) %>%
    filter(!is.na(vintage_change), !is.na(reference_change)) %>%
    transmute(
      aggregation_level,
      lag_bucket,
      trend_type,
      is_direction_match
    )
}


.plot_get_co2_revision_analysis_summary_charts <- function(
  lag_summary,
  stage_summary,
  country_summary,
  country_component_summary,
  trend_direction_agreement,
  comparison_internal,
  output_dir,
  width,
  height,
  dpi,
  top_n
) {
  plot_paths <- c(
    revision_by_lag_bucket = file.path(output_dir, "revision_by_lag_bucket.png"),
    revision_by_data_maturity_stage = file.path(
      output_dir,
      "revision_by_data_maturity_stage.png"
    ),
    signed_revision_by_lag_bucket = file.path(
      output_dir,
      "signed_revision_by_lag_bucket.png"
    ),
    signed_revision_by_data_maturity_stage = file.path(
      output_dir,
      "signed_revision_by_data_maturity_stage.png"
    ),
    eu_revision_heatmap = file.path(output_dir, "eu_revision_heatmap.png"),
    eu_revision_by_data_maturity_stage = file.path(
      output_dir,
      "eu_revision_by_data_maturity_stage.png"
    ),
    top_countries_by_gross_revision = file.path(
      output_dir,
      "top_countries_by_gross_revision.png"
    ),
    top_country_components_by_gross_revision = file.path(
      output_dir,
      "top_country_components_by_gross_revision.png"
    ),
    country_estimate_vs_reference_by_data_maturity = file.path(
      output_dir,
      "country_estimate_vs_reference_by_data_maturity.png"
    ),
    component_estimate_vs_reference_by_data_maturity = file.path(
      output_dir,
      "component_estimate_vs_reference_by_data_maturity.png"
    )
  )

  .plot_get_co2_revision_analysis_summary_bar(
    plot_data = lag_summary,
    x_var = "lag_bucket",
    y_var = "weighted_absolute_revision_pct",
    filepath = plot_paths[["revision_by_lag_bucket"]],
    title = "Weighted absolute revision by lag bucket",
    subtitle = "Revision magnitude relative to the reference estimate",
    y_label = "Weighted absolute revision",
    width = width,
    height = height,
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_summary_bar(
    plot_data = stage_summary,
    x_var = "data_maturity_stage",
    y_var = "weighted_absolute_revision_pct",
    filepath = plot_paths[["revision_by_data_maturity_stage"]],
    title = "Weighted absolute revision by data maturity stage",
    subtitle = "Revision magnitude relative to the reference estimate",
    y_label = "Weighted absolute revision",
    width = width,
    height = height,
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_summary_bar(
    plot_data = lag_summary,
    x_var = "lag_bucket",
    y_var = "signed_revision_pct",
    filepath = plot_paths[["signed_revision_by_lag_bucket"]],
    title = "Signed revision by lag bucket",
    subtitle = "Directional revision relative to the reference estimate",
    y_label = "Signed revision",
    width = width,
    height = height,
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_summary_bar(
    plot_data = stage_summary,
    x_var = "data_maturity_stage",
    y_var = "signed_revision_pct",
    filepath = plot_paths[["signed_revision_by_data_maturity_stage"]],
    title = "Signed revision by data maturity stage",
    subtitle = "Directional revision relative to the reference estimate",
    y_label = "Signed revision",
    width = width,
    height = height,
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_revision_heatmap(
    group_data = comparison_internal %>% filter(aggregation_level == "total"),
    filepath = plot_paths[["eu_revision_heatmap"]],
    title_prefix = "EU",
    width = width,
    height = max(height * 1.8, 7),
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_revision_by_data_maturity_stage(
    group_data = comparison_internal %>% filter(aggregation_level == "total"),
    filepath = plot_paths[["eu_revision_by_data_maturity_stage"]],
    title_prefix = "EU",
    width = width,
    height = max(height * 1.5, 6),
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_top_contributors(
    plot_data = country_summary %>% slice_head(n = top_n),
    label_var = "country",
    filepath = plot_paths[["top_countries_by_gross_revision"]],
    title = "Top countries by gross revision magnitude",
    subtitle = paste(
      "Top",
      top_n,
      "country totals ranked by total absolute revision"
    ),
    width = width,
    height = max(height, 7),
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_top_contributors(
    plot_data = country_component_summary %>%
      slice_head(n = top_n) %>%
      mutate(country_component = paste(country, component, sep = " - ")),
    label_var = "country_component",
    filepath = plot_paths[["top_country_components_by_gross_revision"]],
    title = "Top country-components by gross revision magnitude",
    subtitle = paste(
      "Top",
      top_n,
      "country-components ranked by total absolute revision"
    ),
    width = width,
    height = max(height, 7),
    dpi = dpi
  )
  .plot_get_co2_revision_analysis_summary_estimate_vs_reference(
    plot_data = comparison_internal %>%
      filter(aggregation_level == "country"),
    filepath = plot_paths[["country_estimate_vs_reference_by_data_maturity"]],
    title = "Country estimate versus reference by data maturity stage",
    subtitle = "All country totals shown as log-log hexbins, faceted by data maturity stage",
    width = width,
    height = max(height, 8),
    dpi = dpi,
    free_scales = FALSE
  )
  .plot_get_co2_revision_analysis_summary_estimate_vs_reference(
    plot_data = comparison_internal %>%
      filter(
        aggregation_level == "component",
        !is_always_zero_country_component
      ),
    filepath = plot_paths[["component_estimate_vs_reference_by_data_maturity"]],
    title = "Component estimate versus reference by data maturity stage",
    subtitle = "All component estimates shown as log-log hexbins, faceted by data maturity stage",
    width = width,
    height = max(height, 8),
    dpi = dpi,
    free_scales = TRUE
  )

  if (nrow(trend_direction_agreement) > 0) {
    trend_path <- file.path(output_dir, "trend_direction_agreement.png")
    .plot_get_co2_revision_analysis_trend_direction_agreement(
      plot_data = trend_direction_agreement,
      filepath = trend_path,
      width = width,
      height = height,
      dpi = dpi
    )
    plot_paths <- c(plot_paths, trend_direction_agreement = trend_path)
  }

  plot_paths
}


.plot_get_co2_revision_analysis_summary_line <- function(
  plot_data,
  x_var,
  y_var,
  filepath,
  title,
  subtitle,
  y_label,
  width,
  height,
  dpi
) {
  plot_data <- plot_data %>%
    mutate(
      aggregation_level = factor(
        aggregation_level,
        levels = c("total", "country", "component"),
        labels = c("Total", "Country", "Component")
      )
    )

  plt <- ggplot(
    plot_data,
    aes(.data[[x_var]], .data[[y_var]], group = aggregation_level, color = aggregation_level)
  ) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 2.2, na.rm = TRUE) +
    facet_wrap(~aggregation_level, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    rcrea::theme_crea_new() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label,
      color = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
  plt
}


.plot_get_co2_revision_analysis_summary_bar <- function(
  plot_data,
  x_var,
  y_var,
  filepath,
  title,
  subtitle,
  y_label,
  width,
  height,
  dpi
) {
  plot_data <- plot_data %>%
    mutate(
      aggregation_level = factor(
        aggregation_level,
        levels = c("total", "country", "component"),
        labels = c("Total", "Country", "Component")
      )
    )

  if ("data_maturity_stage" %in% names(plot_data)) {
    plot_data <- plot_data %>%
      mutate(
        data_maturity_stage = factor(
          data_maturity_stage,
          levels = .get_co2_revision_analysis_data_maturity_levels()
        )
      )
  }

  plt <- ggplot(plot_data, aes(.data[[x_var]], .data[[y_var]], fill = aggregation_level)) +
    geom_col(width = 0.7, na.rm = TRUE) +
    facet_wrap(~aggregation_level, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    rcrea::theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label,
      fill = NULL,
      caption = "Source: CREA analysis."
    )

  if (identical(x_var, "data_maturity_stage")) {
    plt <- plt +
      scale_x_discrete(labels = .format_get_co2_revision_analysis_data_maturity_stage)
  }

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
  plt
}


.plot_get_co2_revision_analysis_summary_estimate_vs_reference <- function(
  plot_data,
  filepath,
  title,
  subtitle,
  width,
  height,
  dpi,
  free_scales = FALSE
) {
  stage_levels <- .get_co2_revision_analysis_data_maturity_levels()

  if (nrow(plot_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No comparable observations available") +
      theme_void() +
      labs(
        title = title,
        subtitle = subtitle,
        caption = "Source: CREA analysis."
      )
    rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
    return(plt)
  }

  plot_data <- plot_data %>%
    mutate(
      data_maturity_stage = factor(
        data_maturity_stage,
        levels = stage_levels
      ),
      data_maturity_stage_label = factor(
        .format_get_co2_revision_analysis_data_maturity_stage(data_maturity_stage),
        levels = .format_get_co2_revision_analysis_data_maturity_stage(stage_levels)
      ),
      reference_estimate_mt = reference_estimate / 1e6,
      estimate_mt = estimate / 1e6
    )
  positive_data <- plot_data %>%
    filter(
      is.finite(reference_estimate_mt),
      is.finite(estimate_mt),
      reference_estimate_mt > 0,
      estimate_mt > 0
    )

  if (nrow(positive_data) == 0) {
    plt <- ggplot() +
      annotate(
        "text",
        x = 0,
        y = 0,
        label = "No positive estimate/reference pairs available for log-log hex plots"
      ) +
      theme_void() +
      labs(
        title = title,
        subtitle = subtitle,
        caption = "Source: CREA analysis."
      )
    rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
    return(plt)
  }

  dropped_rows <- nrow(plot_data) - nrow(positive_data)
  coord_layer <- NULL
  if (!isTRUE(free_scales)) {
    limits <- range(
      c(positive_data$reference_estimate_mt, positive_data$estimate_mt),
      na.rm = TRUE
    )
    if (!all(is.finite(limits)) || any(limits <= 0)) {
      limits <- c(1e-6, 1)
    }
    if (limits[[1]] == limits[[2]]) {
      limits <- c(limits[[1]] / 10, limits[[2]] * 10)
    }
    x_scale <- scale_x_log10(limits = limits)
    y_scale <- scale_y_log10(limits = limits)
    coord_layer <- coord_equal()
  } else {
    x_scale <- scale_x_log10()
    y_scale <- scale_y_log10()
  }
  facet_formula <- stats::as.formula("~data_maturity_stage_label")
  plt <- ggplot(
    positive_data,
    aes(reference_estimate_mt, estimate_mt)
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      color = "gray70",
      linewidth = 0.4,
      linetype = "dashed"
    ) +
    geom_hex(
      bins = 35,
      aes(fill = after_stat(count)),
      na.rm = TRUE
    ) +
    facet_wrap(
      facet_formula,
      scales = if (isTRUE(free_scales)) {
        "free"
      } else {
        "fixed"
      },
      ncol = 3
    ) +
    scale_fill_viridis_c(
      option = "C",
      direction = -1,
      end = 0.9,
      name = "Hex count"
    ) +
    x_scale +
    y_scale +
    rcrea::theme_crea_new() +
    theme(legend.position = "bottom") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Reference estimate (Mt CO2)",
      y = "Estimate (Mt CO2)",
      caption = if (dropped_rows > 0) {
        paste(
          "Source: CREA analysis.",
          dropped_rows,
          "non-positive observations were excluded for log-log plotting."
        )
      } else {
        "Source: CREA analysis."
      }
    )

  if (!is.null(coord_layer)) {
    plt <- plt + coord_layer
  }

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
  plt
}


.plot_get_co2_revision_analysis_top_contributors <- function(
  plot_data,
  label_var,
  filepath,
  title,
  subtitle,
  width,
  height,
  dpi
) {
  plot_data <- plot_data %>%
    mutate(label_value = .data[[label_var]]) %>%
    arrange(gross_revision) %>%
    mutate(label_value = factor(label_value, levels = label_value))

  plt <- ggplot(plot_data, aes(gross_revision, label_value)) +
    geom_col(fill = rcrea::pal_crea[["Blue"]]) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
    rcrea::theme_crea_new() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Gross revision (Mt CO2)",
      y = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
  plt
}


.plot_get_co2_revision_analysis_trend_direction_agreement <- function(
  plot_data,
  filepath,
  width,
  height,
  dpi
) {
  plot_data <- plot_data %>%
    mutate(
      aggregation_level = factor(
        aggregation_level,
        levels = c("total", "country", "component"),
        labels = c("Total", "Country", "Component")
      ),
      trend_type = factor(
        trend_type,
        levels = c("month_on_month", "year_on_year"),
        labels = c("Month-on-month", "Year-on-year")
      )
    )

  plt <- ggplot(
    plot_data,
    aes(lag_bucket, direction_agreement_pct, fill = trend_type)
  ) +
    geom_col(position = "dodge", na.rm = TRUE) +
    facet_wrap(~aggregation_level, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    rcrea::theme_crea_new() +
    labs(
      title = "Trend direction agreement by lag bucket",
      subtitle = "Share of cases where vintage and reference changes move in the same direction",
      x = NULL,
      y = "Direction agreement",
      fill = NULL,
      caption = "Source: CREA analysis."
    )

  rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
  plt
}


.plot_get_co2_revision_analysis_detail_charts <- function(
  comparison_internal,
  country_summary,
  country_component_summary,
  output_dir,
  width,
  height,
  dpi
) {
  invisible(lapply(seq_len(nrow(country_summary)), function(i) {
    country_row <- country_summary[i, , drop = FALSE]
    country_id <- country_row$country[[1]]
    group_data <- comparison_internal %>%
      filter(aggregation_level == "country", country == country_id)

    if (nrow(group_data) == 0) {
      return(invisible(NULL))
    }

    country_dir <- file.path(
      output_dir,
      .sanitize_get_co2_revision_analysis_path_name(country_id)
    )
    create_dir(country_dir)
    plot_paths <- c(
      revision_heatmap = file.path(country_dir, "revision_heatmap.png"),
      revision_by_data_maturity_stage = file.path(
        country_dir,
        "revision_by_data_maturity_stage.png"
      )
    )

    .plot_get_co2_revision_analysis_revision_heatmap(
      group_data = group_data,
      filepath = plot_paths[["revision_heatmap"]],
      title_prefix = paste("Country", country_id),
      width = width,
      height = max(height * 1.8, 7),
      dpi = dpi
    )
    .plot_get_co2_revision_analysis_revision_by_data_maturity_stage(
      group_data = group_data,
      filepath = plot_paths[["revision_by_data_maturity_stage"]],
      title_prefix = paste("Country", country_id),
      width = width,
      height = max(height * 1.5, 6),
      dpi = dpi
    )
  }))

  invisible(lapply(seq_len(nrow(country_component_summary)), function(i) {
    component_row <- country_component_summary[i, , drop = FALSE]
    country_id <- component_row$country[[1]]
    component_name <- component_row$component[[1]]
    group_data <- comparison_internal %>%
      filter(
        aggregation_level == "component",
        country == country_id,
        component_id == component_row$component_id[[1]]
      )

    if (nrow(group_data) == 0) {
      return(invisible(NULL))
    }

    component_dir <- file.path(
      output_dir,
      .sanitize_get_co2_revision_analysis_path_name(country_id),
      .sanitize_get_co2_revision_analysis_path_name(component_name)
    )
    create_dir(component_dir)
    plot_paths <- c(
      revision_heatmap = file.path(component_dir, "revision_heatmap.png"),
      revision_by_data_maturity_stage = file.path(
        component_dir,
        "revision_by_data_maturity_stage.png"
      )
    )
    title_prefix <- paste(country_id, component_name, sep = " - ")

    .plot_get_co2_revision_analysis_revision_heatmap(
      group_data = group_data,
      filepath = plot_paths[["revision_heatmap"]],
      title_prefix = title_prefix,
      width = width,
      height = max(height * 1.8, 7),
      dpi = dpi
    )
    .plot_get_co2_revision_analysis_revision_by_data_maturity_stage(
      group_data = group_data,
      filepath = plot_paths[["revision_by_data_maturity_stage"]],
      title_prefix = title_prefix,
      width = width,
      height = max(height * 1.5, 6),
      dpi = dpi
    )
  }))
}


.plot_get_co2_revision_analysis_revision_heatmap <- function(
  group_data,
  filepath,
  title_prefix,
  width,
  height,
  dpi
) {
  stage_levels <- .get_co2_revision_analysis_data_maturity_levels()
  stage_palette <- .get_co2_revision_analysis_data_maturity_palette()
  heatmap_data <- group_data %>%
    filter(vintage_month != reference_vintage) %>%
    arrange(vintage_month, target_month)

  if (nrow(heatmap_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No comparable observations available") +
      theme_void() +
      labs(
        title = paste(title_prefix, "revision heatmap"),
        caption = "Source: CREA analysis."
      )
    rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
    return(plt)
  }

  target_levels <- group_data %>%
    distinct(target_month) %>%
    arrange(target_month) %>%
    pull(target_month)
  vintage_levels <- heatmap_data %>%
    distinct(vintage_month) %>%
    arrange(vintage_month) %>%
    pull(vintage_month)
  target_labels <- format(target_levels, "%Y-%m")
  vintage_labels <- format(vintage_levels, "%Y-%m")

  heatmap_data <- heatmap_data %>%
    mutate(
      target_month_label = factor(
        format(target_month, "%Y-%m"),
        levels = target_labels
      ),
      vintage_month_label = factor(
        format(vintage_month, "%Y-%m"),
        levels = vintage_labels
      ),
      data_maturity_stage = factor(
        data_maturity_stage,
        levels = stage_levels
      )
    )
  reference_data <- group_data %>%
    distinct(target_month, reference_estimate) %>%
    arrange(target_month) %>%
    mutate(
      target_month_label = factor(
        format(target_month, "%Y-%m"),
        levels = target_labels
      )
    )

  revision_limit <- .get_co2_revision_analysis_safe_max(abs(heatmap_data$revision))
  if (!is.finite(revision_limit) || revision_limit == 0) {
    revision_limit <- 1
  }

  top_plot <- ggplot(reference_data, aes(target_month_label, reference_estimate, group = 1)) +
    geom_line(color = rcrea::pal_crea[["Blue"]], linewidth = 0.8, na.rm = TRUE) +
    geom_point(color = rcrea::pal_crea[["Blue"]], size = 1.6, na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
    rcrea::theme_crea_new() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    labs(
      title = paste(title_prefix, "revision heatmap"),
      subtitle = paste(
        "Reference estimate time series, signed revision heatmap,",
        "and data maturity stage"
      ),
      y = "Reference estimate (Mt CO2)"
    )

  revision_plot <- ggplot(
    heatmap_data,
    aes(target_month_label, vintage_month_label, fill = revision)
  ) +
    geom_tile(color = "white", linewidth = 0.15, na.rm = TRUE) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-revision_limit, revision_limit),
      labels = scales::label_number(scale = 1e-6, suffix = "M")
    ) +
    rcrea::theme_crea_new() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    labs(
      y = "Vintage month",
      fill = "Revision\n(Mt CO2)"
    )

  stage_plot <- ggplot(
    heatmap_data,
    aes(target_month_label, vintage_month_label, fill = data_maturity_stage)
  ) +
    geom_tile(color = "white", linewidth = 0.15, na.rm = TRUE) +
    scale_fill_manual(
      values = stage_palette,
      limits = stage_levels,
      drop = FALSE,
      labels = .format_get_co2_revision_analysis_data_maturity_stage
    ) +
    rcrea::theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Target month",
      y = "Vintage month",
      fill = "Data maturity stage",
      caption = "Source: CREA analysis."
    )

  .save_get_co2_revision_analysis_stacked_plots(
    plots = list(top_plot, revision_plot, stage_plot),
    filepath = filepath,
    width = width,
    height = height,
    dpi = dpi,
    plot_heights = c(1.1, 1.5, 1.5)
  )

  invisible(list(
    top_plot = top_plot,
    revision_plot = revision_plot,
    stage_plot = stage_plot
  ))
}


.plot_get_co2_revision_analysis_revision_by_data_maturity_stage <- function(
  group_data,
  filepath,
  title_prefix,
  width,
  height,
  dpi
) {
  stage_levels <- .get_co2_revision_analysis_data_maturity_levels()
  stage_palette <- .get_co2_revision_analysis_data_maturity_palette()
  plot_data <- group_data %>%
    mutate(
      data_maturity_stage = factor(
        data_maturity_stage,
        levels = stage_levels
      )
    )

  if (nrow(plot_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No comparable observations available") +
      theme_void() +
      labs(
        title = paste(title_prefix, "revision by data maturity stage"),
        caption = "Source: CREA analysis."
      )
    rcrea::quicksave(filepath, plot = plt, width = width, height = height, dpi = dpi)
    return(plt)
  }

  point_position <- position_jitter(width = 0.18, height = 0)
  signed_plot <- ggplot(
    plot_data,
    aes(data_maturity_stage, revision, color = data_maturity_stage, fill = data_maturity_stage)
  ) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
    geom_boxplot(
      width = 0.6,
      alpha = 0.2,
      outlier.shape = NA,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    geom_point(
      position = point_position,
      alpha = 0.65,
      size = 1.5,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = stage_palette,
      limits = stage_levels,
      drop = FALSE,
      labels = .format_get_co2_revision_analysis_data_maturity_stage
    ) +
    scale_fill_manual(
      values = stage_palette,
      limits = stage_levels,
      drop = FALSE,
      labels = .format_get_co2_revision_analysis_data_maturity_stage
    ) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
    rcrea::theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels = .format_get_co2_revision_analysis_data_maturity_stage) +
    labs(
      title = paste(title_prefix, "revision by data maturity stage"),
      subtitle = "Signed and absolute revisions across historical vintages",
      x = NULL,
      y = "Revision (Mt CO2)"
    )

  absolute_plot <- ggplot(
    plot_data,
    aes(
      data_maturity_stage,
      absolute_revision,
      color = data_maturity_stage,
      fill = data_maturity_stage
    )
  ) +
    geom_boxplot(
      width = 0.6,
      alpha = 0.2,
      outlier.shape = NA,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    geom_point(
      position = point_position,
      alpha = 0.65,
      size = 1.5,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = stage_palette,
      limits = stage_levels,
      drop = FALSE,
      labels = .format_get_co2_revision_analysis_data_maturity_stage
    ) +
    scale_fill_manual(
      values = stage_palette,
      limits = stage_levels,
      drop = FALSE,
      labels = .format_get_co2_revision_analysis_data_maturity_stage
    ) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
    rcrea::theme_crea_new() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels = .format_get_co2_revision_analysis_data_maturity_stage) +
    labs(
      x = NULL,
      y = "Absolute revision (Mt CO2)",
      caption = "Source: CREA analysis."
    )

  .save_get_co2_revision_analysis_stacked_plots(
    plots = list(signed_plot, absolute_plot),
    filepath = filepath,
    width = width,
    height = height,
    dpi = dpi,
    plot_heights = c(1, 1)
  )

  invisible(list(
    signed_plot = signed_plot,
    absolute_plot = absolute_plot
  ))
}


.save_get_co2_revision_analysis_stacked_plots <- function(
  plots,
  filepath,
  width,
  height,
  dpi,
  plot_heights = rep(1, length(plots))
) {
  grobs <- lapply(plots, ggplotGrob)
  max_widths <- Reduce(
    grid::unit.pmax,
    lapply(grobs, function(grob) grob$widths)
  )
  grobs <- lapply(grobs, function(grob) {
    grob$widths <- max_widths
    grob
  })

  grDevices::png(
    filename = filepath,
    width = width,
    height = height,
    units = "in",
    res = dpi,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = length(grobs),
      ncol = 1,
      heights = grid::unit(plot_heights, "null")
    )
  ))
  for (i in seq_along(grobs)) {
    grid::pushViewport(grid::viewport(
      layout.pos.row = i,
      layout.pos.col = 1
    ))
    grid::grid.draw(grobs[[i]])
    grid::upViewport()
  }
  grid::upViewport()

  invisible(filepath)
}


.get_co2_revision_analysis_data_maturity_palette <- function() {
  c(
    monthly_missing = "#B11226",
    monthly_partial = "#E67E22",
    monthly_complete_annual_missing = "#F2C94C",
    monthly_complete_annual_partial = "#2AA198",
    monthly_complete_annual_complete = "#1F78B4"
  )
}


.get_co2_revision_analysis_data_maturity_labels <- function() {
  c(
    monthly_missing = "No monthly",
    monthly_partial = "Partial monthly",
    monthly_complete_annual_missing = "Monthly only",
    monthly_complete_annual_partial = "Partial annual",
    monthly_complete_annual_complete = "Complete annual"
  )
}


.format_get_co2_revision_analysis_data_maturity_stage <- function(x) {
  label_map <- .get_co2_revision_analysis_data_maturity_labels()
  x_chr <- as.character(x)
  labels <- unname(label_map[x_chr])
  labels[is.na(labels)] <- x_chr[is.na(labels)]
  labels
}


.sanitize_get_co2_revision_analysis_path_name <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "unknown"
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- "unknown"
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("(^_+|_+$)", "", x)
  x[x == ""] <- "unknown"

  x
}


.get_co2_revision_analysis_component_candidates <- function(
  vintage_co2,
  reference_vintage_co2
) {
  bind_rows(vintage_co2, reference_vintage_co2) %>%
    mutate(target_month = lubridate::floor_date(as.Date(date), "month")) %>%
    filter(
      estimate == "central",
      iso2 != "EU",
      !(fuel == FUEL_TOTAL & sector == SECTOR_ALL)
    ) %>%
    transmute(
      country = iso2,
      component = case_when(
        fuel == FUEL_TOTAL ~ .get_co2_revision_analysis_sector_label(sector),
        TRUE ~ .get_co2_revision_analysis_component_label(fuel, sector)
      ),
      component_id = case_when(
        fuel == FUEL_TOTAL ~ .get_co2_revision_analysis_sector_id(sector),
        TRUE ~ .get_co2_revision_analysis_component_id(fuel, sector)
      ),
      component_type = case_when(
        fuel == FUEL_TOTAL ~ "sector",
        TRUE ~ "sector_plus_commodity"
      )
    ) %>%
    distinct() %>%
    arrange(country, component)
}


.get_co2_revision_analysis_lag_bucket <- function(lag_months) {
  case_when(
    lag_months == 0 ~ "0",
    lag_months >= 1 & lag_months <= 4 ~ "1-4",
    lag_months >= 5 & lag_months <= 8 ~ "5-8",
    lag_months >= 9 & lag_months <= 12 ~ "9-12",
    lag_months >= 13 & lag_months <= 24 ~ "13-24",
    lag_months >= 25 ~ "24+",
    TRUE ~ NA_character_
  ) %>%
    factor(levels = c("0", "1-4", "5-8", "9-12", "13-24", "24+"))
}


.get_co2_revision_analysis_data_maturity_levels <- function() {
  c(
    "monthly_missing",
    "monthly_partial",
    "monthly_complete_annual_missing",
    "monthly_complete_annual_partial",
    "monthly_complete_annual_complete"
  )
}


.get_co2_revision_analysis_component_label <- function(fuel, sector) {
  paste(
    stringr::str_to_title(fuel),
    .get_co2_revision_analysis_sector_label(sector),
    sep = " - "
  )
}


.get_co2_revision_analysis_component_id <- function(fuel, sector) {
  id <- paste(fuel, sector, sep = "_")
  id <- gsub("[^a-z0-9]+", "_", tolower(id))
  gsub("(^_+|_+$)", "", id)
}


.get_co2_revision_analysis_sector_id <- function(sector) {
  sector <- tolower(sector)
  sector <- gsub("[^a-z0-9]+", "_", sector)
  gsub("(^_+|_+$)", "", sector)
}


.get_co2_revision_analysis_sector_label <- function(sector) {
  dplyr::recode(
    sector,
    !!SECTOR_ELEC := "Power Generation",
    !!SECTOR_TRANSPORT := "Transport",
    !!SECTOR_TRANSPORT_DOMESTIC := "Transport (Domestic)",
    !!SECTOR_TRANSPORT_INTERNATIONAL_AVIATION := "Transport (International Aviation)",
    !!SECTOR_TRANSPORT_INTERNATIONAL_SHIPPING := "Transport (International Shipping)",
    !!SECTOR_OTHERS := "Others (Industry & Buildings)",
    !!SECTOR_ALL := "Total",
    .default = stringr::str_to_title(gsub("_", " ", sector))
  )
}


.get_co2_revision_analysis_safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}


.get_co2_revision_analysis_safe_quantile <- function(x, probs) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }

  as.numeric(stats::quantile(x, probs = probs, names = FALSE, na.rm = TRUE))
}


.get_co2_revision_analysis_month_lag <- function(target_month, vintage_month) {
  (lubridate::year(vintage_month) - lubridate::year(target_month)) * 12 +
    (lubridate::month(vintage_month) - lubridate::month(target_month))
}


.get_co2_revision_analysis_shift_months <- function(date, months) {
  as.Date(vapply(
    as.Date(date),
    function(x) as.character(.shift_months(x, months)),
    FUN.VALUE = character(1)
  ))
}
