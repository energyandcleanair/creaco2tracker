#' Revision analysis for monthly `get_co2()` vintages
#'
#' Runs `get_co2()` once for each month-end in `validation_year`, masking source
#' inputs with [`data_masking_as_of()`], and compares each vintage month with a
#' later reference vintage.
#'
#' Revision-analysis terminology:
#' - Vintage month: The month the pipeline is run with data available up to that point.
#' - Target month: The emissions month being estimated.
#' - Estimation lag: Difference between model vintage and target month in months.
#' - Reference vintage: Later run used for comparison.
#' - Revision: Difference between an earlier vintage and the reference vintage.
#' - Data availability regime: Source availability state used for a given vintage.
#'
#' @param output_folder Folder for CSV, RDS, and chart outputs. Defaults to
#'   `diagnostics/get_co2_revision_analysis_<validation_year>`.
#' @param validation_year Year to validate month by month.
#' @param reference_vintage_month Date-like input identifying the reference
#'   vintage month. It is normalized to the first day of that month for output
#'   metadata and executed at the corresponding month-end.
#' @param iso2s Region IDs passed to `get_co2()`. Defaults to all EU countries
#'   plus the EU aggregate.
#' @param use_cache Whether to use source-level caches in `get_co2()`.
#' @param downscale_daily Whether to run `get_co2()` at daily frequency before
#'   aggregating to months for comparison.
#' @param data_masking_lags Named publication lags passed to
#'   [`data_masking_as_of()`].
#' @param data_masking_publication_months Named annual publication months passed
#'   to [`data_masking_as_of()`].
#' @param ncv_source NCV source passed to `get_co2()`.
#' @param fill_mode Projection fill mode passed to `get_co2()`.
#' @param co2_diagnostics Whether to keep per-run `get_co2()` diagnostics.
#' @param reuse_run_cache Whether to reuse cached revision-analysis run outputs
#'   from `output_folder/run_cache` when available. Fresh runs are always saved
#'   back to that cache.
#' @param save_runs Whether to save the full reference-vintage and vintage CO2
#'   outputs as RDS files.
#' @param near_zero_reference_threshold_tonnes_co2 Threshold in tonnes CO2 used
#'   to suppress percentage-based revision metrics for near-zero reference
#'   estimates. The default is 100000 tonnes CO2 (0.1 Mt CO2).
#' @param get_co2_args Additional `get_co2()` arguments. Arguments controlled by
#'   this workflow, such as `date_to` and `data_masking`, are not accepted here.
#'
#' @return A list with vintage metadata, raw CO2 outputs, the canonical vintage
#'   revision comparison table, debugging summaries, outlier rows, and summary
#'   chart paths.
#' @export
validate_get_co2_revision_analysis <- function(
  output_folder = NULL,
  validation_year = 2025,
  reference_vintage_month = "2026-03-31",
  iso2s = get_eu_iso2s(include_eu = TRUE),
  use_cache = TRUE,
  downscale_daily = FALSE,
  data_masking_lags = default_source_lags(),
  data_masking_publication_months = default_source_publication_months(),
  ncv_source = "iea",
  fill_mode = c("missing", "overwrite", "ratio"),
  co2_diagnostics = FALSE,
  reuse_run_cache = TRUE,
  save_runs = TRUE,
  near_zero_reference_threshold_tonnes_co2 = 100000,
  get_co2_args = list()
) {
  fill_mode <- match.arg(fill_mode)
  validation_year <- as.integer(validation_year)
  reference_vintage_month <- as.Date(reference_vintage_month)
  if (is.na(reference_vintage_month)) {
    stop("reference_vintage_month must be coercible to Date")
  }

  reference_vintage_month <- lubridate::floor_date(reference_vintage_month, "month")
  target_months <- .get_co2_revision_analysis_target_months(validation_year)
  comparison_target_months <- .get_co2_revision_analysis_comparison_target_months(validation_year)
  comparison_min_year <- lubridate::year(min(comparison_target_months))
  vintage_months <- target_months
  vintage_dates <- .get_co2_revision_analysis_month_ends(vintage_months)
  reference_vintage_date <- .get_co2_revision_analysis_month_ends(reference_vintage_month)[[1]]

  if (is.null(output_folder)) {
    output_folder <- file.path(
      "diagnostics",
      paste0("get_co2_revision_analysis_", validation_year)
    )
  }
  if (reference_vintage_date < max(vintage_dates)) {
    stop("reference_vintage_month must be on or after the last validation month")
  }

  .validate_get_co2_revision_analysis_args(get_co2_args)
  create_dir(output_folder)
  log_info(glue::glue(
    "Using a near-zero reference threshold of ",
    "{near_zero_reference_threshold_tonnes_co2} tonnes CO2"
  ))

  vintage_run_info <- bind_rows(
    tibble(
      vintage_role = "vintage",
      vintage_month = vintage_months,
      date_to = vintage_dates,
      reference_vintage_month = reference_vintage_month
    ),
    tibble(
      vintage_role = "reference_vintage",
      vintage_month = reference_vintage_month,
      date_to = reference_vintage_date,
      reference_vintage_month = reference_vintage_month
    )
  )

  log_info(
    glue::glue(
      "Running get_co2 reference vintage for {reference_vintage_month} ",
      "(month-end {reference_vintage_date})..."
    )
  )
  reference_masking <- data_masking_as_of(
    reference_vintage_date,
    lags = data_masking_lags,
    publication_months = data_masking_publication_months
  )
  reference_vintage_co2 <- .run_get_co2_revision_analysis_at_vintage(
    vintage_date = reference_vintage_date,
    vintage_month = reference_vintage_month,
    date_to = reference_vintage_date,
    data_masking = reference_masking,
    output_folder = output_folder,
    run_name = paste0("reference_vintage_", reference_vintage_date),
    iso2s = iso2s,
    use_cache = use_cache,
    downscale_daily = downscale_daily,
    min_year = comparison_min_year,
    ncv_source = ncv_source,
    fill_mode = fill_mode,
    co2_diagnostics = co2_diagnostics,
    reuse_run_cache = reuse_run_cache,
    get_co2_args = get_co2_args
  )

  vintage_runs <- lapply(seq_along(vintage_dates), function(i) {
    vintage_date <- vintage_dates[[i]]
    vintage_month <- vintage_months[[i]]

    log_info(
      glue::glue(
        "Running get_co2 vintage for {vintage_month} ",
        "(month-end {vintage_date})..."
      )
    )
    masking <- data_masking_as_of(
      vintage_date,
      lags = data_masking_lags,
      publication_months = data_masking_publication_months
    )
    .run_get_co2_revision_analysis_at_vintage(
      vintage_date = vintage_date,
      vintage_month = vintage_month,
      date_to = vintage_date,
      data_masking = masking,
      output_folder = output_folder,
      run_name = paste0("vintage_", vintage_date),
      iso2s = iso2s,
      use_cache = use_cache,
      downscale_daily = downscale_daily,
      min_year = comparison_min_year,
      ncv_source = ncv_source,
      fill_mode = fill_mode,
      co2_diagnostics = co2_diagnostics,
      reuse_run_cache = reuse_run_cache,
      get_co2_args = get_co2_args
    )
  })

  vintage_co2 <- bind_rows(vintage_runs)
  revision_comparison <- .compare_get_co2_revision_analysis(
    vintage_co2 = vintage_co2,
    reference_vintage_co2 = reference_vintage_co2,
    target_months = comparison_target_months,
    reference_vintage_month = reference_vintage_month
  )

  if (save_runs) {
    saveRDS(
      reference_vintage_co2,
      file.path(output_folder, "reference_vintage_co2.rds")
    )
    saveRDS(vintage_co2, file.path(output_folder, "vintage_co2.rds"))
  }

  plot_outputs <- plot_get_co2_revision_analysis_validation(
    revision_comparison = revision_comparison,
    vintage_co2 = vintage_co2,
    reference_vintage_co2 = reference_vintage_co2,
    output_folder = output_folder,
    reference_vintage_month = reference_vintage_month,
    target_months = comparison_target_months,
    vintage_dates = vintage_dates,
    vintage_months = vintage_months,
    data_masking_lags = data_masking_lags,
    data_masking_publication_months = data_masking_publication_months,
    near_zero_reference_threshold_tonnes_co2 = near_zero_reference_threshold_tonnes_co2
  )

  list(
    vintage_run_info = vintage_run_info,
    reference_vintage_co2 = reference_vintage_co2,
    vintage_co2 = vintage_co2,
    vintage_revision_comparison = plot_outputs$vintage_revision_comparison,
    debug_revision_summary = plot_outputs$debug_revision_summary,
    revision_outliers = plot_outputs$revision_outliers,
    summary_plot_paths = plot_outputs$summary_plot_paths
  )
}


.run_get_co2_revision_analysis_at_vintage <- function(
  vintage_date,
  vintage_month,
  date_to,
  data_masking,
  output_folder,
  run_name,
  iso2s,
  use_cache,
  downscale_daily,
  min_year,
  ncv_source,
  fill_mode,
  co2_diagnostics,
  reuse_run_cache,
  get_co2_args
) {
  cache_path <- .get_co2_revision_analysis_run_cache_path(
    output_folder = output_folder,
    run_name = run_name,
    date_to = date_to,
    data_masking = data_masking,
    iso2s = iso2s,
    downscale_daily = downscale_daily,
    min_year = min_year,
    ncv_source = ncv_source,
    fill_mode = fill_mode,
    co2_diagnostics = co2_diagnostics,
    get_co2_args = get_co2_args
  )

  if (isTRUE(reuse_run_cache) && file.exists(cache_path)) {
    log_info(glue::glue("Reading cached get_co2 revision-analysis run from {cache_path}"))
    co2 <- readRDS(cache_path)
    co2$vintage_month <- as.Date(vintage_month)
    return(co2)
  }

  diagnostics_folder <- if (isTRUE(co2_diagnostics)) {
    file.path(output_folder, "get_co2_diagnostics", run_name)
  } else {
    NULL
  }

  args <- c(
    list(
      diagnostics_folder = diagnostics_folder,
      downscale_daily = downscale_daily,
      use_cache = use_cache,
      iso2s = iso2s,
      min_year = min_year,
      date_to = date_to,
      ncv_source = ncv_source,
      fill_mode = fill_mode,
      data_masking = data_masking
    ),
    get_co2_args
  )

  co2 <- do.call(get_co2, args)
  co2$vintage_month <- as.Date(vintage_month)
  saveRDS(co2, cache_path)
  co2
}


.get_co2_revision_analysis_run_cache_path <- function(
  output_folder,
  run_name,
  date_to,
  data_masking,
  iso2s,
  downscale_daily,
  min_year,
  ncv_source,
  fill_mode,
  co2_diagnostics,
  get_co2_args
) {
  cache_dir <- file.path(output_folder, "run_cache")
  create_dir(cache_dir)

  cache_hash <- digest::digest(list(
    date_to = as.Date(date_to),
    data_masking = data_masking,
    iso2s = iso2s,
    downscale_daily = downscale_daily,
    min_year = min_year,
    ncv_source = ncv_source,
    fill_mode = fill_mode,
    co2_diagnostics = co2_diagnostics,
    get_co2_args = get_co2_args
  ))

  file.path(cache_dir, paste0(run_name, "_", cache_hash, ".rds"))
}


.compare_get_co2_revision_analysis <- function(
  vintage_co2,
  reference_vintage_co2,
  target_months,
  reference_vintage_month
) {
  group_cols <- c("iso2", "fuel", "sector", "estimate", "unit", "target_month")

  reference_vintage_monthly <- .aggregate_get_co2_revision_analysis_monthly(
    reference_vintage_co2,
    target_months = target_months
  ) %>%
    select(all_of(group_cols), reference_vintage_value = value)

  vintage_monthly <- .aggregate_get_co2_revision_analysis_monthly(
    vintage_co2,
    target_months = target_months
  ) %>%
    filter(target_month <= vintage_month) %>%
    select(vintage_month, all_of(group_cols), vintage_value = value)

  vintage_monthly %>%
    left_join(reference_vintage_monthly, by = group_cols) %>%
    mutate(
      reference_vintage_month = as.Date(reference_vintage_month),
      estimation_lag = .get_co2_revision_analysis_month_lag(target_month, vintage_month),
      revision = vintage_value - reference_vintage_value,
      absolute_revision = abs(revision),
      revision_pct = if_else(reference_vintage_value == 0, NA_real_, revision / reference_vintage_value),
      absolute_revision_pct = abs(revision_pct)
    ) %>%
    arrange(vintage_month, target_month, iso2, fuel, sector, estimate)
}


.aggregate_get_co2_revision_analysis_monthly <- function(co2, target_months) {
  required_cols <- c("iso2", "fuel", "sector", "estimate", "unit", "date", "value")
  missing_cols <- setdiff(required_cols, names(co2))
  if (length(missing_cols) > 0) {
    stop(glue("CO2 data is missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  group_cols <- intersect(
    c("vintage_month", "iso2", "fuel", "sector", "estimate", "unit", "target_month"),
    c(names(co2), "target_month")
  )

  co2 %>%
    mutate(
      date = as.Date(date),
      target_month = lubridate::floor_date(date, "month")
    ) %>%
    filter(target_month %in% target_months) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
      .groups = "drop"
    )
}


.summarise_get_co2_revision_analysis_data_availability_regime <- function(
  vintage_dates,
  vintage_months,
  vintage_role,
  lags,
  publication_months
) {
  bind_rows(lapply(seq_along(vintage_dates), function(i) {
    cfg <- data_masking_as_of(
      vintage_dates[[i]],
      lags = lags,
      publication_months = publication_months
    )
    bind_rows(lapply(names(cfg), function(source_name) {
      rules <- .rule_list_from_config(cfg[[source_name]])
      if (length(rules) == 0) {
        return(tibble())
      }

      bind_rows(lapply(rules, function(rule) {
        tibble(
          vintage_role = vintage_role[[i]],
          vintage_month = as.Date(vintage_months[[i]]),
          source_name = source_name,
          unavailable_from = if (!is.null(rule$date_from)) as.character(rule$date_from) else NA_character_,
          unavailable_to = if (!is.null(rule$date_to)) as.character(rule$date_to) else NA_character_
        )
      }))
    }))
  }))
}


.get_co2_revision_analysis_target_months <- function(validation_year) {
  validation_year <- as.integer(validation_year)
  if (length(validation_year) != 1 || is.na(validation_year)) {
    stop("validation_year must be a single integer year")
  }

  seq.Date(
    from = as.Date(sprintf("%d-01-01", validation_year)),
    to = as.Date(sprintf("%d-12-01", validation_year)),
    by = "month"
  )
}


.get_co2_revision_analysis_comparison_target_months <- function(
  validation_year,
  history_years = 1
) {
  validation_year <- as.integer(validation_year)
  history_years <- as.integer(history_years)

  if (length(validation_year) != 1 || is.na(validation_year)) {
    stop("validation_year must be a single integer year")
  }
  if (length(history_years) != 1 || is.na(history_years) || history_years < 0) {
    stop("history_years must be a single non-negative integer")
  }

  seq.Date(
    from = as.Date(sprintf("%d-01-01", validation_year - history_years)),
    to = as.Date(sprintf("%d-12-01", validation_year)),
    by = "month"
  )
}


.get_co2_revision_analysis_month_ends <- function(target_months) {
  lubridate::ceiling_date(as.Date(target_months), "month") - 1
}


.validate_get_co2_revision_analysis_args <- function(get_co2_args) {
  if (!is.list(get_co2_args)) {
    stop("get_co2_args must be a list")
  }

  reserved_args <- c(
    "diagnostics_folder",
    "downscale_daily",
    "use_cache",
    "iso2s",
    "min_year",
    "date_to",
    "ncv_source",
    "fill_mode",
    "data_masking"
  )
  invalid_args <- intersect(names(get_co2_args), reserved_args)
  if (length(invalid_args) > 0) {
    stop(
      glue(
        "get_co2_args cannot include workflow-controlled arguments: ",
        "{paste(invalid_args, collapse = ', ')}"
      )
    )
  }
}
