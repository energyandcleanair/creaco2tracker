#' Walk-forward validation for get_co2 on each month in 2025
#'
#' Runs `get_co2()` once for each month-end in `validation_year`, masking source
#' inputs with [`data_masking_as_of()`], and compares those outputs to the
#' result that would have been generated at `final_as_of`.
#'
#' @param output_folder Folder for CSV, RDS, and chart outputs.
#' @param validation_year Year to validate month by month.
#' @param final_as_of Reference date for the final comparison run.
#' @param iso2s Region IDs passed to `get_co2()`.
#' @param use_cache Whether to use source-level caches in `get_co2()`.
#' @param downscale_daily Whether to run `get_co2()` at daily frequency before
#'   aggregating to months for comparison.
#' @param data_masking_lags Named publication lags passed to
#'   [`data_masking_as_of()`].
#' @param ncv_source NCV source passed to `get_co2()`.
#' @param fill_mode Projection fill mode passed to `get_co2()`.
#' @param co2_diagnostics Whether to keep per-run `get_co2()` diagnostics.
#' @param save_runs Whether to save the full final and walk-forward CO2 outputs
#'   as RDS files.
#' @param get_co2_args Additional `get_co2()` arguments. Arguments controlled by
#'   this workflow, such as `date_to` and `data_masking`, are not accepted here.
#'
#' @return A list with run metadata, comparison tables, CO2 outputs, and chart
#'   paths.
#' @export
validate_get_co2_walk_forward_2025 <- function(
  output_folder = file.path("diagnostics", "get_co2_walk_forward_2025"),
  validation_year = 2025,
  final_as_of = "2026-03-31",
  iso2s = "EU",
  use_cache = TRUE,
  downscale_daily = FALSE,
  data_masking_lags = default_source_lags(),
  ncv_source = "iea",
  fill_mode = c("missing", "overwrite", "ratio"),
  co2_diagnostics = FALSE,
  save_runs = TRUE,
  get_co2_args = list()
) {
  fill_mode <- match.arg(fill_mode)
  final_as_of <- as.Date(final_as_of)
  target_months <- .get_co2_walk_forward_target_months(validation_year)
  as_of_dates <- .get_co2_walk_forward_month_ends(target_months)

  if (is.na(final_as_of)) {
    stop("final_as_of must be coercible to Date")
  }
  if (final_as_of < max(as_of_dates)) {
    stop("final_as_of must be on or after the last validation month-end")
  }

  .validate_get_co2_walk_forward_args(get_co2_args)
  create_dir(output_folder)

  run_info <- bind_rows(
    tibble(
      run_type = "walk_forward",
      as_of_date = as_of_dates,
      date_to = as_of_dates,
      target_month = target_months
    ),
    tibble(
      run_type = "final",
      as_of_date = final_as_of,
      date_to = final_as_of,
      target_month = as.Date(NA)
    )
  )

  message(glue("Running final get_co2 comparison as of {final_as_of}..."))
  final_masking <- data_masking_as_of(final_as_of, lags = data_masking_lags)
  final_co2 <- .run_get_co2_walk_forward_as_of(
    as_of_date = final_as_of,
    date_to = final_as_of,
    data_masking = final_masking,
    output_folder = output_folder,
    run_name = "final",
    iso2s = iso2s,
    use_cache = use_cache,
    downscale_daily = downscale_daily,
    min_year = validation_year,
    ncv_source = ncv_source,
    fill_mode = fill_mode,
    co2_diagnostics = co2_diagnostics,
    get_co2_args = get_co2_args
  )

  walk_forward_runs <- lapply(seq_along(as_of_dates), function(i) {
    as_of_date <- as_of_dates[[i]]
    target_month <- target_months[[i]]

    message(glue("Running get_co2 walk-forward as of {as_of_date}..."))
    masking <- data_masking_as_of(as_of_date, lags = data_masking_lags)
    co2 <- .run_get_co2_walk_forward_as_of(
      as_of_date = as_of_date,
      date_to = as_of_date,
      data_masking = masking,
      output_folder = output_folder,
      run_name = paste0("as_of_", as_of_date),
      iso2s = iso2s,
      use_cache = use_cache,
      downscale_daily = downscale_daily,
      min_year = validation_year,
      ncv_source = ncv_source,
      fill_mode = fill_mode,
      co2_diagnostics = co2_diagnostics,
      get_co2_args = get_co2_args
    )

    co2 %>%
      mutate(
        as_of_date = as_of_date,
        target_as_of_month = target_month
      )
  })

  walk_forward_co2 <- bind_rows(walk_forward_runs)
  comparison <- .compare_get_co2_walk_forward(
    walk_forward_co2 = walk_forward_co2,
    final_co2 = final_co2,
    target_months = target_months
  )
  latest_month_comparison <- comparison %>%
    mutate(as_of_month = lubridate::floor_date(as.Date(as_of_date), "month")) %>%
    filter(target_month == as_of_month) %>%
    select(-as_of_month)
  masking_cutoffs <- .summarise_get_co2_walk_forward_masking(
    as_of_dates = c(as_of_dates, final_as_of),
    run_type = c(rep("walk_forward", length(as_of_dates)), "final"),
    lags = data_masking_lags
  )

  readr::write_csv(run_info, file.path(output_folder, "run_info.csv"))
  readr::write_csv(masking_cutoffs, file.path(output_folder, "masking_cutoffs.csv"))
  readr::write_csv(comparison, file.path(output_folder, "walk_forward_comparison.csv"))
  readr::write_csv(
    latest_month_comparison,
    file.path(output_folder, "walk_forward_latest_month_comparison.csv")
  )

  if (save_runs) {
    saveRDS(final_co2, file.path(output_folder, "final_co2.rds"))
    saveRDS(walk_forward_co2, file.path(output_folder, "walk_forward_co2.rds"))
  }

  plot_paths <- plot_get_co2_walk_forward_validation(
    comparison = comparison,
    output_folder = output_folder,
    final_as_of = final_as_of
  )

  list(
    run_info = run_info,
    masking_cutoffs = masking_cutoffs,
    comparison = comparison,
    latest_month_comparison = latest_month_comparison,
    final_co2 = final_co2,
    walk_forward_co2 = walk_forward_co2,
    plot_paths = plot_paths
  )
}


.run_get_co2_walk_forward_as_of <- function(
  as_of_date,
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
  get_co2_args
) {
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
  co2$as_of_date <- as.Date(as_of_date)
  co2
}


.compare_get_co2_walk_forward <- function(walk_forward_co2, final_co2, target_months) {
  group_cols <- c("iso2", "fuel", "sector", "estimate", "unit", "target_month")

  final_monthly <- .aggregate_get_co2_walk_forward_monthly(
    final_co2,
    target_months = target_months
  ) %>%
    select(all_of(group_cols), final_value = value)

  walk_forward_monthly <- .aggregate_get_co2_walk_forward_monthly(
    walk_forward_co2,
    target_months = target_months
  ) %>%
    filter(target_month <= lubridate::floor_date(as.Date(as_of_date), "month")) %>%
    select(as_of_date, all_of(group_cols), walk_forward_value = value)

  walk_forward_monthly %>%
    left_join(final_monthly, by = group_cols) %>%
    mutate(
      diff = walk_forward_value - final_value,
      abs_diff = abs(diff),
      pct_diff = if_else(final_value == 0, NA_real_, diff / final_value),
      abs_pct_diff = abs(pct_diff)
    ) %>%
    arrange(as_of_date, target_month, iso2, fuel, sector, estimate)
}


.aggregate_get_co2_walk_forward_monthly <- function(co2, target_months) {
  required_cols <- c("iso2", "fuel", "sector", "estimate", "unit", "date", "value")
  missing_cols <- setdiff(required_cols, names(co2))
  if (length(missing_cols) > 0) {
    stop(glue("CO2 data is missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  group_cols <- intersect(
    c("as_of_date", "iso2", "fuel", "sector", "estimate", "unit", "target_month"),
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


.summarise_get_co2_walk_forward_masking <- function(as_of_dates, run_type, lags) {
  bind_rows(lapply(seq_along(as_of_dates), function(i) {
    cfg <- data_masking_as_of(as_of_dates[[i]], lags = lags)
    bind_rows(lapply(names(cfg), function(source_name) {
      rules <- .rule_list_from_config(cfg[[source_name]])
      if (length(rules) == 0) {
        return(tibble())
      }

      bind_rows(lapply(rules, function(rule) {
        tibble(
          run_type = run_type[[i]],
          as_of_date = as.Date(as_of_dates[[i]]),
          source_name = source_name,
          mask_date_from = .coalesce_mask_field(rule$date_from, rule$available_from),
          mask_date_to = .coalesce_mask_field(rule$date_to, rule$available_to)
        )
      }))
    }))
  }))
}


.coalesce_mask_field <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value)) {
      return(as.character(value))
    }
  }
  NA_character_
}


.get_co2_walk_forward_target_months <- function(validation_year) {
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


.get_co2_walk_forward_month_ends <- function(target_months) {
  lubridate::ceiling_date(as.Date(target_months), "month") - 1
}


.validate_get_co2_walk_forward_args <- function(get_co2_args) {
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
