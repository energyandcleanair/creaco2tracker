REV_ANALYSIS_VALIDATION_YEARS <- 2020:2024
REV_ANALYSIS_USE_CACHE <- TRUE
REV_ANALYSIS_DOWNSCALE_DAILY <- FALSE
REV_ANALYSIS_NCV_SOURCE <- "iea"
REV_ANALYSIS_FILL_MODE <- "missing"
REV_ANALYSIS_CO2_DIAGNOSTICS <- FALSE
REV_ANALYSIS_REUSE_RUN_CACHE <- TRUE
REV_ANALYSIS_SAVE_RUNS <- TRUE
REV_ANALYSIS_NEAR_ZERO_REFERENCE_THRESHOLD_TONNES_CO2 <- 100000
REV_ANALYSIS_TOP_N <- 15L
REV_ANALYSIS_TREND_MIN_N <- 6L
REV_ANALYSIS_PLOT_WIDTH <- 10
REV_ANALYSIS_PLOT_HEIGHT <- 6
REV_ANALYSIS_PLOT_DPI <- 300
REV_ANALYSIS_DATA_COLLECTION_MAX_ATTEMPTS <- 3L
REV_ANALYSIS_DATA_COLLECTION_RETRY_DELAY_SECONDS <- 300


#' Revision analysis for monthly `get_co2()` vintages
#'
#' Runs `get_co2()` once for each unique month-end needed by `validation_years`,
#' masking source inputs with [`data_masking_as_of()`], and compares each
#' validation year with its January `y + 2` reference vintage.
#'
#' Revision-analysis terminology:
#' - Vintage month: The month the pipeline is run with data available up to that point.
#' - Target month: The emissions month being estimated.
#' - Estimation lag: Difference between model vintage and target month in months.
#' - Reference vintage: Later run used for comparison.
#' - Revision: Difference between an earlier vintage and the reference vintage.
#' - Data availability regime: Source availability state used for a given vintage.
#'
#' @param output_folder Folder for CSV, RDS, and chart outputs. Defaults to a
#'   diagnostics folder named for the validation-year range.
#' @param validation_years Years to validate.
#' @param include_country_detail_charts Whether to render the per-country and
#'   per-component detail charts under `charts/details`. Skip this to make the
#'   analysis faster.
#'
#' @return A list with vintage metadata, raw CO2 outputs, the canonical vintage
#'   revision comparison table, debugging summaries, outlier rows, and summary
#'   chart paths.
#' @export
validate_get_co2_revision_analysis <- function(
  output_folder = NULL,
  validation_years = REV_ANALYSIS_VALIDATION_YEARS,
  include_country_detail_charts = FALSE
) {
  .with_revision_analysis_stacktrace(
    .validate_get_co2_revision_analysis_impl(
      output_folder = output_folder,
      validation_years = validation_years,
      include_country_detail_charts = include_country_detail_charts
    ),
    entrypoint = "validate_get_co2_revision_analysis"
  )
}


.validate_get_co2_revision_analysis_impl <- function(
  output_folder = NULL,
  validation_years = REV_ANALYSIS_VALIDATION_YEARS,
  include_country_detail_charts = TRUE
) {
  analysis_plan <- .get_co2_revision_analysis_year_plan(
    validation_years = validation_years
  )
  run_plan <- .get_co2_revision_analysis_run_plan(analysis_plan)
  comparison_min_year <- min(analysis_plan$validation_year)

  if (is.null(output_folder)) {
    year_label <- if (nrow(analysis_plan) == 1) {
      as.character(analysis_plan$validation_year[[1]])
    } else {
      paste0(min(analysis_plan$validation_year), "_", max(analysis_plan$validation_year))
    }
    output_folder <- file.path(
      "diagnostics",
      paste0("get_co2_revision_analysis_", year_label)
    )
  }

  create_dir(output_folder)
  log_info(glue::glue(
    "Using a near-zero reference threshold of ",
    "{REV_ANALYSIS_NEAR_ZERO_REFERENCE_THRESHOLD_TONNES_CO2} tonnes CO2"
  ))

  vintage_run_info <- .get_co2_revision_analysis_vintage_run_info(analysis_plan)

  all_runs <- lapply(seq_len(nrow(run_plan)), function(i) {
    vintage_date <- run_plan$date_to[[i]]
    vintage_month <- run_plan$vintage_month[[i]]
    log_info(
      glue::glue(
        "Running get_co2 revision-analysis vintage for {vintage_month} ",
        "(month-end {vintage_date})..."
      )
    )
    .collect_get_co2_revision_analysis_at_vintage(
      vintage_date = vintage_date,
      vintage_month = vintage_month,
      date_to = vintage_date,
      output_folder = output_folder,
      run_name = paste0("vintage_", vintage_date),
      min_year = comparison_min_year
    )
  })
  all_run_co2 <- bind_rows(all_runs)

  year_outputs <- lapply(seq_len(nrow(analysis_plan)), function(i) {
    plan_row <- analysis_plan[i, , drop = FALSE]
    validation_year_i <- plan_row$validation_year[[1]]
    vintage_months_i <- plan_row$vintage_months[[1]]
    target_months_i <- plan_row$target_months[[1]]
    reference_vintage_month_i <- plan_row$reference_vintage_month[[1]]

    vintage_co2_i <- all_run_co2 %>%
      filter(vintage_month %in% vintage_months_i) %>%
      mutate(validation_year = validation_year_i)
    reference_vintage_co2_i <- all_run_co2 %>%
      filter(vintage_month == reference_vintage_month_i) %>%
      mutate(validation_year = validation_year_i)
    revision_comparison_i <- .compare_get_co2_revision_analysis(
      vintage_co2 = vintage_co2_i,
      reference_vintage_co2 = reference_vintage_co2_i,
      target_months = target_months_i,
      reference_vintage_month = reference_vintage_month_i,
      validation_year = validation_year_i
    )

    list(
      vintage_co2 = vintage_co2_i,
      reference_vintage_co2 = reference_vintage_co2_i,
      revision_comparison = revision_comparison_i
    )
  })

  vintage_co2 <- bind_rows(lapply(year_outputs, `[[`, "vintage_co2"))
  reference_vintage_co2 <- bind_rows(lapply(year_outputs, `[[`, "reference_vintage_co2"))
  revision_comparison <- bind_rows(lapply(year_outputs, `[[`, "revision_comparison"))

  if (REV_ANALYSIS_SAVE_RUNS) {
    saveRDS(
      reference_vintage_co2,
      file.path(output_folder, "reference_vintage_co2.rds")
    )
    saveRDS(vintage_co2, file.path(output_folder, "vintage_co2.rds"))
    saveRDS(all_run_co2, file.path(output_folder, "all_run_co2.rds"))
  }

  plot_outputs <- plot_get_co2_revision_analysis_validation(
    revision_comparison = revision_comparison,
    vintage_co2 = vintage_co2,
    reference_vintage_co2 = reference_vintage_co2,
    output_folder = output_folder,
    analysis_plan = analysis_plan,
    include_country_detail_charts = include_country_detail_charts
  )

  list(
    vintage_run_info = vintage_run_info,
    analysis_plan = analysis_plan,
    reference_vintage_co2 = reference_vintage_co2,
    vintage_co2 = vintage_co2,
    vintage_revision_comparison = plot_outputs$vintage_revision_comparison,
    debug_revision_summary = plot_outputs$debug_revision_summary,
    revision_outliers = plot_outputs$revision_outliers,
    summary_plot_paths = plot_outputs$summary_plot_paths
  )
}


.with_revision_analysis_stacktrace <- function(expr, entrypoint) {
  captured_calls <- NULL
  tryCatch(
    withCallingHandlers(
      force(expr),
      error = function(e) {
        captured_calls <<- sys.calls()
      }
    ),
    error = function(e) {
      .print_revision_analysis_exception(
        error = e,
        calls = captured_calls,
        entrypoint = entrypoint
      )
      stop(e)
    }
  )
}


.print_revision_analysis_exception <- function(error, calls = sys.calls(), entrypoint) {
  stacktrace <- .format_revision_analysis_stacktrace(calls)
  if (identical(stacktrace, "<no stacktrace available>")) {
    stacktrace <- .format_revision_analysis_stacktrace(sys.calls())
  }

  message(glue::glue("\n========== {entrypoint} failed =========="))
  message("Exception:")
  message(conditionMessage(error))
  message("\nStacktrace:")
  message(stacktrace)
  message("=======================================\n")
}


.format_revision_analysis_stacktrace <- function(calls) {
  calls <- as.list(calls)
  if (length(calls) == 0) {
    return("<no stacktrace available>")
  }

  call_text <- vapply(
    calls,
    function(call) paste(deparse(call), collapse = " "),
    character(1)
  )
  call_text <- call_text[nzchar(call_text)]
  if (length(call_text) == 0) {
    return("<no stacktrace available>")
  }

  paste(sprintf("%02d: %s", seq_along(call_text), call_text), collapse = "\n")
}


.collect_get_co2_revision_analysis_at_vintage <- function(
  vintage_date,
  vintage_month,
  date_to,
  output_folder,
  run_name,
  min_year
) {
  attempt <- 1L
  repeat {
    result <- tryCatch(
      {
        masking <- data_masking_as_of(
          vintage_date,
          lags = default_source_lags(),
          publication_months = default_source_publication_months()
        )
        .run_get_co2_revision_analysis_at_vintage(
          vintage_date = vintage_date,
          vintage_month = vintage_month,
          date_to = date_to,
          data_masking = masking,
          output_folder = output_folder,
          run_name = run_name,
          min_year = min_year
        )
      },
      error = identity
    )

    if (!inherits(result, "error")) {
      return(result)
    }

    if (attempt >= REV_ANALYSIS_DATA_COLLECTION_MAX_ATTEMPTS) {
      stop(result)
    }

    log_warn(glue::glue(
      "Revision-analysis data collection failed for vintage {vintage_month} ",
      "(month-end {vintage_date}) on attempt {attempt}/",
      "{REV_ANALYSIS_DATA_COLLECTION_MAX_ATTEMPTS}: {conditionMessage(result)}. ",
      "Retrying in {REV_ANALYSIS_DATA_COLLECTION_RETRY_DELAY_SECONDS}s."
    ))
    Sys.sleep(REV_ANALYSIS_DATA_COLLECTION_RETRY_DELAY_SECONDS)
    attempt <- attempt + 1L
  }
}


.run_get_co2_revision_analysis_at_vintage <- function(
  vintage_date,
  vintage_month,
  date_to,
  data_masking,
  output_folder,
  run_name,
  min_year
) {
  cache_path <- .get_co2_revision_analysis_run_cache_path(
    output_folder = output_folder,
    run_name = run_name,
    date_to = date_to,
    data_masking = data_masking,
    min_year = min_year
  )

  if (REV_ANALYSIS_REUSE_RUN_CACHE && file.exists(cache_path)) {
    log_info(glue::glue("Reading cached get_co2 revision-analysis run from {cache_path}"))
    co2 <- readRDS(cache_path)
    co2$vintage_month <- as.Date(vintage_month)
    return(co2)
  }

  diagnostics_folder <- if (REV_ANALYSIS_CO2_DIAGNOSTICS) {
    file.path(output_folder, "get_co2_diagnostics", run_name)
  } else {
    NULL
  }

  args <- c(
    list(
      diagnostics_folder = diagnostics_folder,
      downscale_daily = REV_ANALYSIS_DOWNSCALE_DAILY,
      use_cache = REV_ANALYSIS_USE_CACHE,
      iso2s = get_eu_iso2s(include_eu = TRUE),
      min_year = min_year,
      date_to = date_to,
      ncv_source = REV_ANALYSIS_NCV_SOURCE,
      fill_mode = REV_ANALYSIS_FILL_MODE,
      data_masking = data_masking
    )
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
  min_year
) {
  cache_dir <- file.path(output_folder, "run_cache")
  create_dir(cache_dir)

  cache_hash <- digest::digest(list(
    date_to = as.Date(date_to),
    data_masking = data_masking,
    iso2s = get_eu_iso2s(include_eu = TRUE),
    downscale_daily = REV_ANALYSIS_DOWNSCALE_DAILY,
    min_year = min_year,
    ncv_source = REV_ANALYSIS_NCV_SOURCE,
    fill_mode = REV_ANALYSIS_FILL_MODE,
    co2_diagnostics = REV_ANALYSIS_CO2_DIAGNOSTICS
  ))

  file.path(cache_dir, paste0(run_name, "_", cache_hash, ".rds"))
}


.compare_get_co2_revision_analysis <- function(
  vintage_co2,
  reference_vintage_co2,
  target_months,
  reference_vintage_month,
  validation_year = NA_integer_
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
      validation_year = as.integer(validation_year),
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
  history_years = 0
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


.get_co2_revision_analysis_vintage_months <- function(validation_year) {
  validation_year <- as.integer(validation_year)
  if (length(validation_year) != 1 || is.na(validation_year)) {
    stop("validation_year must be a single integer year")
  }

  seq.Date(
    from = as.Date(sprintf("%d-01-01", validation_year)),
    to = as.Date(sprintf("%d-12-01", validation_year + 1L)),
    by = "month"
  )
}


.get_co2_revision_analysis_reference_vintage_month <- function(validation_year) {
  validation_year <- as.integer(validation_year)
  if (length(validation_year) != 1 || is.na(validation_year)) {
    stop("validation_year must be a single integer year")
  }

  as.Date(sprintf("%d-01-01", validation_year + 2L))
}


.get_co2_revision_analysis_year_plan <- function(
  validation_years = REV_ANALYSIS_VALIDATION_YEARS
) {
  validation_years <- as.integer(validation_years)
  if (length(validation_years) == 0 || any(is.na(validation_years))) {
    stop("validation_years must contain one or more integer years")
  }
  validation_years <- sort(unique(validation_years))

  bind_rows(lapply(seq_along(validation_years), function(i) {
    validation_year <- validation_years[[i]]
    target_months <- .get_co2_revision_analysis_comparison_target_months(validation_year)
    vintage_months <- .get_co2_revision_analysis_vintage_months(validation_year)
    following_year_vintage_months <- seq.Date(
      from = as.Date(sprintf("%d-01-01", validation_year + 1L)),
      to = as.Date(sprintf("%d-12-01", validation_year + 1L)),
      by = "month"
    )
    reference_vintage_month <- .get_co2_revision_analysis_reference_vintage_month(
      validation_year
    )
    vintage_dates <- .get_co2_revision_analysis_month_ends(vintage_months)
    following_year_vintage_dates <- .get_co2_revision_analysis_month_ends(
      following_year_vintage_months
    )
    reference_vintage_date <- .get_co2_revision_analysis_month_ends(
      reference_vintage_month
    )[[1]]

    if (reference_vintage_date < max(vintage_dates)) {
      stop("reference_vintage_month must be on or after the last vintage month")
    }

    tibble(
      validation_year = validation_year,
      target_start_month = min(target_months),
      target_end_month = max(target_months),
      vintage_start_month = min(vintage_months),
      vintage_end_month = max(vintage_months),
      following_year_vintage_start_month = min(following_year_vintage_months),
      following_year_vintage_end_month = max(following_year_vintage_months),
      reference_vintage_month = reference_vintage_month,
      reference_vintage_date = reference_vintage_date,
      target_months = list(target_months),
      vintage_months = list(vintage_months),
      vintage_dates = list(vintage_dates),
      following_year_vintage_months = list(following_year_vintage_months),
      following_year_vintage_dates = list(following_year_vintage_dates)
    )
  }))
}


.get_co2_revision_analysis_run_plan <- function(analysis_plan) {
  run_months <- sort(unique(as.Date(c(
    as.Date(unlist(analysis_plan$vintage_months), origin = "1970-01-01"),
    analysis_plan$reference_vintage_month
  ))))

  tibble(
    vintage_month = run_months,
    date_to = .get_co2_revision_analysis_month_ends(run_months)
  )
}


.get_co2_revision_analysis_vintage_run_info <- function(analysis_plan) {
  bind_rows(lapply(seq_len(nrow(analysis_plan)), function(i) {
    validation_year <- analysis_plan$validation_year[[i]]
    vintage_months <- analysis_plan$vintage_months[[i]]
    reference_vintage_month <- analysis_plan$reference_vintage_month[[i]]
    bind_rows(
      tibble(
        validation_year = validation_year,
        vintage_role = "vintage",
        vintage_month = vintage_months,
        date_to = .get_co2_revision_analysis_month_ends(vintage_months),
        reference_vintage_month = reference_vintage_month
      ),
      tibble(
        validation_year = validation_year,
        vintage_role = "reference_vintage",
        vintage_month = reference_vintage_month,
        date_to = .get_co2_revision_analysis_month_ends(reference_vintage_month),
        reference_vintage_month = reference_vintage_month
      )
    )
  })) %>%
    arrange(validation_year, vintage_role, vintage_month)
}


.get_co2_revision_analysis_month_ends <- function(target_months) {
  lubridate::ceiling_date(as.Date(target_months), "month") - 1
}
