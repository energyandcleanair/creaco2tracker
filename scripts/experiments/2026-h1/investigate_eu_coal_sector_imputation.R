#!/usr/bin/env Rscript

# EU coal imputation experiment for the H1 2026 report.
# Inputs: annual.csv, monthly.csv and ember.csv. See README emitted by the run.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

COAL_SIEC <- c("C0100", "C0200", "C0330", "S2000")
HISTORICAL_MODELS <- paste0("historical_level_", 1:3, "y")

coal_parse_args <- function(args) {
  out <- list(
    input_dir = ".tmp/eu_coal_sector_live",
    output_dir = "diagnostics/eu_coal_sector_imputation_2026-09-16",
    cutoff = "2026-09-16", tolerance = 0.05,
    min_evaluation_years = 5L, min_activity_coverage = 0.70
  )
  valid <- c("--input-dir", "--output-dir", "--cutoff", "--tolerance",
    "--min-evaluation-years", "--min-activity-coverage")
  while (length(args) > 0) {
    if (length(args) < 2 || !args[[1]] %in% valid) {
      stop("Every argument must be a supported --key followed by a value")
    }
    key <- gsub("-", "_", sub("^--", "", args[[1]]))
    out[[key]] <- args[[2]]
    args <- args[-c(1, 2)]
  }
  out$tolerance <- as.numeric(out$tolerance)
  out$min_evaluation_years <- as.integer(out$min_evaluation_years)
  out$min_activity_coverage <- as.numeric(out$min_activity_coverage)
  out$cutoff <- as.Date(out$cutoff)
  if (is.na(out$cutoff)) stop("--cutoff must be an ISO date")
  out
}

coal_safe_sum <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

coal_first_value <- function(x) {
  values <- unique(x[!is.na(x)])
  if (length(values) == 0) NA_real_ else values[[1]]
}

coal_require_columns <- function(x, required, name) {
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) stop(name, " is missing columns: ", paste(missing, collapse = ", "))
}

coal_tracker_factors <- function(iso2s) {
  env <- new.env(parent = globalenv())
  source("R/core_constants.R", local = env)
  source("R/data_ipcc_emission_factors.R", local = env)
  source("R/data_ipcc_ncv.R", local = env)
  cache_files <- list.files("cache", pattern = "^ieaconversion_.*\\.(RDS|parquet)$",
    full.names = TRUE)
  if (length(cache_files) == 0) stop("A cached IEA conversion-factor extract is required")
  cache_file <- cache_files[[which.max(file.info(cache_files)$mtime)]]
  conversion_raw <- if (grepl("\\.parquet$", cache_file)) {
    arrow::read_parquet(cache_file)
  } else {
    readRDS(cache_file)
  }
  mapping <- tibble::tribble(
    ~siec, ~product_raw,
    "C0100", "ANTHRACITE",
    "C0200", "LIGNITE",
    "C0330", "BKB",
    "S2000", "OIL_SHALE"
  )
  conversion <- conversion_raw |>
    filter(flow_raw == "NAVERAGE", unit == "KJ_KG", iso2 %in% iso2s) |>
    inner_join(mapping, by = "product_raw") |>
    filter(!is.na(value)) |>
    group_by(iso2, siec, year) |>
    summarise(ncv_kjkg = mean(value), .groups = "drop") |>
    group_by(iso2, siec) |>
    summarise(ncv_kjkg = mean(ncv_kjkg), .groups = "drop") |>
    group_by(siec) |>
    mutate(zscore = {
      group_sd <- sd(ncv_kjkg)
      if (is.na(group_sd) || group_sd == 0) rep(0, n()) else
        (ncv_kjkg - mean(ncv_kjkg)) / group_sd
    }) |>
    filter(is.na(zscore) | abs(zscore) < 2) |>
    select(-zscore) |>
    ungroup()
  global <- conversion |> group_by(siec) |>
    summarise(global_ncv_kjkg = mean(ncv_kjkg), .groups = "drop")
  ipcc <- env$get_ipcc_ncv() |> select(siec, ipcc_ncv_kjkg = ncv_kjkg)
  factors <- tidyr::crossing(iso2 = iso2s, siec = COAL_SIEC) |>
    left_join(conversion, by = c("iso2", "siec")) |>
    left_join(global, by = "siec") |>
    left_join(ipcc, by = "siec") |>
    mutate(
      ncv_source = case_when(
        !is.na(ncv_kjkg) ~ "IEA country average",
        !is.na(global_ncv_kjkg) ~ "IEA EU average",
        TRUE ~ "IPCC fallback"
      ),
      ncv_kjkg = coalesce(ncv_kjkg, global_ncv_kjkg, ipcc_ncv_kjkg)
    ) |>
    select(-global_ncv_kjkg, -ipcc_ncv_kjkg) |>
    left_join(env$get_ipcc_emission_factors() |>
      select(siec, co2_factor_t_per_TJ), by = "siec") |>
    mutate(co2_t_per_kt = ncv_kjkg / 1000 * co2_factor_t_per_TJ,
      conversion_cache = cache_file,
      conversion_cache_md5 = unname(tools::md5sum(cache_file)))
  if (any(is.na(factors$co2_t_per_kt))) stop("Tracker coal conversion factors are incomplete")
  factors
}

# Monthly GID_CAL is reconstructed as `total`. The provided non-power series is
# a residual, so it is retained for sensitivity only and never called validated.
coal_prepare_data <- function(monthly, annual, ember) {
  coal_require_columns(monthly, c("iso2", "year", "month", "siec", "sector",
    "consumption_kt", "production_kt", "imports_kt", "exports_kt", "stock_draw_kt"),
  "monthly.csv")
  coal_require_columns(annual, c("iso2", "year", "siec", "sector", "annual_kt"), "annual.csv")
  coal_require_columns(ember, c("iso2", "year", "month", "coal_twh"), "ember.csv")
  monthly <- monthly |>
    mutate(year = as.integer(year), month = as.integer(month)) |>
    filter(siec %in% COAL_SIEC, sector %in% c("electricity", "non_power"))
  annual <- annual |>
    mutate(year = as.integer(year)) |>
    filter(siec %in% COAL_SIEC, sector %in% c("electricity", "non_power"))
  ember <- ember |> mutate(year = as.integer(year), month = as.integer(month))

  total_monthly <- monthly |>
    group_by(iso2, year, month, siec) |>
    summarise(
      consumption_kt = if (all(c("electricity", "non_power") %in% sector)) {
        coal_safe_sum(consumption_kt)
      } else NA_real_,
      across(c(production_kt, imports_kt, exports_kt, stock_draw_kt), coal_first_value),
      .groups = "drop"
    ) |>
    mutate(sector = "total", target_status = "validated_total")
  sector_monthly <- monthly |>
    mutate(target_status = if_else(sector == "electricity", "validated_power",
      "unvalidated_residual")) |>
    mutate(across(c(production_kt, imports_kt, exports_kt, stock_draw_kt), ~ NA_real_))
  total_annual <- annual |>
    group_by(iso2, year, siec) |>
    summarise(annual_kt = if (all(c("electricity", "non_power") %in% sector)) {
      coal_safe_sum(annual_kt)
    } else NA_real_, .groups = "drop") |>
    mutate(sector = "total")
  list(
    monthly = bind_rows(sector_monthly, total_monthly) |>
      left_join(ember, by = c("iso2", "year", "month")),
    annual = bind_rows(annual, total_annual)
  )
}

coal_emissions_audit <- function(data) {
  data |>
    group_by(iso2, siec, sector, target_status) |>
    summarise(reported_months = sum(!is.na(consumption_kt)), .groups = "drop") |>
    mutate(
      emissions_status = case_when(
        target_status == "validated_power" ~ "validated_combustion_proxy",
        target_status == "validated_total" ~ "unresolved_non_energy_and_transformation_use",
        TRUE ~ "unvalidated_sector_residual"
      ),
      include_in_validated_co2 = target_status == "validated_power"
    )
}

coal_annual_reconciliation <- function(data, annual, tolerance) {
  data |>
    filter(!is.na(consumption_kt)) |>
    group_by(iso2, year, siec, sector, target_status) |>
    summarise(month_count = n_distinct(month), monthly_kt = sum(consumption_kt), .groups = "drop") |>
    filter(month_count == 12) |>
    left_join(annual, by = c("iso2", "year", "siec", "sector")) |>
    filter(!is.na(annual_kt)) |>
    mutate(
      difference_kt = monthly_kt - annual_kt,
      relative_difference = if_else(annual_kt == 0,
        if_else(abs(difference_kt) <= 1e-6, 0, NA_real_), difference_kt / abs(annual_kt)),
      within_bounds = abs(difference_kt) <= pmax(abs(annual_kt) * tolerance, 1e-6),
      source_class = "reported"
    )
}

coal_accounting_reconciliation <- function(data, annual, tolerance) {
  data |>
    filter(sector == "total") |>
    mutate(complete = !is.na(production_kt) & !is.na(imports_kt) &
      !is.na(exports_kt) & !is.na(stock_draw_kt),
    raw_accounting_kt = production_kt + imports_kt - exports_kt + stock_draw_kt) |>
    group_by(iso2, year, siec) |>
    summarise(month_count = sum(complete),
      accounting_kt = if (sum(complete) == 12) sum(raw_accounting_kt[complete]) else NA_real_,
      .groups = "drop") |>
    left_join(annual |> filter(sector == "total"), by = c("iso2", "year", "siec")) |>
    mutate(difference_kt = accounting_kt - annual_kt,
      relative_difference = if_else(annual_kt == 0,
        if_else(abs(difference_kt) <= 1e-6, 0, NA_real_), difference_kt / abs(annual_kt)),
      within_bounds = month_count == 12 & !is.na(annual_kt) &
        abs(difference_kt) <= pmax(abs(annual_kt) * tolerance, 1e-6))
}

coal_accounting_eligible <- function(reconciliation, iso2, siec, holdout_year) {
  recent <- reconciliation |>
    filter(.data$iso2 == !!iso2, .data$siec == !!siec, year < holdout_year) |>
    arrange(desc(year)) |> slice_head(n = 3)
  nrow(recent) == 3 && all(recent$within_bounds)
}

coal_complete_h1_years <- function(x, before_year) {
  x |> filter(year < before_year, month <= 6, !is.na(consumption_kt)) |>
    count(year) |> filter(n == 6) |> arrange(year) |> pull(year)
}

coal_predict_historical <- function(train_country, target, years) {
  use <- tail(coal_complete_h1_years(train_country, unique(target$year)), years)
  if (length(use) < years) return(rep(NA_real_, nrow(target)))
  monthly <- train_country |> filter(year %in% use, month %in% target$month) |>
    group_by(month) |> summarise(predicted_kt = mean(consumption_kt), .groups = "drop")
  target |> select(month) |> left_join(monthly, by = "month") |> pull(predicted_kt)
}

# Empirical partial pooling: local seasonal indices are shrunk toward the
# fuel-sector EU pattern. Sparse countries therefore borrow shape, not level.
coal_predict_partial_pool <- function(train_system, iso2, target) {
  complete <- train_system |> filter(year < unique(target$year), month <= 6,
    !is.na(consumption_kt)) |>
    group_by(iso2, year) |> filter(n_distinct(month) == 6) |>
    mutate(h1_mean = mean(consumption_kt), seasonal_index = if_else(h1_mean > 0,
      consumption_kt / h1_mean, 1)) |> ungroup()
  local <- complete |> filter(.data$iso2 == !!iso2)
  use <- tail(sort(unique(local$year)), 3)
  local <- local |> filter(year %in% use)
  if (nrow(local) == 0) return(rep(NA_real_, nrow(target)))
  local_scale <- mean(local$h1_mean)
  local_index <- local |> group_by(month) |>
    summarise(local_index = mean(seasonal_index), local_n = n(), .groups = "drop")
  shared_index <- complete |> group_by(month) |>
    summarise(shared_index = median(seasonal_index), .groups = "drop")
  target |> select(month) |> left_join(local_index, by = "month") |>
    left_join(shared_index, by = "month") |>
    mutate(weight = local_n / (local_n + 3),
      index = coalesce(weight * local_index + (1 - weight) * shared_index, shared_index),
      predicted_kt = pmax(local_scale * index, 0)) |> pull(predicted_kt)
}

coal_predict_ember <- function(train_system, iso2, target) {
  if (unique(target$sector) != "electricity" || any(is.na(target$coal_twh))) {
    return(rep(NA_real_, nrow(target)))
  }
  usable <- train_system |> filter(!is.na(consumption_kt), !is.na(coal_twh), coal_twh > 0)
  local <- usable |> filter(.data$iso2 == !!iso2)
  if (nrow(usable) < 24 || nrow(local) < 6) return(rep(NA_real_, nrow(target)))
  pooled <- sum(usable$coal_twh * usable$consumption_kt) / sum(usable$coal_twh^2)
  country <- sum(local$coal_twh * local$consumption_kt) / sum(local$coal_twh^2)
  weight <- nrow(local) / (nrow(local) + 12)
  pmax(target$coal_twh * (weight * country + (1 - weight) * pooled), 0)
}

coal_predict_accounting <- function(train, target, reconciliation, iso2, siec, calibrated) {
  if (unique(target$sector) != "total" ||
      !coal_accounting_eligible(reconciliation, iso2, siec, unique(target$year))) {
    return(rep(NA_real_, nrow(target)))
  }
  target <- target |> mutate(raw = production_kt + imports_kt - exports_kt + stock_draw_kt)
  if (any(is.na(target$raw))) return(rep(NA_real_, nrow(target)))
  if (!calibrated) return(pmax(target$raw, 0))
  train <- train |> mutate(raw = production_kt + imports_kt - exports_kt + stock_draw_kt) |>
    filter(!is.na(consumption_kt), !is.na(raw))
  if (nrow(train) < 24 || n_distinct(train$consumption_kt) < 2) return(rep(NA_real_, nrow(target)))
  fit <- lm(consumption_kt ~ raw + factor(month), data = train)
  pmax(as.numeric(predict(fit, newdata = target)), 0)
}

coal_predict_candidates <- function(data, accounting_reconciliation, holdout) {
  target <- data |> filter(iso2 == holdout$iso2, siec == holdout$siec,
    sector == holdout$sector, year == holdout$year, month <= 6) |> arrange(month)
  if (nrow(target) != 6 || any(is.na(target$consumption_kt))) return(tibble())
  train_system <- data |> filter(siec == holdout$siec, sector == holdout$sector,
    year < holdout$year)
  train_country <- train_system |> filter(iso2 == holdout$iso2)
  predictions <- list(
    raw_accounting = coal_predict_accounting(train_country, target,
      accounting_reconciliation, holdout$iso2, holdout$siec, FALSE),
    calibrated_accounting = coal_predict_accounting(train_country, target,
      accounting_reconciliation, holdout$iso2, holdout$siec, TRUE),
    ember_power = coal_predict_ember(train_system, holdout$iso2, target),
    seasonal_partial_pool = coal_predict_partial_pool(train_system, holdout$iso2, target),
    historical_level_1y = coal_predict_historical(train_country, target, 1),
    historical_level_2y = coal_predict_historical(train_country, target, 2),
    historical_level_3y = coal_predict_historical(train_country, target, 3)
  )
  bind_rows(lapply(names(predictions), function(model_name) {
    prediction <- predictions[[model_name]]
    tibble(
      iso2 = holdout$iso2, siec = holdout$siec, sector = holdout$sector,
      target_status = unique(target$target_status), year = holdout$year,
      month = target$month, model = model_name, actual_kt = target$consumption_kt,
      predicted_kt = prediction, eligible = all(is.finite(prediction))
    )
  }))
}

coal_score_candidates <- function(predictions, data, factors) {
  prior <- data |> filter(month <= 6, !is.na(consumption_kt)) |>
    group_by(iso2, siec, sector, year) |>
    summarise(prior_h1_kt = sum(consumption_kt), .groups = "drop") |>
    mutate(year = year + 1L)
  predictions |> group_by(iso2, siec, sector, target_status, year, model) |>
    summarise(eligible = all(eligible), h1_actual_kt = sum(actual_kt),
      h1_predicted_kt = if (all(eligible)) sum(predicted_kt) else NA_real_,
      monthly_mae_kt = if (all(eligible)) mean(abs(predicted_kt - actual_kt)) else NA_real_,
      .groups = "drop") |>
    left_join(prior, by = c("iso2", "siec", "sector", "year")) |>
    left_join(factors |> select(iso2, siec, co2_t_per_kt), by = c("iso2", "siec")) |>
    mutate(actual_change_kt = h1_actual_kt - prior_h1_kt,
      predicted_change_kt = h1_predicted_kt - prior_h1_kt,
      change_error_kt = predicted_change_kt - actual_change_kt,
      absolute_change_error_kt = abs(change_error_kt),
      change_error_co2_t = change_error_kt * co2_t_per_kt,
      absolute_change_error_co2_t = abs(change_error_co2_t),
      h1_error_kt = h1_predicted_kt - h1_actual_kt,
      validated_co2 = target_status == "validated_power")
}

coal_rank_sequence <- function(scores, min_evaluation_years = 5L,
                               min_activity_coverage = 0.70) {
  systems <- scores |> distinct(siec, sector, target_status)
  bind_rows(lapply(seq_len(nrow(systems)), function(i) {
    system <- systems[i, ]
    x <- scores |> filter(siec == system$siec, sector == system$sector,
      target_status == system$target_status)
    all_activity <- x |> distinct(iso2, year, h1_actual_kt) |>
      summarise(total = sum(abs(h1_actual_kt), na.rm = TRUE)) |> pull(total)
    stats <- x |> filter(eligible, !is.na(absolute_change_error_co2_t)) |>
      group_by(model) |> summarise(evaluation_years = n_distinct(year),
        countries = n_distinct(iso2), paired_folds = n(),
        mean_absolute_change_error_co2_t = mean(absolute_change_error_co2_t),
        mean_absolute_change_error_kt = mean(absolute_change_error_kt),
        signed_change_bias_kt = mean(change_error_kt), monthly_mae_kt = mean(monthly_mae_kt),
        covered_activity_kt = sum(abs(h1_actual_kt), na.rm = TRUE), .groups = "drop") |>
      mutate(activity_coverage = pmin(covered_activity_kt / all_activity, 1))
    if (nrow(stats) == 0) return(tibble())
    baseline <- stats |> filter(model %in% HISTORICAL_MODELS) |>
      slice_min(mean_absolute_change_error_co2_t, n = 1, with_ties = FALSE) |> pull(model)
    if (length(baseline) == 0) baseline <- HISTORICAL_MODELS[[1]]
    paired <- x |> filter(model %in% unique(c(stats$model, baseline)), eligible) |>
      select(iso2, year, model, absolute_change_error_co2_t) |>
      pivot_wider(names_from = model, values_from = absolute_change_error_co2_t)
    baseline_error <- paired[[baseline]]
    gates <- bind_rows(lapply(stats$model, function(model) {
      candidate <- paired[[model]]
      ok <- !is.na(candidate) & !is.na(baseline_error)
      country <- tibble(iso2 = paired$iso2[ok], win = candidate[ok] < baseline_error[ok]) |>
        group_by(iso2) |> summarise(win = mean(win) > 0.5, .groups = "drop")
      year <- tibble(year = paired$year[ok], candidate = candidate[ok],
        baseline = baseline_error[ok]) |> group_by(year) |>
        summarise(win = sum(candidate) < sum(baseline), .groups = "drop")
      tibble(model, country_win_share = if (nrow(country)) mean(country$win) else 0,
        year_win_share = if (nrow(year)) mean(year$win) else 0)
    }))
    baseline_mae <- stats$mean_absolute_change_error_co2_t[match(baseline, stats$model)]
    stats |> left_join(gates, by = "model") |>
      mutate(
        siec = system$siec,
        sector = system$sector,
        target_status = system$target_status,
        baseline_model = baseline,
        passes_shared_gate = model %in% HISTORICAL_MODELS |
          (evaluation_years >= min_evaluation_years &
            activity_coverage >= min_activity_coverage & country_win_share > 0.5 &
            year_win_share > 0.5 & mean_absolute_change_error_co2_t < baseline_mae),
        central_eligible = passes_shared_gate & target_status != "unvalidated_residual") |>
      arrange(desc(central_eligible), mean_absolute_change_error_co2_t) |> mutate(rank = row_number())
  }))
}

coal_sequence_before_year <- function(scores, year, siec, sector, target_status, settings) {
  ranked <- coal_rank_sequence(scores |> filter(.data$year < !!year, .data$siec == !!siec,
    .data$sector == !!sector, .data$target_status == !!target_status),
  settings$min_evaluation_years, settings$min_activity_coverage)
  if (nrow(ranked) == 0) return(HISTORICAL_MODELS)
  unique(c(ranked |> filter(central_eligible) |> pull(model),
    ranked |> arrange(mean_absolute_change_error_co2_t) |> pull(model), HISTORICAL_MODELS))
}

coal_apply_sequence <- function(predictions, scores, settings) {
  folds <- predictions |> distinct(iso2, siec, sector, target_status, year)
  sequence_cache <- new.env(parent = emptyenv())
  bind_rows(lapply(seq_len(nrow(folds)), function(i) {
    fold <- folds[i, ]
    cache_key <- paste(fold$siec, fold$sector, fold$target_status, fold$year, sep = "|")
    if (!exists(cache_key, envir = sequence_cache, inherits = FALSE)) {
      assign(cache_key, coal_sequence_before_year(
        scores, fold$year, fold$siec, fold$sector, fold$target_status, settings
      ), envir = sequence_cache)
    }
    sequence <- get(cache_key, envir = sequence_cache, inherits = FALSE)
    x <- predictions |> filter(iso2 == fold$iso2, siec == fold$siec,
      sector == fold$sector, target_status == fold$target_status, year == fold$year)
    eligible <- x |> group_by(model) |> summarise(eligible = all(eligible), .groups = "drop") |>
      filter(eligible, model %in% sequence) |> mutate(rank = match(model, sequence)) |> arrange(rank)
    if (nrow(eligible) == 0) return(x |> slice_head(n = 6) |>
      transmute(iso2, siec, sector, target_status, year, month, model = "unresolved",
        actual_kt, predicted_kt = NA_real_, eligible = FALSE, fallback_rank = NA_integer_))
    chosen_model <- eligible$model[[1]]
    chosen_rank <- eligible$rank[[1]]
    x |> filter(model == chosen_model) |> mutate(fallback_rank = chosen_rank)
  }))
}

coal_build_holdouts <- function(data) {
  data |> filter(year < max(year), month <= 6, !is.na(consumption_kt)) |>
    group_by(iso2, siec, sector, target_status, year) |>
    summarise(target_months = n_distinct(month), .groups = "drop") |> filter(target_months == 6) |>
    group_by(iso2, siec, sector, target_status) |> filter(year > min(year)) |> ungroup()
}

coal_impute_target_year <- function(data, accounting_reconciliation, scores, settings, target_year) {
  target_rows <- data |> filter(year == target_year, month <= 6) |>
    distinct(iso2, siec, sector, target_status, year, month, .keep_all = TRUE)
  groups <- target_rows |> distinct(iso2, siec, sector, target_status, year)
  bind_rows(lapply(seq_len(nrow(groups)), function(i) {
    group <- groups[i, ]
    target <- target_rows |> filter(iso2 == group$iso2, siec == group$siec,
      sector == group$sector, target_status == group$target_status) |> arrange(month)
    if (nrow(target) != 6) return(NULL)
    masked <- target
    masked$consumption_kt <- 0
    candidate_data <- data |> filter(!(iso2 == group$iso2 & siec == group$siec &
      sector == group$sector & year == target_year)) |> bind_rows(masked)
    candidates <- coal_predict_candidates(candidate_data, accounting_reconciliation,
      as.list(group)) |> select(month, model, predicted_kt, eligible) |>
      left_join(target |> select(month, reported_kt = consumption_kt), by = "month")
    sequence <- coal_sequence_before_year(scores, target_year, group$siec, group$sector,
      group$target_status, settings)
    bind_rows(lapply(target$month, function(target_month) {
      row <- candidates |> filter(month == target_month)
      if (!is.na(row$reported_kt[[1]])) return(tibble(iso2 = group$iso2, siec = group$siec,
        sector = group$sector, target_status = group$target_status, year = target_year,
        month = target_month, value_kt = row$reported_kt[[1]], method = "reported",
        fallback_rank = 0L, validation_status = group$target_status))
      available <- row |> filter(eligible, !is.na(predicted_kt), model %in% sequence) |>
        mutate(fallback_rank = match(model, sequence)) |> arrange(fallback_rank)
      if (nrow(available) == 0) return(tibble(iso2 = group$iso2, siec = group$siec,
        sector = group$sector, target_status = group$target_status, year = target_year,
        month = target_month, value_kt = NA_real_, method = "unresolved",
        fallback_rank = NA_integer_, validation_status = "unresolved"))
      tibble(iso2 = group$iso2, siec = group$siec, sector = group$sector,
        target_status = group$target_status, year = target_year, month = target_month,
        value_kt = available$predicted_kt[[1]], method = available$model[[1]],
        fallback_rank = available$fallback_rank[[1]], validation_status = group$target_status)
    }))
  }))
}

coal_target_candidate_sensitivity <- function(data, accounting_reconciliation, target_year, factors) {
  target_rows <- data |> filter(year == target_year, month <= 6) |>
    distinct(iso2, siec, sector, target_status, year, month, .keep_all = TRUE)
  groups <- target_rows |> distinct(iso2, siec, sector, target_status, year)
  candidates <- bind_rows(lapply(seq_len(nrow(groups)), function(i) {
    group <- groups[i, ]
    target <- target_rows |> filter(iso2 == group$iso2, siec == group$siec,
      sector == group$sector, target_status == group$target_status) |> arrange(month)
    if (nrow(target) != 6) return(NULL)
    masked <- target
    masked$consumption_kt <- 0
    candidate_data <- data |> filter(!(iso2 == group$iso2 & siec == group$siec &
      sector == group$sector & year == target_year)) |> bind_rows(masked)
    coal_predict_candidates(candidate_data, accounting_reconciliation, as.list(group)) |>
      select(iso2, siec, sector, target_status, year, month, model, predicted_kt) |>
      left_join(target |> select(month, reported_kt = consumption_kt), by = "month") |>
      mutate(value_kt = coalesce(reported_kt, predicted_kt),
        resolved = !is.na(reported_kt) | !is.na(predicted_kt))
  }))
  prior <- data |> filter(year == target_year - 1L, month <= 6,
    sector == "electricity", target_status == "validated_power") |>
    group_by(iso2, siec) |>
    summarise(prior_months = sum(!is.na(consumption_kt)),
      prior_h1_kt = coal_safe_sum(consumption_kt), .groups = "drop")
  candidates |> filter(sector == "electricity", target_status == "validated_power") |>
    left_join(factors |> select(iso2, siec, co2_t_per_kt), by = c("iso2", "siec")) |>
    left_join(prior, by = c("iso2", "siec")) |>
    filter(prior_months == 6) |>
    group_by(model, iso2, siec) |>
    summarise(
      series_resolved = all(resolved),
      resolved_months = sum(resolved),
      months = n(),
      current_h1_co2_t = if (series_resolved) sum(value_kt * co2_t_per_kt) else NA_real_,
      prior_h1_co2_t = first(prior_h1_kt * co2_t_per_kt),
      .groups = "drop"
    ) |>
    group_by(model) |>
    summarise(
      resolved_months = sum(resolved_months),
      months = sum(months),
      current_h1_co2_t = if (all(series_resolved)) sum(current_h1_co2_t) else NA_real_,
      prior_h1_co2_t = sum(prior_h1_co2_t),
      .groups = "drop"
    ) |>
    mutate(h1_change_co2_t = current_h1_co2_t - prior_h1_co2_t,
      complete = resolved_months == months)
}

coal_h1_change <- function(h1, data, factors, target_year) {
  current <- h1 |> filter(sector == "electricity", target_status == "validated_power") |>
    group_by(iso2, siec) |>
    summarise(current_months = sum(!is.na(value_kt)), current_h1_kt = coal_safe_sum(value_kt),
      .groups = "drop")
  prior <- data |> filter(year == target_year - 1L, month <= 6,
    sector == "electricity", target_status == "validated_power") |>
    group_by(iso2, siec) |>
    summarise(prior_months = sum(!is.na(consumption_kt)),
      prior_h1_kt = coal_safe_sum(consumption_kt), .groups = "drop")
  current |> full_join(prior, by = c("iso2", "siec")) |>
    left_join(factors |> select(iso2, siec, co2_t_per_kt), by = c("iso2", "siec")) |>
    mutate(complete_pair = current_months == 6 & prior_months == 6,
      current_h1_co2_t = current_h1_kt * co2_t_per_kt,
      prior_h1_co2_t = prior_h1_kt * co2_t_per_kt,
      h1_change_co2_t = current_h1_co2_t - prior_h1_co2_t)
}

coal_empirical_uncertainty <- function(fallback_scores, central_change_co2_t = NA_real_) {
  yearly <- fallback_scores |>
    filter(validated_co2, eligible, !is.na(change_error_co2_t)) |>
    group_by(year) |>
    summarise(error_co2_t = sum(change_error_co2_t), .groups = "drop")
  if (nrow(yearly) == 0) return(tibble(level = "EU", interval = "empirical_90",
    central_change_co2_t, lower_change_co2_t = NA_real_, upper_change_co2_t = NA_real_,
    error_lower_co2_t = NA_real_, error_upper_co2_t = NA_real_, historical_years = 0L))
  error_lower <- unname(quantile(yearly$error_co2_t, 0.05))
  error_upper <- unname(quantile(yearly$error_co2_t, 0.95))
  tibble(level = "EU", interval = "empirical_90", central_change_co2_t,
    lower_change_co2_t = central_change_co2_t - error_upper,
    upper_change_co2_t = central_change_co2_t - error_lower,
    error_lower_co2_t = error_lower, error_upper_co2_t = error_upper,
    historical_years = nrow(yearly))
}

coal_target_pattern_uncertainty <- function(
  fallback_predictions,
  h1,
  factors,
  central_change_co2_t
) {
  target_keys <- h1 |>
    filter(target_status == "validated_power", method != "reported") |>
    distinct(iso2, siec, month)
  if (nrow(target_keys) == 0) {
    return(tibble(level = "EU", interval = "empirical_90_target_pattern",
      central_change_co2_t, lower_change_co2_t = central_change_co2_t,
      upper_change_co2_t = central_change_co2_t, error_lower_co2_t = 0,
      error_upper_co2_t = 0, historical_years = 0L, target_cells = 0L))
  }
  yearly <- fallback_predictions |>
    filter(target_status == "validated_power", eligible) |>
    inner_join(target_keys, by = c("iso2", "siec", "month")) |>
    left_join(factors |> select(iso2, siec, co2_t_per_kt), by = c("iso2", "siec")) |>
    group_by(year) |>
    summarise(
      matched_cells = n(),
      error_co2_t = sum((predicted_kt - actual_kt) * co2_t_per_kt),
      .groups = "drop"
    ) |>
    filter(matched_cells == nrow(target_keys))
  if (nrow(yearly) == 0) {
    return(tibble(level = "EU", interval = "empirical_90_target_pattern",
      central_change_co2_t, lower_change_co2_t = NA_real_, upper_change_co2_t = NA_real_,
      error_lower_co2_t = NA_real_, error_upper_co2_t = NA_real_,
      historical_years = 0L, target_cells = nrow(target_keys)))
  }
  error_lower <- unname(quantile(yearly$error_co2_t, 0.05))
  error_upper <- unname(quantile(yearly$error_co2_t, 0.95))
  tibble(level = "EU", interval = "empirical_90_target_pattern", central_change_co2_t,
    lower_change_co2_t = central_change_co2_t - error_upper,
    upper_change_co2_t = central_change_co2_t - error_lower,
    error_lower_co2_t = error_lower, error_upper_co2_t = error_upper,
    historical_years = nrow(yearly), target_cells = nrow(target_keys))
}

coal_write_manifest <- function(files, cutoff, output_dir) {
  info <- file.info(files)
  tibble(source_file = basename(files), source_path = normalizePath(files), bytes = info$size,
    modified_at = format(info$mtime, tz = "UTC", usetz = TRUE),
    md5 = unname(tools::md5sum(files)), report_cutoff = as.character(cutoff),
    history_vintage = "current_revised") |> write_csv(file.path(output_dir, "input_manifest.csv"))
}

coal_write_note <- function(output_dir, settings, sequence, h1, scores, reconciliation,
                            eu_change, uncertainty, target_spread) {
  primary <- sequence |> filter(rank == 1) |> count(model, sort = TRUE)
  lines <- c("# EU coal imputation experiment", "",
    paste0("Report cutoff: ", settings$cutoff, ". Historical data use current revisions."), "",
    "Monthly power input is the validated combustion proxy. Total delivery is validated as a coal",
    "quantity, but its non-energy and transformation content remains unresolved. Residual non-power",
    "splits are excluded from validated sector and CO2 claims.", "", "## Run summary", "",
    paste0("- Untouched H1 evaluation years: ", n_distinct(scores$year[scores$eligible])),
    paste0("- H1 months imputed: ", sum(!h1$method %in% c("reported", "unresolved"))),
    paste0("- H1 months unresolved: ", sum(h1$method == "unresolved")),
    paste0("- Complete annual series outside the 5% bound: ", sum(!reconciliation$within_bounds)),
    "- Empirical uncertainty and candidate-method spread are separate outputs.", "",
    "## H1 2026 power result", "",
    paste0("- Estimated year-on-year change: ",
      round(eu_change$h1_change_co2_t / 1e6, 2), " MtCO2 (",
      round(eu_change$h1_change_pct, 2), "%)."),
    paste0("- Empirical 90% interval: ",
      round(uncertainty$lower_change_co2_t / 1e6, 2), " to ",
      round(uncertainty$upper_change_co2_t / 1e6, 2), " MtCO2."),
    paste0("- Complete candidate-method range: ",
      round(target_spread$candidate_min_change_co2_t / 1e6, 2), " to ",
      round(target_spread$candidate_max_change_co2_t / 1e6, 2), " MtCO2."),
    "- This result covers the validated monthly power target. Other coal use remains an accounting gap.",
    "",
    "## First method by system", "",
    if (nrow(primary)) paste0("- ", primary$model, ": ", primary$n, " systems") else
      "No sequence could be selected.", "", "This study does not enable production imputation.")
  writeLines(lines, file.path(output_dir, "decision_note.md"))
}

coal_main <- function() {
  settings <- coal_parse_args(commandArgs(trailingOnly = TRUE))
  files <- file.path(settings$input_dir, c("annual.csv", "monthly.csv", "ember.csv"))
  if (any(!file.exists(files))) stop("Input directory must contain annual.csv, monthly.csv and ember.csv")
  dir.create(settings$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(settings$output_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  file.copy(files, file.path(settings$output_dir, "raw"), overwrite = TRUE)
  coal_write_manifest(files, settings$cutoff, settings$output_dir)
  prepared <- coal_prepare_data(read_csv(files[[2]], show_col_types = FALSE),
    read_csv(files[[1]], show_col_types = FALSE), read_csv(files[[3]], show_col_types = FALSE))
  data <- prepared$monthly
  annual <- prepared$annual
  factors <- coal_tracker_factors(unique(data$iso2))
  write_csv(factors, file.path(settings$output_dir, "co2_factors.csv"))
  coverage <- data |> group_by(iso2, year, siec, sector, target_status) |>
    summarise(target_months = sum(!is.na(consumption_kt)),
      h1_target_months = sum(month <= 6 & !is.na(consumption_kt)),
      accounting_months = sum(!is.na(production_kt) & !is.na(imports_kt) &
        !is.na(exports_kt) & !is.na(stock_draw_kt)), ember_months = sum(!is.na(coal_twh)),
      .groups = "drop")
  write_csv(coverage, file.path(settings$output_dir, "source_coverage.csv"))
  mappings <- tibble::tribble(
    ~source_series, ~experiment_target, ~target_status, ~co2_treatment,
    "TI_EHG_MAP", "electricity", "validated_power", "validated combustion proxy",
    "GID_CAL", "total", "validated_total", "quantity only; emissions use unresolved",
    "GID_CAL minus TI_EHG_MAP", "non_power", "unvalidated_residual",
      "excluded from validated CO2 claims"
  )
  write_csv(mappings, file.path(settings$output_dir, "target_mappings.csv"))
  exclusions <- annual |>
    distinct(iso2, year, siec, sector) |>
    anti_join(coverage |> distinct(iso2, year, siec, sector),
      by = c("iso2", "year", "siec", "sector")) |>
    mutate(reason = "no monthly target series")
  write_csv(exclusions, file.path(settings$output_dir, "excluded_series.csv"))
  write_csv(coal_emissions_audit(data), file.path(settings$output_dir, "emissions_accounting_audit.csv"))
  reconciliation <- coal_annual_reconciliation(data, annual, settings$tolerance)
  accounting_reconciliation <- coal_accounting_reconciliation(data, annual, settings$tolerance)
  write_csv(reconciliation, file.path(settings$output_dir, "annual_reconciliation.csv"))
  write_csv(accounting_reconciliation, file.path(settings$output_dir, "accounting_reconciliation.csv"))
  if (any(!reconciliation$within_bounds)) warning("Annual reconciliation has out-of-bounds coal series")
  holdouts <- coal_build_holdouts(data)
  write_csv(holdouts |> mutate(held_out_months = "January-June",
    information_rule = "prior years plus holdout-year Ember and supply inputs"),
  file.path(settings$output_dir, "holdout_definitions.csv"))
  predictions <- bind_rows(lapply(seq_len(nrow(holdouts)), function(i) {
    coal_predict_candidates(data, accounting_reconciliation, holdouts[i, ])
  }))
  scores <- coal_score_candidates(predictions, data, factors)
  write_csv(predictions, file.path(settings$output_dir, "candidate_predictions.csv"))
  write_csv(scores, file.path(settings$output_dir, "candidate_scores.csv"))
  sequence <- coal_rank_sequence(scores, settings$min_evaluation_years,
    settings$min_activity_coverage)
  write_csv(sequence, file.path(settings$output_dir, "fallback_sequences.csv"))
  fallback_predictions <- coal_apply_sequence(predictions, scores, settings)
  fallback_scores <- coal_score_candidates(fallback_predictions, data, factors)
  write_csv(fallback_predictions, file.path(settings$output_dir, "fallback_predictions.csv"))
  write_csv(fallback_scores, file.path(settings$output_dir, "fallback_scores.csv"))
  target_year <- as.integer(format(settings$cutoff, "%Y"))
  h1 <- coal_impute_target_year(data, accounting_reconciliation, scores, settings, target_year) |>
    left_join(factors |> select(iso2, siec, co2_t_per_kt), by = c("iso2", "siec")) |>
    mutate(validated_co2_t = if_else(target_status == "validated_power",
      value_kt * co2_t_per_kt, NA_real_))
  write_csv(h1, file.path(settings$output_dir, "h1_2026_estimates.csv"))
  write_csv(h1 |> group_by(sector, target_status, method) |>
    summarise(months = n(), resolved_months = sum(!is.na(value_kt)),
      coal_kt = coal_safe_sum(value_kt), validated_co2_t = coal_safe_sum(validated_co2_t),
      .groups = "drop"), file.path(settings$output_dir, "h1_2026_summary.csv"))
  h1_change <- coal_h1_change(h1, data, factors, target_year)
  write_csv(h1_change, file.path(settings$output_dir, "h1_2026_change_by_country.csv"))
  eu_change <- h1_change |> filter(complete_pair) |>
    summarise(
      current_h1_co2_t = sum(current_h1_co2_t),
      prior_h1_co2_t = sum(prior_h1_co2_t),
      h1_change_co2_t = sum(h1_change_co2_t),
      h1_change_pct = 100 * h1_change_co2_t / prior_h1_co2_t,
      country_fuel_series = n(),
      countries = n_distinct(iso2)
    )
  write_csv(eu_change, file.path(settings$output_dir, "h1_2026_change_summary.csv"))
  target_sensitivity <- coal_target_candidate_sensitivity(
    data, accounting_reconciliation, target_year, factors
  )
  write_csv(target_sensitivity,
    file.path(settings$output_dir, "h1_2026_candidate_sensitivity.csv"))
  target_spread <- target_sensitivity |> filter(complete) |>
    summarise(
      candidate_min_change_co2_t = min(h1_change_co2_t),
      candidate_max_change_co2_t = max(h1_change_co2_t),
      candidate_spread_co2_t = candidate_max_change_co2_t - candidate_min_change_co2_t,
      candidate_methods = n()
    )
  write_csv(target_spread, file.path(settings$output_dir, "h1_2026_method_spread.csv"))
  full_gap_uncertainty <- coal_empirical_uncertainty(
    fallback_scores, eu_change$h1_change_co2_t
  )
  write_csv(full_gap_uncertainty,
    file.path(settings$output_dir, "full_gap_uncertainty.csv"))
  uncertainty <- coal_target_pattern_uncertainty(
    fallback_predictions, h1, factors, eu_change$h1_change_co2_t
  )
  write_csv(uncertainty, file.path(settings$output_dir, "uncertainty.csv"))
  spread <- predictions |> filter(eligible) |> group_by(siec, sector, target_status, year, model) |>
    summarise(h1_predicted_kt = sum(predicted_kt), .groups = "drop") |>
    group_by(siec, sector, target_status, year) |>
    summarise(candidate_min_kt = min(h1_predicted_kt), candidate_max_kt = max(h1_predicted_kt),
      candidate_spread_kt = candidate_max_kt - candidate_min_kt, .groups = "drop")
  write_csv(spread, file.path(settings$output_dir, "historical_candidate_method_spread.csv"))
  chart <- sequence |> filter(target_status != "unvalidated_residual") |>
    ggplot(aes(reorder(model, mean_absolute_change_error_kt), mean_absolute_change_error_kt,
      fill = central_eligible)) + geom_col() + coord_flip() +
    facet_grid(sector ~ siec, scales = "free_y") +
    labs(title = "Coal imputation error across EU systems",
      subtitle = "H1 year-on-year change MAE | current revised history | lower is better",
      x = NULL, y = "Mean absolute error (kt)", fill = "Central eligible") +
    theme_minimal(base_size = 11)
  ggsave(file.path(settings$output_dir, "fallback_score_chart.png"), chart,
    width = 12, height = 7, dpi = 160, bg = "white")
  coal_write_note(settings$output_dir, settings, sequence, h1, fallback_scores,
    reconciliation, eu_change, uncertainty, target_spread)
  message("Outputs: ", normalizePath(settings$output_dir))
}

if (sys.nframe() == 0) coal_main()
