#!/usr/bin/env Rscript

# Compare transparent monthly imputation models for a country's brown-coal consumption.
#
# Run after downloading the four JSON source snapshots into --input-dir:
#   annual_2010_2025.json, monthly_2010_2025.json,
#   trade_2010_2025.json, ember_GRC.json
#
# Example:
# Rscript scripts/investigate_greek_lignite_imputation.R \
#   --input-dir .tmp/h1_coal_investigation \
#   --output-dir diagnostics/greek_lignite_imputation_2026-09-16

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

parse_args <- function(args) {
  result <- list(
    input_dir = ".tmp/h1_coal_investigation",
    output_dir = "diagnostics/greek_lignite_imputation",
    country_name = "Greece",
    ember_file = "ember_GRC.json",
    application_year = 2025,
    co2_factor_t_per_tonne = 0.5595549,
    report_h1_2025_mt = 1059.4,
    report_h1_2026_mt = 1041.6
  )

  while (length(args) > 0) {
    key <- args[[1]]
    if (!key %in% c(
      "--input-dir", "--output-dir", "--co2-factor-t-per-tonne",
      "--report-h1-2025-mt", "--report-h1-2026-mt", "--country-name", "--ember-file",
      "--application-year"
    )) {
      stop("Unknown argument: ", key)
    }
    if (length(args) < 2) stop("Missing value for ", key)
    value <- args[[2]]
    name <- gsub("-", "_", sub("^--", "", key))
    result[[name]] <- if (grepl("mt$|tonne$|year$", name)) as.numeric(value) else value
    args <- args[-c(1, 2)]
  }

  result
}

read_jsonstat <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  dimension_ids <- unlist(x$id, use.names = FALSE)
  dimension_values <- lapply(dimension_ids, function(id) {
    index <- unlist(x$dimension[[id]]$category$index)
    names(sort(index))
  })
  names(dimension_values) <- dimension_ids
  # JSON-stat indexes the final dimension fastest. expand.grid() makes its first
  # argument vary fastest, hence the reverse before restoring source order.
  grid <- expand.grid(rev(dimension_values), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- rev(dimension_ids)
  grid <- grid[, dimension_ids, drop = FALSE]

  values <- rep(NA_real_, nrow(grid))
  if (length(x$value) > 0) {
    raw_values <- unlist(x$value)
    values[as.integer(names(raw_values)) + 1] <- as.numeric(raw_values)
  }
  bind_cols(as_tibble(grid), tibble(value = values))
}

normalise_weights <- function(x) {
  x <- pmax(as.numeric(x), 0)
  if (!all(is.finite(x)) || sum(x) <= 0) return(rep(1 / length(x), length(x)))
  x / sum(x)
}

annual_allocate <- function(weights, annual_total) {
  annual_total * normalise_weights(weights)
}

complete_years_before <- function(data, target_year) {
  data |>
    filter(year < target_year) |>
    count(year, name = "n_months") |>
    filter(n_months == 12) |>
    pull(year)
}

historical_weights <- function(training, target_year, n_years) {
  eligible <- complete_years_before(training, target_year)
  use_years <- tail(eligible, n_years)
  if (length(use_years) == 0) return(rep(1 / 12, 12))
  training |>
    filter(year %in% use_years) |>
    group_by(month) |>
    summarise(weight = mean(share), .groups = "drop") |>
    arrange(month) |>
    pull(weight) |>
    normalise_weights()
}

ember_weights <- function(target) {
  target |>
    arrange(month) |>
    pull(ember_coal_twh) |>
    normalise_weights()
}

blend_weights <- function(training, target, target_year, blend_weight) {
  historic <- historical_weights(training, target_year, n_years = 3)
  ember <- ember_weights(target)
  normalise_weights((1 - blend_weight) * historic + blend_weight * ember)
}

select_blend_weight <- function(training, target_year) {
  candidate_weights <- seq(0, 1, by = 0.25)
  eligible <- complete_years_before(training, target_year)
  if (length(eligible) < 2) return(0.5)

  errors <- lapply(candidate_weights, function(weight) {
    fold_errors <- vapply(eligible[-1], function(validation_year) {
      train_fold <- training |> filter(year < validation_year)
      validation <- training |> filter(year == validation_year) |> arrange(month)
      predicted <- annual_allocate(
        blend_weights(train_fold, validation, validation_year, weight),
        unique(validation$annual_total)
      )
      mean(abs(predicted - validation$consumption_kt))
    }, numeric(1))
    mean(fold_errors)
  })
  candidate_weights[[which.min(unlist(errors))]]
}

seasonal_linear_weights <- function(training, target) {
  if (nrow(training) < 36) return(NULL)
  fit <- tryCatch(
    lm(share ~ ember_share + factor(month), data = training),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  prediction <- tryCatch(predict(fit, newdata = target), error = function(e) NULL)
  if (is.null(prediction)) return(NULL)
  normalise_weights(prediction)
}

ridge_weights <- function(training, target) {
  if (!requireNamespace("glmnet", quietly = TRUE) || nrow(training) < 48) return(NULL)
  x_train <- model.matrix(~ ember_share + factor(month), data = training)[, -1, drop = FALSE]
  x_target <- model.matrix(~ ember_share + factor(month), data = target)[, -1, drop = FALSE]
  fit <- tryCatch(
    glmnet::cv.glmnet(x_train, training$share, alpha = 0, nfolds = min(10, nrow(training)),
      standardize = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  normalise_weights(as.numeric(predict(fit, x_target, s = "lambda.min")))
}

predict_primary_models <- function(training, target, target_year) {
  annual_total <- unique(target$annual_total)
  blend_weight <- select_blend_weight(training, target_year)
  weights <- list(
    flat = rep(1 / 12, 12),
    historical_share_1y = historical_weights(training, target_year, 1),
    historical_share_2y = historical_weights(training, target_year, 2),
    historical_share_3y = historical_weights(training, target_year, 3),
    ember_generation = ember_weights(target),
    blend = blend_weights(training, target, target_year, blend_weight),
    seasonal_linear = seasonal_linear_weights(training, target),
    ridge_seasonal = ridge_weights(training, target)
  )

  bind_rows(lapply(names(weights), function(model_name) {
    if (is.null(weights[[model_name]])) return(NULL)
    prediction <- annual_allocate(weights[[model_name]], annual_total)
    tibble(
      year = target_year,
      month = target$month,
      model = model_name,
      blend_weight = if (model_name == "blend") blend_weight else NA_real_,
      actual_kt = target$consumption_kt,
      predicted_kt = prediction,
      annual_total_kt = annual_total,
      annual_predicted_total_kt = sum(prediction)
    )
  }))
}

calculate_metrics <- function(predictions) {
  predictions |>
    summarise(
      n_months = n(),
      h1_actual_kt = sum(actual_kt),
      h1_predicted_kt = sum(predicted_kt),
      h1_error_kt = sum(predicted_kt) - sum(actual_kt),
      h1_abs_error_kt = abs(sum(predicted_kt) - sum(actual_kt)),
      h1_squared_error_kt2 = (sum(predicted_kt) - sum(actual_kt))^2,
      h1_percent_error = 100 * (sum(predicted_kt) / sum(actual_kt) - 1),
      monthly_mae_kt = mean(abs(predicted_kt - actual_kt)),
      monthly_rmse_kt = sqrt(mean((predicted_kt - actual_kt)^2)),
      annual_constraint_error_kt = if (all(is.na(annual_predicted_total_kt))) {
        NA_real_
      } else {
        abs(annual_predicted_total_kt[[1]] - annual_total_kt[[1]])
      },
      all_non_negative = all(predicted_kt >= 0),
      .groups = "drop"
    )
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  required_files <- c(
    "annual_2010_2025.json", "monthly_2010_2025.json", args$ember_file
  )
  missing <- required_files[!file.exists(file.path(args$input_dir, required_files))]
  if (length(missing) > 0) stop("Missing input files: ", paste(missing, collapse = ", "))

  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
  raw_dir <- file.path(args$output_dir, "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(args$input_dir, required_files), raw_dir, overwrite = TRUE)

  annual <- read_jsonstat(file.path(args$input_dir, "annual_2010_2025.json")) |>
    transmute(year = as.integer(time), balance = nrg_bal, value) |>
    pivot_wider(names_from = balance, values_from = value) |>
    transmute(
      year,
      annual_total = IC_CAL,
      annual_electricity = TI_E,
      annual_final = FC_E,
      annual_statistical_difference = STATDIFF
    )

  monthly <- read_jsonstat(file.path(args$input_dir, "monthly_2010_2025.json")) |>
    transmute(
      date = as.Date(paste0(time, "-01")),
      year = as.integer(substr(time, 1, 4)),
      month = as.integer(substr(time, 6, 7)),
      balance = nrg_bal,
      value
    ) |>
    pivot_wider(names_from = balance, values_from = value) |>
    transmute(year, month, consumption_kt = GID_CAL, production_kt = IPRD,
      stock_draw_kt = STK_CHG, energy_export_kt = EXP)

  trade_path <- file.path(args$input_dir, "trade_2010_2025.json")
  trade <- if (file.exists(trade_path)) {
    read_jsonstat(trade_path) |>
      transmute(
        year = as.integer(substr(time, 1, 4)),
        month = as.integer(substr(time, 6, 7)),
        flow,
        trade_kt = value / 10000
      ) |>
      mutate(flow = recode(flow, `1` = "imports_kt", `2` = "exports_kt")) |>
      pivot_wider(names_from = flow, values_from = trade_kt)
  } else {
    tibble(year = integer(), month = integer(), imports_kt = numeric(), exports_kt = numeric())
  }

  ember_json <- jsonlite::fromJSON(file.path(args$input_dir, args$ember_file))
  ember <- as_tibble(ember_json$data) |>
    filter(series == "Coal") |>
    transmute(
      year = as.integer(substr(date, 1, 4)),
      month = as.integer(substr(date, 6, 7)),
      ember_coal_twh = generation_twh
    )

  data <- monthly |>
    left_join(annual, by = "year") |>
    left_join(trade, by = c("year", "month")) |>
    left_join(ember, by = c("year", "month")) |>
    group_by(year) |>
    mutate(ember_share = normalise_weights(ember_coal_twh)) |>
    ungroup() |>
    mutate(share = consumption_kt / annual_total)

  coverage <- data |>
    group_by(year) |>
    summarise(
      target_months = sum(!is.na(consumption_kt)),
      production_months = sum(!is.na(production_kt)),
      stock_months = sum(!is.na(stock_draw_kt)),
      trade_export_months = sum(!is.na(exports_kt)),
      ember_months = sum(!is.na(ember_coal_twh)),
      annual_total_kt = first(annual_total),
      annual_electricity_kt = first(annual_electricity),
      annual_final_kt = first(annual_final),
      annual_statistical_difference_kt = first(annual_statistical_difference),
      .groups = "drop"
    ) |>
    mutate(eligible_primary_holdout = target_months == 12 & ember_months == 12 &
      !is.na(annual_total_kt))
  write_csv(coverage, file.path(args$output_dir, "source_coverage.csv"))

  eligible_years <- coverage |>
    filter(eligible_primary_holdout) |>
    pull(year)
  holdout_years <- eligible_years[eligible_years > min(eligible_years)]

  masked_holdouts <- tibble(
    holdout_year = holdout_years,
    held_out_months = "January-June",
    masked_energy_fields = "GID_CAL, IPRD, STK_CHG, TI_EHG_MAP, sector breakdown",
    retained_sources = "annual balance, Comext trade, Ember generation, pre-holdout history"
  )
  write_csv(masked_holdouts, file.path(args$output_dir, "masked_holdouts.csv"))

  primary_predictions <- bind_rows(lapply(holdout_years, function(holdout_year) {
    training <- data |>
      filter(year < holdout_year, !is.na(consumption_kt), !is.na(annual_total),
        !is.na(ember_coal_twh))
    target <- data |>
      filter(year == holdout_year, !is.na(annual_total),
        !is.na(ember_coal_twh)) |>
      arrange(month)
    if (nrow(target) != 12 || sum(!is.na(target$consumption_kt)) != 12) return(NULL)
    predict_primary_models(training, target, holdout_year) |>
      filter(month <= 6)
  }))

  if (nrow(primary_predictions) == 0) stop("No eligible H1 holdouts were found.")
  primary_predictions <- primary_predictions |>
    mutate(track = "primary_2025_style_missingness")
  write_csv(primary_predictions, file.path(args$output_dir, "model_predictions.csv"))

  primary_scores <- primary_predictions |>
    group_by(track, model, year, blend_weight) |>
    calculate_metrics() |>
    ungroup()
  fold_comparisons <- primary_scores |>
    select(model, year, h1_abs_error_kt) |>
    rename(model_error_kt = h1_abs_error_kt) |>
    inner_join(
      primary_scores |>
        filter(model %in% c("historical_share_3y", "ember_generation")) |>
        select(year, baseline = model, h1_abs_error_kt) |>
        pivot_wider(names_from = baseline, values_from = h1_abs_error_kt),
      by = "year"
    ) |>
    group_by(model) |>
    summarise(
      comparable_holdouts = n(),
      beats_historical_share_every_holdout = all(model_error_kt < historical_share_3y),
      beats_ember_every_holdout = all(model_error_kt < ember_generation),
      .groups = "drop"
    )
  score_summary <- primary_scores |>
    group_by(track, model) |>
    summarise(
      holdouts = n(),
      mean_h1_abs_error_kt = mean(h1_abs_error_kt),
      h1_rmse_kt = sqrt(mean(h1_squared_error_kt2)),
      mean_h1_percent_error = mean(h1_percent_error),
      mean_monthly_mae_kt = mean(monthly_mae_kt),
      mean_monthly_rmse_kt = mean(monthly_rmse_kt),
      max_annual_constraint_error_kt = max(annual_constraint_error_kt, na.rm = TRUE),
      all_non_negative = all(all_non_negative),
      .groups = "drop"
    ) |>
    left_join(fold_comparisons, by = "model")

  historical_baselines <- c("historical_share_1y", "historical_share_2y", "historical_share_3y")
  reference_scores <- score_summary |>
    filter(model %in% c(historical_baselines, "ember_generation"))
  score_summary <- score_summary |>
    mutate(
      qualifies_for_recommendation = all_non_negative & max_annual_constraint_error_kt < 1e-6 &
        beats_historical_share_every_holdout & beats_ember_every_holdout &
        abs(mean_h1_percent_error) <= 2
    )

  fallback_model <- score_summary |>
    filter(model %in% c("flat", historical_baselines, "ember_generation", "blend")) |>
    slice_min(mean_h1_abs_error_kt, n = 1, with_ties = FALSE) |>
    pull(model)
  recommendation <- score_summary |>
    filter(qualifies_for_recommendation) |>
    slice_min(mean_h1_abs_error_kt, n = 1, with_ties = FALSE) |>
    pull(model)
  recommended_model <- if (length(recommendation) == 1) recommendation else fallback_model
  score_summary <- score_summary |>
    mutate(selected = model == recommended_model,
      selection_reason = if_else(
        selected & length(recommendation) == 1,
        "Meets acceptance checks", if_else(selected, "Best-performing baseline", "Not selected")
      ))
  write_csv(primary_scores, file.path(args$output_dir, "holdout_scores.csv"))
  write_csv(score_summary, file.path(args$output_dir, "model_scores.csv"))

  # Secondary track retains the monthly balance inputs. It is deliberately not used for 2025.
  accounting_data <- data |>
    filter(!is.na(consumption_kt), !is.na(production_kt), !is.na(stock_draw_kt),
      !is.na(exports_kt)) |>
    mutate(raw_balance_kt = production_kt + stock_draw_kt + coalesce(imports_kt, 0) - exports_kt)
  accounting_predictions <- bind_rows(lapply(sort(unique(accounting_data$year)), function(holdout_year) {
    train <- accounting_data |> filter(year < holdout_year)
    target <- accounting_data |> filter(year == holdout_year, month <= 6)
    if (nrow(train) < 24 || nrow(target) != 6) return(NULL)
    fit <- lm(consumption_kt ~ raw_balance_kt, data = train)
    bind_rows(
      transmute(target, year = holdout_year, month, model = "raw_accounting", actual_kt = consumption_kt,
        predicted_kt = raw_balance_kt, annual_total_kt = annual_total,
        annual_predicted_total_kt = NA_real_),
      transmute(target, year = holdout_year, month, model = "linear_accounting_calibration",
        actual_kt = consumption_kt, predicted_kt = pmax(predict(fit, target), 0),
        annual_total_kt = annual_total, annual_predicted_total_kt = NA_real_)
    )
  }))
  if (nrow(accounting_predictions) > 0) {
    accounting_predictions <- accounting_predictions |> mutate(track = "secondary_full_monthly_balance")
    write_csv(accounting_predictions, file.path(args$output_dir, "accounting_predictions.csv"))
    accounting_scores <- accounting_predictions |>
      group_by(track, model, year) |>
      calculate_metrics() |>
      ungroup()
    write_csv(accounting_scores, file.path(args$output_dir, "accounting_scores.csv"))
  }

  target_application <- data |>
    filter(year == args$application_year, !is.na(annual_total), !is.na(ember_coal_twh)) |>
    arrange(month)
  training_application <- data |>
    filter(year < args$application_year, !is.na(consumption_kt), !is.na(annual_total), !is.na(ember_coal_twh))
  if (nrow(target_application) != 12) {
    stop("Application year has no complete annual and Ember monthly coverage: ", args$application_year)
  }
  h1_2025_predictions <- predict_primary_models(training_application, target_application, args$application_year) |>
    filter(month <= 6) |>
    mutate(track = "h1_2025_application")

  h1_errors <- primary_scores |>
    group_by(model) |>
    summarise(
      empirical_error_low_kt = quantile(h1_error_kt, 0.1, names = FALSE),
      empirical_error_high_kt = quantile(h1_error_kt, 0.9, names = FALSE),
      .groups = "drop"
    )
  h1_2025_estimates <- h1_2025_predictions |>
    group_by(model, blend_weight) |>
    summarise(h1_lignite_kt = sum(predicted_kt), .groups = "drop") |>
    left_join(h1_errors, by = "model") |>
    mutate(
      empirical_interval_low_kt = pmax(h1_lignite_kt + empirical_error_low_kt, 0),
      empirical_interval_high_kt = h1_lignite_kt + empirical_error_high_kt,
      h1_lignite_co2_mt = h1_lignite_kt * args$co2_factor_t_per_tonne / 1000,
      adjusted_report_h1_2025_mt = args$report_h1_2025_mt + h1_lignite_co2_mt,
      adjusted_report_decline_pct = 100 *
        (adjusted_report_h1_2025_mt - args$report_h1_2026_mt) / adjusted_report_h1_2025_mt,
      selected = model == recommended_model
    )
  write_csv(h1_2025_estimates, file.path(args$output_dir, "h1_2025_estimates.csv"))

  annual_2025 <- annual |> filter(year == args$application_year)
  h1_2025_sector <- h1_2025_predictions |>
    filter(model == recommended_model) |>
    mutate(
      electricity_kt = pmin(
        predicted_kt,
        annual_2025$annual_electricity[[1]] * ember_weights(target_application)[target_application$month <= 6]
      ),
      non_power_kt = predicted_kt - electricity_kt
    ) |>
    summarise(
      model = recommended_model,
      h1_total_kt = sum(predicted_kt),
      h1_electricity_kt = sum(electricity_kt),
      h1_non_power_kt = sum(non_power_kt),
      annual_electricity_kt = annual_2025$annual_electricity[[1]],
      annual_final_kt = annual_2025$annual_final[[1]],
      sector_note = "Electricity uses Ember's monthly profile; residual is lower-confidence non-power use."
    )
  write_csv(h1_2025_sector, file.path(args$output_dir, "h1_2025_sector_estimate.csv"))

  plot_data <- h1_2025_estimates |> arrange(h1_lignite_kt)
  plot <- ggplot(plot_data, aes(x = reorder(model, h1_lignite_kt), y = h1_lignite_kt)) +
    geom_col(fill = "#377eb8") +
    geom_errorbar(aes(ymin = empirical_interval_low_kt, ymax = empirical_interval_high_kt),
      width = 0.2, colour = "#333333") +
    coord_flip() +
    labs(
      title = paste0(args$country_name, " H1 ", args$application_year, " brown-coal imputation by model"),
      subtitle = "Annual Eurostat consumption constrained | bars are H1 estimates; whiskers are 10th–90th holdout errors",
      x = NULL, y = "Thousand tonnes"
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(args$output_dir, "h1_2025_model_range.png"), plot,
    width = 9, height = 5.5, dpi = 180, bg = "white")

  methodology <- c(
    paste0("# ", args$country_name, " monthly brown-coal imputation comparison"),
    "",
    paste0("The primary target is ", args$country_name,
      " monthly Eurostat `GID_CAL` brown-coal consumption. Each annual-constrained estimate sums to the annual Eurostat `IC_CAL` value."),
    "",
    "Each historical H1 holdout masks the monthly energy series, including consumption, production, stocks, power input and sector detail. It retains annual energy balances, Comext trade where supplied, Ember generation and earlier observations. The accounting models form a separate optimistic-coverage track because they need monthly production and stocks.",
    "",
    paste0(
      "The selected model is `", recommended_model,
      "`. Selection requires non-negative predictions, an exact annual constraint, lower H1 error than the 3-year historical-share and Ember baselines in every comparable holdout, and mean H1 bias within 2%. If no model meets those checks, the best baseline is selected."
    ),
    "",
    "The optional report sensitivity fields use the supplied emissions factor and report baseline. They are only meaningful when those inputs correspond to the country and fuel.",
    "",
    "Input JSON snapshots are copied to `raw/`; `source_coverage.csv`, `masked_holdouts.csv`, model outputs and the chart document the run."
  )
  write_lines(methodology, file.path(args$output_dir, "methodology.md"))

  manifest <- tibble(
    run_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input_dir = normalizePath(args$input_dir),
    country_name = args$country_name,
    ember_file = args$ember_file,
    application_year = args$application_year,
    co2_factor_t_per_tonne = args$co2_factor_t_per_tonne,
    report_h1_2025_mt = args$report_h1_2025_mt,
    report_h1_2026_mt = args$report_h1_2026_mt,
    selected_model = recommended_model
  )
  write_csv(manifest, file.path(args$output_dir, "run_manifest.csv"))

  message("Selected model: ", recommended_model)
  message("Outputs: ", normalizePath(args$output_dir))
}

main()
