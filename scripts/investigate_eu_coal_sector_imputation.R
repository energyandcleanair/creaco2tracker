#!/usr/bin/env Rscript

# Reproducible EU coal-sector imputation experiment.
#
# Input CSVs in --input-dir are frozen source extracts, one row per observation:
#   annual.csv: iso2,year,siec,sector,annual_kt
#   monthly.csv: iso2,year,month,siec,sector,consumption_kt,production_kt,
#                imports_kt,exports_kt,stock_draw_kt
#   ember.csv: iso2,year,month,coal_twh
#
# `sector` is `electricity` or `non_power`. Annual total and electricity
# balances should be converted from Eurostat before running; non-power is the
# annual residual. Coke and peat must not be included in the extracts.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
})

coal_siec <- c("C0100", "C0200", "C0330", "S2000")

parse_args <- function(args) {
  out <- list(input_dir = ".tmp/eu_coal_sector_imputation",
              output_dir = "diagnostics/eu_coal_sector_imputation",
              tolerance = 0.05)
  while (length(args) > 0) {
    key <- args[[1]]
    if (!key %in% c("--input-dir", "--output-dir", "--tolerance") || length(args) < 2) {
      stop("Expected --input-dir, --output-dir, or --tolerance followed by a value")
    }
    out[[gsub("-", "_", sub("^--", "", key))]] <- args[[2]]
    args <- args[-c(1, 2)]
  }
  out$tolerance <- as.numeric(out$tolerance)
  out
}

normalise <- function(x) {
  x <- pmax(x, 0)
  if (any(!is.finite(x)) || sum(x) <= 0) return(rep(1 / length(x), length(x)))
  x / sum(x)
}

complete_years <- function(x, year) {
  x |> filter(year < !!year) |> count(year) |> filter(n == 12) |> pull(year)
}

history_weights <- function(train, year, years) {
  use <- tail(complete_years(train, year), years)
  if (length(use) == 0) return(rep(1 / 12, 12))
  train |> filter(year %in% use) |> group_by(month) |>
    summarise(weight = mean(share), .groups = "drop") |> arrange(month) |>
    pull(weight) |> normalise()
}

allocate <- function(weights, annual_kt) annual_kt * normalise(weights)

predict_primary <- function(train, target) {
  year <- unique(target$year)
  annual_kt <- unique(target$annual_kt)
  models <- list(
    flat = rep(1 / 12, 12),
    historical_share_1y = history_weights(train, year, 1),
    historical_share_2y = history_weights(train, year, 2),
    historical_share_3y = history_weights(train, year, 3),
    seasonal_fallback = history_weights(train, year, 3)
  )
  # Ember is intentionally restricted to electricity; it is not a proxy for
  # industry, coke, or other non-power coal use.
  if (unique(target$sector) == "electricity") {
    models$ember_generation <- normalise(target$coal_twh)
  }
  bind_rows(lapply(names(models), function(model) {
    prediction <- allocate(models[[model]], annual_kt)
    tibble(month = target$month, model, actual_kt = target$consumption_kt,
           predicted_kt = prediction, annual_kt)
  }))
}

# The annual allocation is made over all twelve months. H1 score rows retain
# the full-year allocation total separately so compliance is unambiguous.
summarise_scores <- function(predictions, annual_predictions) {
  h1 <- predictions |> group_by(model, iso2, siec, sector, year) |>
    summarise(h1_actual_kt = sum(actual_kt), h1_predicted_kt = sum(predicted_kt),
      h1_error_kt = h1_predicted_kt - h1_actual_kt,
      h1_abs_error_kt = abs(h1_error_kt),
      h1_squared_error_kt2 = h1_error_kt^2,
      h1_percent_error = 100 * h1_error_kt / h1_actual_kt,
      monthly_mae_kt = mean(abs(predicted_kt - actual_kt)), .groups = "drop")
  h1 |> left_join(annual_predictions, by = c("model", "iso2", "siec", "sector", "year"))
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  files <- file.path(args$input_dir, c("annual.csv", "monthly.csv", "ember.csv"))
  if (any(!file.exists(files))) stop("Input directory must contain annual.csv, monthly.csv and ember.csv")
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
  raw_dir <- file.path(args$output_dir, "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(files, raw_dir, overwrite = TRUE)

  annual <- read_csv(files[[1]], show_col_types = FALSE) |>
    filter(siec %in% coal_siec, sector %in% c("electricity", "non_power"))
  monthly <- read_csv(files[[2]], show_col_types = FALSE) |>
    filter(siec %in% coal_siec, sector %in% c("electricity", "non_power"))
  ember <- read_csv(files[[3]], show_col_types = FALSE)
  data <- monthly |> left_join(annual, by = c("iso2", "year", "siec", "sector")) |>
    left_join(ember, by = c("iso2", "year", "month")) |>
    group_by(iso2, year, siec, sector) |>
    mutate(share = consumption_kt / annual_kt) |> ungroup()

  coverage <- data |> group_by(iso2, year, siec, sector) |>
    summarise(target_months = sum(!is.na(consumption_kt)),
      accounting_months = sum(!is.na(production_kt) & !is.na(imports_kt) &
        !is.na(exports_kt) & !is.na(stock_draw_kt)),
      ember_months = sum(!is.na(coal_twh)), annual_available = !is.na(first(annual_kt)),
      eligible_primary = target_months == 12 & annual_available &
        (first(sector) != "electricity" | ember_months == 12), .groups = "drop")
  write_csv(coverage, file.path(args$output_dir, "source_coverage.csv"))
  write_csv(coverage |> filter(!eligible_primary), file.path(args$output_dir, "excluded_series.csv"))

  keys <- c("iso2", "siec", "sector")
  eligible <- coverage |> filter(eligible_primary) |> group_by(across(all_of(keys))) |>
    filter(year > min(year)) |> ungroup()
  write_csv(eligible |> transmute(iso2, siec, sector, holdout_year = year,
    held_out_months = "January-June",
    retained_sources = if_else(sector == "electricity", "annual balance, Ember, history", "annual balance, history")),
    file.path(args$output_dir, "masked_holdouts.csv"))

  predictions <- bind_rows(lapply(seq_len(nrow(eligible)), function(i) {
    fold <- eligible[i, ]
    x <- data |> filter(iso2 == fold$iso2, siec == fold$siec, sector == fold$sector)
    train <- x |> filter(year < fold$year, !is.na(consumption_kt), !is.na(annual_kt))
    target <- x |> filter(year == fold$year, !is.na(consumption_kt), !is.na(annual_kt)) |> arrange(month)
    if (nrow(target) != 12) return(NULL)
    predict_primary(train, target) |> mutate(iso2 = fold$iso2, siec = fold$siec,
      sector = fold$sector, year = fold$year)
  }))
  annual_predictions <- predictions |> group_by(model, iso2, siec, sector, year) |>
    summarise(annual_predicted_kt = sum(predicted_kt), annual_kt = first(annual_kt),
      annual_constraint_error_kt = abs(annual_predicted_kt - annual_kt),
      source_class = "imputed", .groups = "drop")
  h1_predictions <- predictions |> filter(month <= 6) |> mutate(source_class = "imputed")
  write_csv(h1_predictions, file.path(args$output_dir, "model_predictions.csv"))
  holdout_scores <- summarise_scores(h1_predictions, annual_predictions)
  write_csv(holdout_scores, file.path(args$output_dir, "holdout_scores.csv"))
  score_table <- holdout_scores |> group_by(iso2, siec, sector, model) |>
    summarise(holdouts = n(), h1_mae_kt = mean(h1_abs_error_kt),
      h1_rmse_kt = sqrt(mean(h1_squared_error_kt2)), bias_pct = mean(h1_percent_error),
      monthly_mae_kt = mean(monthly_mae_kt), max_annual_constraint_error_kt = max(annual_constraint_error_kt),
      .groups = "drop") |> group_by(iso2, siec, sector) |>
    mutate(selected = h1_mae_kt == min(h1_mae_kt)) |> ungroup()
  write_csv(score_table, file.path(args$output_dir, "model_scores.csv"))
  chart <- score_table |> group_by(siec, sector, model) |>
    summarise(h1_mae_kt = mean(h1_mae_kt), .groups = "drop") |>
    ggplot(aes(model, h1_mae_kt, fill = sector)) +
    geom_col(position = "dodge") + coord_flip() + facet_wrap(~ siec, scales = "free_y") +
    labs(title = "EU coal-sector H1 imputation error by model",
      subtitle = "Rolling holdouts | lower is better", x = NULL, y = "Mean absolute error (kt)") +
    theme_minimal(base_size = 11)
  ggsave(file.path(args$output_dir, "model_score_chart.png"), chart,
    width = 10, height = 6, dpi = 160, bg = "white")

  accounting <- data |> filter(sector == "non_power", !is.na(consumption_kt),
    !is.na(production_kt), !is.na(imports_kt), !is.na(exports_kt), !is.na(stock_draw_kt)) |>
    mutate(raw_accounting_kt = production_kt + imports_kt - exports_kt + stock_draw_kt)
  accounting_predictions <- bind_rows(lapply(split(accounting, interaction(accounting$iso2, accounting$siec)), function(x) {
    bind_rows(lapply(sort(unique(x$year)), function(year) {
      train <- x |> filter(year < !!year)
      target <- x |> filter(year == !!year, month <= 6)
      if (nrow(train) < 24 || nrow(target) != 6) return(NULL)
      fit <- lm(consumption_kt ~ raw_accounting_kt, data = train)
      bind_rows(transmute(target, iso2, siec, sector, year, month, model = "raw_accounting",
        actual_kt = consumption_kt, predicted_kt = raw_accounting_kt),
        transmute(target, iso2, siec, sector, year, month, model = "calibrated_accounting",
          actual_kt = consumption_kt, predicted_kt = pmax(predict(fit, target), 0)))
    }))
  }))
  write_csv(accounting_predictions, file.path(args$output_dir, "accounting_predictions.csv"))

  reconciliation <- data |> filter(!is.na(consumption_kt), !is.na(annual_kt)) |>
    group_by(iso2, year, siec, sector) |> summarise(month_count = n(), monthly_kt = sum(consumption_kt),
      annual_kt = first(annual_kt), .groups = "drop") |> filter(month_count == 12) |>
    mutate(difference_kt = monthly_kt - annual_kt,
      within_bounds = abs(difference_kt) <= pmax(abs(annual_kt) * args$tolerance, 1e-6),
      source_class = "reported")
  write_csv(reconciliation, file.path(args$output_dir, "annual_reconciliation.csv"))
  if (any(!reconciliation$within_bounds)) warning("Annual reconciliation has out-of-bounds coal series")
  message("Outputs: ", normalizePath(args$output_dir))
}
main()
