#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

DEFAULT_DIFF_TOLERANCE <- 1e-6
VALID_OPTIONS <- c(
  "comparison_dir",
  "raw_base",
  "raw_target",
  "base_short_sha",
  "target_short_sha"
)
REQUIRED_OUTPUTS <- c(
  "comparison_totals_eu.csv",
  "comparison_totals_eu_component.csv",
  "comparison_totals_country.csv",
  "comparison_totals_component.csv",
  "monthly_eu_component_metrics.csv",
  "monthly_country_metrics.csv",
  "monthly_component_metrics.csv"
)

usage <- function() {
  cat(paste(
    "Usage:",
    "  compare_get_co2.R --comparison-dir <dir> --raw-base <csv> --raw-target <csv>",
    "    --base-short-sha <sha12> --target-short-sha <sha12>",
    sep = "\n"
  ), "\n")
}

stop_usage <- function(...) {
  message("Error: ", paste(..., collapse = ""))
  usage()
  quit(status = 1)
}

parse_options <- function(args) {
  opts <- list()
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      stop_usage("unexpected argument: ", arg)
    }
    if (i == length(args)) {
      stop_usage("missing value for option: ", arg)
    }

    name <- gsub("-", "_", sub("^--", "", arg))
    if (!name %in% VALID_OPTIONS) {
      stop_usage("unknown option: ", arg)
    }
    opts[[name]] <- args[[i + 1]]
    i <- i + 2
  }

  opts
}

require_option <- function(opts, name) {
  value <- opts[[name]]
  if (is.null(value) || !nzchar(value)) {
    stop_usage("missing required option --", gsub("_", "-", name))
  }
  value
}

sum_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}

metric_r2 <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 2 || sd(x) == 0 || sd(y) == 0) {
    NA_real_
  } else {
    cor(x, y)^2
  }
}

metric_rmse <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) {
    NA_real_
  } else {
    sqrt(mean((y[ok] - x[ok])^2))
  }
}

metric_summary <- function(data) {
  data %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    summarise(
      n_compared = n(),
      rmse = metric_rmse(base_value, target_value),
      r2 = metric_r2(base_value, target_value),
      .groups = "drop"
    )
}

metric_subtitle <- function(data) {
  metrics <- metric_summary(data)
  if (nrow(metrics) == 0 || is.na(metrics$n_compared) || metrics$n_compared == 0) {
    return("No complete base/target pairs")
  }

  rmse_mt <- metrics$rmse / 1e6
  paste0(
    "RMSE: ", number(rmse_mt, accuracy = 0.01), " Mt CO2; ",
    "R2: ", ifelse(is.na(metrics$r2), "NA", number(metrics$r2, accuracy = 0.001)), "; ",
    "n: ", comma(metrics$n_compared)
  )
}

safe_percent_diff <- function(target_total, base_total) {
  if_else(!is.na(base_total) & base_total != 0, 100 * (target_total - base_total) / base_total, NA_real_)
}

expand_plot_range <- function(x) {
  range_x <- range(x, finite = TRUE)
  if (length(range_x) != 2 || !all(is.finite(range_x))) {
    return(c(0, 1))
  }

  if (range_x[[1]] == range_x[[2]]) {
    pad <- max(abs(range_x[[1]]) * 0.05, 1)
  } else {
    pad <- diff(range_x) * 0.04
  }

  range_x + c(-pad, pad)
}

theme_compare <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 8)
    )
}

read_raw <- function(path) {
  if (!file.exists(path)) {
    stop("Missing raw output: ", path)
  }

  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "value")
  raw <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Raw output is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  raw %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )
}

normalise_for_compare <- function(raw) {
  raw %>%
    filter(estimate == "central") %>%
    mutate(date = floor_date(as.Date(date), "month")) %>%
    group_by(iso2, date, fuel, sector) %>%
    summarise(value = sum_or_na(value), .groups = "drop")
}

make_pairs <- function(base, target, keys) {
  base %>%
    select(all_of(keys), base_value = value) %>%
    full_join(
      target %>% select(all_of(keys), target_value = value),
      by = keys
    )
}

total_comparison <- function(pairs, group_cols = character()) {
  grouped <- if (length(group_cols) > 0) {
    pairs %>% group_by(across(all_of(group_cols)))
  } else {
    pairs
  }

  grouped %>%
    summarise(
      base_total = sum_or_na(base_value),
      target_total = sum_or_na(target_value),
      diff = target_total - base_total,
      pct_diff = safe_percent_diff(target_total, base_total),
      n_base = sum(!is.na(base_value)),
      n_target = sum(!is.na(target_value)),
      n_compared = sum(!is.na(base_value) & !is.na(target_value)),
      n_months = n_distinct(date[!is.na(base_value) | !is.na(target_value)]),
      .groups = "drop"
    )
}

monthly_metrics <- function(pairs) {
  pairs %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    group_by(date) %>%
    summarise(
      n_compared = n(),
      rmse = metric_rmse(base_value, target_value),
      r2 = metric_r2(base_value, target_value),
      .groups = "drop"
    )
}

zero_small_pair_diffs <- function(data, diff_tolerance = DEFAULT_DIFF_TOLERANCE) {
  data %>%
    mutate(
      is_small_diff = is.finite(base_value) &
        is.finite(target_value) &
        abs(target_value - base_value) <= diff_tolerance,
      target_value = if_else(is_small_diff, base_value, target_value)
    ) %>%
    select(-is_small_diff)
}

zero_small_total_diffs <- function(data, diff_tolerance = DEFAULT_DIFF_TOLERANCE) {
  data %>%
    mutate(
      is_small_diff = is.finite(diff) & abs(diff) <= diff_tolerance,
      diff = if_else(is_small_diff, 0, diff),
      pct_diff = if_else(is_small_diff & !is.na(pct_diff), 0, pct_diff)
    ) %>%
    select(-is_small_diff)
}

write_comparison_tables <- function(comparison_dir, base_data, target_data) {
  eu_pairs <- make_pairs(
    base_data %>% filter(iso2 == "EU", fuel == "total", sector == "all"),
    target_data %>% filter(iso2 == "EU", fuel == "total", sector == "all"),
    c("date")
  ) %>%
    zero_small_pair_diffs()

  eu_component_pairs <- make_pairs(
    base_data %>% filter(iso2 == "EU", fuel != "total"),
    target_data %>% filter(iso2 == "EU", fuel != "total"),
    c("date", "fuel", "sector")
  ) %>%
    zero_small_pair_diffs()

  country_pairs <- make_pairs(
    base_data %>% filter(iso2 != "EU", fuel == "total", sector == "all"),
    target_data %>% filter(iso2 != "EU", fuel == "total", sector == "all"),
    c("iso2", "date")
  ) %>%
    zero_small_pair_diffs()

  component_pairs <- make_pairs(
    base_data %>% filter(iso2 != "EU", fuel != "total"),
    target_data %>% filter(iso2 != "EU", fuel != "total"),
    c("iso2", "date", "fuel", "sector")
  ) %>%
    zero_small_pair_diffs()

  comparison_totals_eu <- total_comparison(eu_pairs) %>%
    zero_small_total_diffs() %>%
    mutate(iso2 = "EU", .before = 1)
  comparison_totals_eu_component <- total_comparison(eu_component_pairs, c("fuel", "sector")) %>%
    zero_small_total_diffs() %>%
    mutate(iso2 = "EU", .before = 1) %>%
    arrange(desc(abs(diff)))
  comparison_totals_country <- total_comparison(country_pairs, "iso2") %>%
    zero_small_total_diffs() %>%
    arrange(desc(abs(diff)))
  comparison_totals_component <- total_comparison(component_pairs, c("iso2", "fuel", "sector")) %>%
    zero_small_total_diffs() %>%
    arrange(desc(abs(diff)))

  monthly_eu_component_metrics <- monthly_metrics(eu_component_pairs)
  monthly_country_metrics <- monthly_metrics(country_pairs)
  monthly_component_metrics <- monthly_metrics(component_pairs)

  write_csv(comparison_totals_eu, file.path(comparison_dir, "comparison_totals_eu.csv"), na = "")
  write_csv(
    comparison_totals_eu_component,
    file.path(comparison_dir, "comparison_totals_eu_component.csv"),
    na = ""
  )
  write_csv(comparison_totals_country, file.path(comparison_dir, "comparison_totals_country.csv"), na = "")
  write_csv(comparison_totals_component, file.path(comparison_dir, "comparison_totals_component.csv"), na = "")
  write_csv(
    monthly_eu_component_metrics,
    file.path(comparison_dir, "monthly_eu_component_metrics.csv"),
    na = ""
  )
  write_csv(monthly_country_metrics, file.path(comparison_dir, "monthly_country_metrics.csv"), na = "")
  write_csv(monthly_component_metrics, file.path(comparison_dir, "monthly_component_metrics.csv"), na = "")

  list(
    eu_pairs = eu_pairs,
    eu_component_pairs = eu_component_pairs,
    country_pairs = country_pairs,
    component_pairs = component_pairs
  )
}

plot_eu_timeseries <- function(eu_pairs, plots_dir, base_label, target_label) {
  plot_data <- eu_pairs %>%
    pivot_longer(
      cols = c(base_value, target_value),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = recode(series, base_value = base_label, target_value = target_label),
      value_mt = value / 1e6
    ) %>%
    filter(!is.na(value_mt))

  plt <- ggplot(plot_data, aes(date, value_mt, color = series)) +
    geom_line(linewidth = 0.7) +
    scale_y_continuous(labels = label_number()) +
    scale_color_manual(values = c("#1f77b4", "#d62728")) +
    labs(
      title = "EU monthly get_co2 comparison",
      subtitle = "Total CO2, fuel == 'total' and sector == 'all'",
      x = NULL,
      y = "Mt CO2 / month",
      color = NULL
    ) +
    theme_compare()

  ggsave(
    file.path(plots_dir, "eu_timeseries.png"),
    plot = plt,
    width = 10,
    height = 5.5,
    dpi = 320,
    bg = "white"
  )
}

plot_country_timeseries <- function(country_pairs, plots_dir, base_label, target_label) {
  plot_data <- country_pairs %>%
    pivot_longer(
      cols = c(base_value, target_value),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(
      series = recode(series, base_value = base_label, target_value = target_label),
      value_mt = value / 1e6
    ) %>%
    filter(!is.na(value_mt))

  n_country <- n_distinct(plot_data$iso2)
  ncol <- min(4, max(1, n_country))
  nrow <- ceiling(n_country / ncol)
  width <- max(8, ncol * 2.6)
  height <- max(6, nrow * 2.35)

  plt <- ggplot(plot_data, aes(date, value_mt, color = series)) +
    geom_line(linewidth = 0.28, alpha = 0.9) +
    facet_wrap(~iso2, scales = "free_y", ncol = ncol) +
    scale_y_continuous(labels = label_number()) +
    scale_color_manual(values = c("#1f77b4", "#d62728")) +
    labs(
      title = "Country monthly get_co2 comparison",
      subtitle = "Total CO2 by EU country, fuel == 'total' and sector == 'all'",
      x = NULL,
      y = "Mt CO2 / month",
      color = NULL
    ) +
    theme_compare() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6)
    )

  ggsave(
    file.path(plots_dir, "country_timeseries_small_multiples.png"),
    plot = plt,
    width = width,
    height = height,
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )
}

plot_revision_timeseries <- function(pairs, plots_dir, filename, title, subtitle, facet_var = NULL) {
  plot_data <- pairs %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    mutate(diff_mt = (target_value - base_value) / 1e6)

  if (nrow(plot_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No complete base/target pairs") +
      theme_void() +
      labs(title = title)
    width <- 7.5
    height <- 5
  } else if (is.null(facet_var)) {
    plt <- ggplot(plot_data, aes(date, diff_mt)) +
      geom_hline(yintercept = 0, color = "#9ca3af", linewidth = 0.35) +
      geom_line(color = "#374151", linewidth = 0.7) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = title,
        subtitle = subtitle,
        x = NULL,
        y = "Target - base (Mt CO2 / month)"
      ) +
      theme_compare()
    width <- 10
    height <- 5.5
  } else {
    n_facet <- n_distinct(plot_data[[facet_var]])
    ncol <- min(4, max(1, n_facet))
    nrow <- ceiling(n_facet / ncol)
    width <- max(8, ncol * 2.6)
    height <- max(6, nrow * 2.35)

    plt <- ggplot(plot_data, aes(date, diff_mt)) +
      geom_hline(yintercept = 0, color = "#9ca3af", linewidth = 0.25) +
      geom_line(color = "#374151", linewidth = 0.28, alpha = 0.9) +
      facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y", ncol = ncol) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = title,
        subtitle = subtitle,
        x = NULL,
        y = "Target - base (Mt CO2 / month)"
      ) +
      theme_compare() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6)
      )
  }

  ggsave(
    file.path(plots_dir, filename),
    plot = plt,
    width = width,
    height = height,
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )
}

plot_rmse_ranking <- function(
  data, plots_dir, filename, title, subtitle, group_cols,
  label_cols = group_cols, label_sep = " / "
) {
  ranking <- data %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_compared = n(),
      rmse = metric_rmse(base_value, target_value),
      .groups = "drop"
    ) %>%
    filter(!is.na(rmse)) %>%
    mutate(
      rmse_mt = rmse / 1e6,
      label = do.call(paste, c(across(all_of(label_cols)), sep = label_sep))
    ) %>%
    arrange(rmse_mt)

  if (nrow(ranking) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No complete base/target pairs") +
      theme_void() +
      labs(title = title)
    height <- 5
  } else {
    height <- max(5, min(18, 1.2 + nrow(ranking) * 0.28))
    plt <- ggplot(ranking, aes(rmse_mt, fct_reorder(label, rmse_mt))) +
      geom_col(fill = "#4b5563", width = 0.72) +
      scale_x_continuous(labels = label_number()) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "RMSE (Mt CO2 / month)",
        y = NULL
      ) +
      theme_compare() +
      theme(
        axis.text.y = element_text(size = 7),
        legend.position = "none"
      )
  }

  ggsave(
    file.path(plots_dir, filename),
    plot = plt,
    width = 8,
    height = height,
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )
}

plot_scatter <- function(data, plots_dir, filename, title, subtitle_context) {
  plot_data <- data %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    mutate(
      base_mt = base_value / 1e6,
      target_mt = target_value / 1e6
    )

  if (nrow(plot_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No complete base/target pairs") +
      theme_void() +
      labs(title = title)
  } else {
    axis_range <- expand_plot_range(c(plot_data$base_mt, plot_data$target_mt))
    plt <- ggplot(plot_data, aes(base_mt, target_mt)) +
      geom_abline(slope = 1, intercept = 0, color = "#9ca3af", linewidth = 0.45) +
      geom_point(color = "#2563eb", alpha = 0.32, size = 0.75, shape = 16, stroke = 0) +
      coord_equal(xlim = axis_range, ylim = axis_range) +
      scale_x_continuous(labels = label_number()) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = title,
        subtitle = paste(subtitle_context, metric_subtitle(plot_data), sep = "\n"),
        x = "Base (Mt CO2 / month)",
        y = "Target (Mt CO2 / month)"
      ) +
      theme_compare()
  }

  ggsave(
    file.path(plots_dir, filename),
    plot = plt,
    width = 7.5,
    height = 7,
    dpi = 320,
    bg = "white"
  )
}

plot_scatter_facets <- function(data, plots_dir, filename, title, subtitle_context, facet_var) {
  plot_data <- data %>%
    filter(!is.na(base_value), !is.na(target_value)) %>%
    mutate(
      base_mt = base_value / 1e6,
      target_mt = target_value / 1e6
    )

  if (nrow(plot_data) == 0) {
    plt <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No complete base/target pairs") +
      theme_void() +
      labs(title = title)
    width <- 7.5
    height <- 5
  } else {
    n_facet <- n_distinct(plot_data[[facet_var]])
    ncol <- min(4, max(1, n_facet))
    nrow <- ceiling(n_facet / ncol)
    width <- max(8, ncol * 2.6)
    height <- max(6, nrow * 2.35)

    plt <- ggplot(plot_data, aes(base_mt, target_mt)) +
      geom_abline(slope = 1, intercept = 0, color = "#9ca3af", linewidth = 0.25) +
      geom_point(color = "#2563eb", alpha = 0.28, size = 0.55, shape = 16, stroke = 0) +
      facet_wrap(as.formula(paste("~", facet_var)), scales = "free", ncol = ncol) +
      scale_x_continuous(labels = label_number()) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = title,
        subtitle = paste(subtitle_context, metric_subtitle(plot_data), sep = "\n"),
        x = "Base (Mt CO2 / month)",
        y = "Target (Mt CO2 / month)"
      ) +
      theme_compare() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6)
      )
  }

  ggsave(
    file.path(plots_dir, filename),
    plot = plt,
    width = width,
    height = height,
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )
}

write_plots <- function(pairs, comparison_dir, base_short_sha, target_short_sha) {
  plots_dir <- file.path(comparison_dir, "plots")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  base_label <- paste0("Base ", base_short_sha)
  target_label <- paste0("Target ", target_short_sha)

  eu_component_pairs_for_plots <- pairs$eu_component_pairs %>%
    mutate(fuel_sector = paste(fuel, sector, sep = " / "))
  component_pairs_for_plots <- pairs$component_pairs %>%
    mutate(fuel_sector = paste(fuel, sector, sep = " / "))

  plot_eu_timeseries(pairs$eu_pairs, plots_dir, base_label, target_label)
  plot_country_timeseries(pairs$country_pairs, plots_dir, base_label, target_label)
  plot_revision_timeseries(
    pairs$eu_pairs,
    plots_dir,
    "eu_revision_timeseries.png",
    "EU monthly get_co2 revision",
    "Target minus base, fuel == 'total' and sector == 'all'"
  )
  plot_revision_timeseries(
    pairs$country_pairs,
    plots_dir,
    "country_revision_timeseries_small_multiples.png",
    "Country monthly get_co2 revision",
    "Target minus base by EU country, fuel == 'total' and sector == 'all'",
    facet_var = "iso2"
  )
  plot_rmse_ranking(
    eu_component_pairs_for_plots,
    plots_dir,
    "eu_fuel_sector_rmse_ranking.png",
    "EU fuel-sectors ranked by monthly RMSE",
    "Computed across EU component-month observations for each fuel-sector",
    group_cols = c("fuel", "sector")
  )
  plot_rmse_ranking(
    pairs$country_pairs,
    plots_dir,
    "country_rmse_ranking.png",
    "Countries ranked by monthly RMSE",
    "Computed across each country time series, fuel == 'total' and sector == 'all'",
    group_cols = "iso2"
  )
  plot_rmse_ranking(
    component_pairs_for_plots,
    plots_dir,
    "fuel_sector_rmse_ranking.png",
    "Fuel-sectors ranked by monthly RMSE",
    "Computed across all country-month observations for each fuel-sector",
    group_cols = c("fuel", "sector")
  )
  plot_scatter(
    pairs$country_pairs,
    plots_dir,
    "country_month_scatter.png",
    "Country-month get_co2 comparison",
    "One point per country-month; no country-specific styling"
  )
  plot_scatter(
    component_pairs_for_plots,
    plots_dir,
    "component_month_scatter.png",
    "Component-month get_co2 comparison",
    "One point per country-month-fuel-sector; no component-specific styling"
  )
  plot_scatter_facets(
    pairs$country_pairs,
    plots_dir,
    "country_month_scatter_small_multiples.png",
    "Country-month get_co2 comparison by country",
    "One panel per EU country",
    facet_var = "iso2"
  )
  plot_scatter_facets(
    component_pairs_for_plots,
    plots_dir,
    "fuel_sector_month_scatter_small_multiples.png",
    "Component-month get_co2 comparison by fuel-sector",
    "One panel per fuel-sector; points are country-month observations",
    facet_var = "fuel_sector"
  )
}

validate_comparison_outputs <- function(comparison_dir) {
  missing_outputs <- REQUIRED_OUTPUTS[
    !file.exists(file.path(comparison_dir, REQUIRED_OUTPUTS)) |
      file.info(file.path(comparison_dir, REQUIRED_OUTPUTS))$size <= 0
  ]
  if (length(missing_outputs) > 0) {
    stop(
      "Comparison report is missing required outputs: ",
      paste(missing_outputs, collapse = ", ")
    )
  }

  plots_dir <- file.path(comparison_dir, "plots")
  if (!dir.exists(plots_dir)) {
    stop("Comparison report is missing plots directory: ", plots_dir)
  }
}

run_compare <- function(opts) {
  comparison_dir <- normalizePath(require_option(opts, "comparison_dir"), mustWork = TRUE)
  raw_base <- normalizePath(require_option(opts, "raw_base"), mustWork = TRUE)
  raw_target <- normalizePath(require_option(opts, "raw_target"), mustWork = TRUE)
  base_short_sha <- require_option(opts, "base_short_sha")
  target_short_sha <- require_option(opts, "target_short_sha")

  base_data <- read_raw(raw_base) %>% normalise_for_compare()
  target_data <- read_raw(raw_target) %>% normalise_for_compare()

  pairs <- write_comparison_tables(comparison_dir, base_data, target_data)
  write_plots(pairs, comparison_dir, base_short_sha, target_short_sha)
  validate_comparison_outputs(comparison_dir)

  message("[compare_get_co2.R] Wrote comparison report to ", comparison_dir)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  usage()
  quit(status = 1)
}

run_compare(parse_options(args))
