#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

VALID_OPTIONS <- c(
  "comparison_dir",
  "raw_crea",
  "external_raw",
  "source_status",
  "target_label",
  "date_to"
)
REQUIRED_OUTPUTS <- c(
  "normalized_external_sources.csv",
  "crea_totals.csv",
  "annual_pairs.csv",
  "monthly_pairs.csv",
  "comparison_totals_eu.csv",
  "comparison_totals_country.csv",
  "source_coverage.csv",
  "source_status.csv",
  "summary.md"
)

usage <- function() {
  cat(paste(
    "Usage:",
    "  compare_get_co2_external.R --comparison-dir <dir> --raw-crea <csv>",
    "    --external-raw <csv> --source-status <csv> --target-label <label>",
    "    --date-to <YYYY-MM-DD>",
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

safe_percent_diff <- function(crea_total, external_total) {
  if_else(!is.na(external_total) & external_total != 0,
    100 * (crea_total - external_total) / external_total,
    NA_real_
  )
}

period_end_date <- function(period, date, year) {
  if_else(
    period == "annual",
    as.Date(paste0(year, "-12-31")),
    ceiling_date(as.Date(date), "month") - days(1)
  )
}

filter_complete_periods <- function(data, date_to) {
  data %>%
    mutate(period_end = period_end_date(period, date, year)) %>%
    filter(period_end <= date_to) %>%
    select(-period_end)
}

read_crea_raw <- function(path) {
  required_cols <- c("iso2", "date", "fuel", "sector", "estimate", "value")
  raw <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("CREA raw output is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  raw %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )
}

read_external_raw <- function(path) {
  required_cols <- c("source_id", "source", "period", "iso2", "date", "year", "value_mt", "unit")
  raw <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "External source output is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  raw %>%
    mutate(
      date = as.Date(date),
      year = as.integer(year),
      value_mt = as.numeric(value_mt)
    )
}

empty_crea_totals <- function() {
  tibble(
    crea_variant = character(),
    period = character(),
    iso2 = character(),
    date = as.Date(character()),
    year = integer(),
    value_mt = numeric(),
    unit = character()
  )
}

target_series_name <- function() {
  "CREA"
}

target_variant_name <- function(crea_variant) {
  recode(
    crea_variant,
    raw = target_series_name(),
    adjusted = paste(target_series_name(), "adjusted"),
    .default = crea_variant
  )
}

short_source_name <- function(source_id, source) {
  case_when(
    source_id == "climate-watch" ~ "Climate Watch",
    source_id == "unfccc" ~ "UNFCCC",
    source_id == "pik" ~ "PIK",
    source_id == "global-carbon-budget-2025" ~ "GCB 2025",
    source_id == "carbon-monitor" ~ "Carbon Monitor",
    source_id == "primap-energy-and-industry" ~ "PRIMAP E+I",
    source_id == "primap-energy-and-industry-excl-mineral-industry" ~ "PRIMAP excl. mineral",
    TRUE ~ source
  )
}

safe_filename <- function(value) {
  value %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")
}

normalise_crea_totals <- function(raw, date_to) {
  central <- raw %>%
    filter(
      estimate == "central",
      iso2 == "EU" | str_detect(iso2, "^[A-Z]{2}$")
    )

  totals <- central %>%
    filter(fuel == "total", sector == "all") %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(iso2, date) %>%
    summarise(raw_t = sum_or_na(value), .groups = "drop")

  aviation <- central %>%
    filter(sector == "transport_international_aviation") %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(iso2, date) %>%
    summarise(aviation_t = sum_or_na(value), .groups = "drop")

  monthly_wide <- totals %>%
    left_join(aviation, by = c("iso2", "date")) %>%
    mutate(
      year = year(date),
      raw = raw_t / 1e6,
      adjusted = (raw_t - coalesce(aviation_t, 0)) / 1e6
    ) %>%
    select(iso2, date, year, raw, adjusted)

  monthly <- monthly_wide %>%
    pivot_longer(c(raw, adjusted),
      names_to = "crea_variant",
      values_to = "value_mt"
    ) %>%
    mutate(period = "monthly", unit = "Mt") %>%
    select(crea_variant, period, iso2, date, year, value_mt, unit)

  annual <- monthly %>%
    group_by(crea_variant, iso2, year) %>%
    summarise(value_mt = sum_or_na(value_mt), .groups = "drop") %>%
    mutate(
      period = "annual",
      date = as.Date(paste0(year, "-01-01")),
      unit = "Mt"
    ) %>%
    select(crea_variant, period, iso2, date, year, value_mt, unit)

  bind_rows(annual, monthly) %>%
    filter_complete_periods(date_to)
}

make_pairs <- function(crea_totals, external) {
  external %>%
    mutate(source_short = short_source_name(source_id, source)) %>%
    select(
      source_id,
      source,
      source_short,
      period,
      iso2,
      date,
      year,
      external_value_mt = value_mt,
      external_unit = unit
    ) %>%
    inner_join(
      crea_totals %>%
        select(
          crea_variant,
          period,
          iso2,
          date,
          year,
          crea_value_mt = value_mt,
          crea_unit = unit
        ),
      by = c("period", "iso2", "date", "year"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      diff_mt = crea_value_mt - external_value_mt,
      pct_diff = safe_percent_diff(crea_value_mt, external_value_mt),
      has_external = !is.na(external_value_mt),
      has_crea = !is.na(crea_value_mt)
    ) %>%
    arrange(period, source_id, iso2, date, crea_variant)
}

total_comparison <- function(pairs, group_cols) {
  pairs %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      external_total_mt = sum_or_na(external_value_mt),
      crea_total_mt = sum_or_na(crea_value_mt),
      diff_mt = crea_total_mt - external_total_mt,
      pct_diff = safe_percent_diff(crea_total_mt, external_total_mt),
      n_external = sum(has_external),
      n_crea = sum(has_crea),
      n_compared = sum(has_external & has_crea),
      min_date = suppressWarnings(min(date[has_external | has_crea], na.rm = TRUE)),
      max_date = suppressWarnings(max(date[has_external | has_crea], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_date = if_else(is.infinite(min_date), as.Date(NA), min_date),
      max_date = if_else(is.infinite(max_date), as.Date(NA), max_date)
    )
}

source_coverage <- function(pairs) {
  pairs %>%
    group_by(period, source_id, source, source_short, crea_variant) %>%
    summarise(
      n_external = sum(has_external),
      n_compared = sum(has_external & has_crea),
      n_iso2_external = n_distinct(iso2[has_external]),
      n_iso2_compared = n_distinct(iso2[has_external & has_crea]),
      min_date = suppressWarnings(min(date[has_external], na.rm = TRUE)),
      max_date = suppressWarnings(max(date[has_external], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_date = if_else(is.infinite(min_date), as.Date(NA), min_date),
      max_date = if_else(is.infinite(max_date), as.Date(NA), max_date)
    )
}

theme_external_compare <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 8)
    )
}

save_blank_plot <- function(path, title, message = "No comparable rows available") {
  plt <- ggplot() +
    annotate("text", x = 0, y = 0, label = message) +
    theme_void() +
    labs(title = title)
  ggsave(path, plot = plt, width = 8, height = 5, dpi = 220, bg = "white")
}

plot_annual_eu_timeseries <- function(annual_pairs, plots_dir) {
  external_lines <- annual_pairs %>%
    filter(iso2 == "EU", has_external, has_crea) %>%
    distinct(source_id, source_short, date, external_value_mt) %>%
    transmute(
      date,
      value_mt = external_value_mt,
      series = source_short,
      group = source_id,
      type = "External"
    )

  crea_lines <- annual_pairs %>%
    filter(iso2 == "EU", has_crea) %>%
    distinct(crea_variant, date, crea_value_mt) %>%
    transmute(
      date,
      value_mt = crea_value_mt,
      series = target_variant_name(crea_variant),
      group = paste("crea", crea_variant),
      type = "Target"
    )

  plot_data <- bind_rows(external_lines, crea_lines) %>%
    filter(!is.na(value_mt))
  path <- file.path(plots_dir, "annual_eu_timeseries.png")

  if (nrow(plot_data) == 0) {
    save_blank_plot(path, "EU annual external comparison")
    return(invisible(path))
  }

  crea_names <- c(target_series_name(), paste(target_series_name(), "adjusted"))
  crea_color <- "#1f77b4"
  crea_linetypes <- setNames(
    c("solid", "dashed"),
    crea_names
  )
  external_names <- setdiff(unique(plot_data$series), crea_names)
  all_linetypes <- c(crea_linetypes, setNames(rep("solid", length(external_names)), external_names))
  crea_colors <- setNames(rep(crea_color, length(crea_names)), crea_names)
  external_colors <- scales::hue_pal()(length(external_names))
  all_colors <- c(crea_colors, setNames(external_colors, external_names))

  plt <- ggplot(plot_data, aes(date, value_mt, color = series, linetype = series)) +
    geom_line(aes(group = group), linewidth = 0.7) +
    scale_y_continuous(labels = label_number()) +
    scale_color_manual(values = all_colors) +
    scale_linetype_manual(values = all_linetypes) +
    labs(
      title = "EU annual CO2 comparison",
      subtitle = "CREA normal/adjusted totals vs validation emissions sources",
      x = NULL,
      y = "Mt CO2 / year",
      color = NULL,
      linetype = NULL
    ) +
    theme_external_compare()

  ggsave(path, plot = plt, width = 11, height = 6.5, dpi = 320, bg = "white")
  invisible(path)
}

plot_annual_eu_timeseries_by_provider <- function(annual_pairs, plots_dir) {
  provider_rows <- annual_pairs %>%
    filter(iso2 == "EU", has_external, has_crea)

  path <- file.path(plots_dir, "annual_eu_timeseries_by_provider.png")
  if (nrow(provider_rows) == 0) {
    save_blank_plot(path, "EU annual provider comparisons")
    return(invisible(path))
  }

  plot_data <- provider_rows %>%
    group_split(source_id, source_short) %>%
    map_dfr(function(provider) {
      external_lines <- provider %>%
        distinct(source_id, source_short, date, external_value_mt) %>%
        transmute(
          source_short,
          date,
          value_mt = external_value_mt,
          series = source_short,
          series_type = "external"
        )
      target_lines <- provider %>%
        distinct(source_short, crea_variant, date, crea_value_mt) %>%
        transmute(
          source_short,
          date,
          value_mt = crea_value_mt,
          series = target_variant_name(crea_variant),
          series_type = "crea"
        )
      bind_rows(external_lines, target_lines)
    }) %>%
    filter(!is.na(value_mt))

  if (nrow(plot_data) == 0) {
    save_blank_plot(path, "EU annual provider comparisons")
    return(invisible(path))
  }

  crea_raw_name <- target_series_name()
  crea_adj_name <- paste(target_series_name(), "adjusted")
  crea_color <- "#1f77b4"
  external_color <- "#d62728"
  all_series <- unique(plot_data$series)
  series_colors <- setNames(
    ifelse(all_series %in% c(crea_raw_name, crea_adj_name), crea_color, external_color),
    all_series
  )
  crea_linetypes <- setNames(c("solid", "dashed"), c(crea_raw_name, crea_adj_name))
  external_series <- setdiff(all_series, c(crea_raw_name, crea_adj_name))
  all_linetypes <- c(crea_linetypes, setNames(rep("solid", length(external_series)), external_series))

  plt <- ggplot(plot_data, aes(date, value_mt, color = series, linetype = series)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~source_short, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = label_number()) +
    scale_color_manual(values = series_colors) +
    scale_linetype_manual(values = all_linetypes) +
    labs(
      title = "EU annual provider comparisons",
      subtitle = "Each panel: CREA (blue solid/dashed) vs external source (red)",
      x = NULL,
      y = "Mt CO2 / year",
      color = NULL,
      linetype = NULL
    ) +
    theme_external_compare()

  ggsave(path, plot = plt, width = 12, height = 8, dpi = 320, bg = "white")
  invisible(path)
}

plot_country_scatter <- function(annual_pairs, plots_dir) {
  plot_data <- annual_pairs %>%
    filter(crea_variant == "adjusted", iso2 != "EU", has_external, has_crea) %>%
    group_by(source_id, source_short, iso2) %>%
    summarise(
      external_total_mt = sum(external_value_mt, na.rm = TRUE),
      crea_total_mt = sum(crea_value_mt, na.rm = TRUE),
      .groups = "drop"
    )

  path <- file.path(plots_dir, "annual_country_adjusted_scatter.png")
  if (nrow(plot_data) == 0) {
    save_blank_plot(path, "Country annual adjusted comparison")
    return(invisible(path))
  }

  plt <- ggplot(plot_data, aes(external_total_mt, crea_total_mt)) +
    geom_abline(slope = 1, intercept = 0, color = "#9ca3af", linewidth = 0.35) +
    geom_point(color = "#2563eb", alpha = 0.55, size = 0.9) +
    facet_wrap(~source_short, scales = "free", ncol = 3) +
    scale_x_continuous(labels = label_number()) +
    scale_y_continuous(labels = label_number()) +
    labs(
      title = "Country annual adjusted comparison",
      subtitle = "One point per country, summed over comparable complete years",
      x = "External source (Mt CO2)",
      y = "Target adjusted (Mt CO2)"
    ) +
    theme_external_compare()

  ggsave(path, plot = plt, width = 11, height = 8, dpi = 320, bg = "white")
  invisible(path)
}

plot_country_diff_ranking <- function(country_totals, plots_dir) {
  ranking <- country_totals %>%
    filter(period == "annual", crea_variant == "adjusted", iso2 != "EU", n_compared > 0) %>%
    group_by(source_id, source_short) %>%
    slice_max(order_by = abs(diff_mt), n = 12, with_ties = FALSE) %>%
    ungroup()

  path <- file.path(plots_dir, "annual_country_adjusted_diff_ranking.png")
  if (nrow(ranking) == 0) {
    save_blank_plot(path, "Largest country annual adjusted differences")
    return(invisible(path))
  }

  plt <- ggplot(ranking, aes(diff_mt, fct_reorder(iso2, diff_mt))) +
    geom_col(fill = "#4b5563", width = 0.72) +
    geom_vline(xintercept = 0, color = "#9ca3af", linewidth = 0.3) +
    facet_wrap(~source_short, scales = "free_y", ncol = 3) +
    scale_x_continuous(labels = label_number()) +
    labs(
      title = "Largest country annual adjusted differences",
      subtitle = "Target adjusted minus external source, summed over comparable complete years",
      x = "Difference (Mt CO2)",
      y = NULL
    ) +
    theme_external_compare() +
    theme(axis.text.y = element_text(size = 7))

  n_provider <- n_distinct(ranking$source_id)
  height <- max(6, ceiling(n_provider / 3) * 4)
  ggsave(path, plot = plt, width = 12, height = height, dpi = 320, bg = "white")
  invisible(path)
}

plot_monthly_carbonmonitor_eu <- function(monthly_pairs, plots_dir) {
  plot_data <- monthly_pairs %>%
    filter(
      source_id == "carbon-monitor",
      crea_variant == "adjusted",
      iso2 == "EU"
    ) %>%
    select(date, external_value_mt, crea_value_mt) %>%
    pivot_longer(
      c(external_value_mt, crea_value_mt),
      names_to = "series",
      values_to = "value_mt"
    ) %>%
    mutate(series = recode(
      series,
      external_value_mt = "Carbon Monitor",
      crea_value_mt = paste(target_series_name(), "adjusted")
    )) %>%
    filter(!is.na(value_mt))

  path <- file.path(plots_dir, "monthly_carbonmonitor_eu_timeseries.png")
  if (nrow(plot_data) == 0) {
    save_blank_plot(path, "EU monthly Carbon Monitor comparison")
    return(invisible(path))
  }

  plt <- ggplot(plot_data, aes(date, value_mt, color = series)) +
    geom_line(linewidth = 0.65) +
    scale_y_continuous(labels = label_number()) +
    labs(
      title = "EU monthly Carbon Monitor comparison",
      subtitle = "Target adjusted total vs Carbon Monitor",
      x = NULL,
      y = "Mt CO2 / month",
      color = NULL
    ) +
    theme_external_compare()

  ggsave(path, plot = plt, width = 10, height = 5.5, dpi = 320, bg = "white")
  invisible(path)
}

plot_monthly_carbonmonitor_countries <- function(monthly_pairs, plots_dir) {
  plot_data <- monthly_pairs %>%
    filter(
      source_id == "carbon-monitor",
      crea_variant == "adjusted",
      iso2 != "EU"
    ) %>%
    select(iso2, date, external_value_mt, crea_value_mt) %>%
    pivot_longer(
      c(external_value_mt, crea_value_mt),
      names_to = "series",
      values_to = "value_mt"
    ) %>%
    mutate(series = recode(
      series,
      external_value_mt = "Carbon Monitor",
      crea_value_mt = paste(target_series_name(), "adjusted")
    )) %>%
    filter(!is.na(value_mt))

  path <- file.path(plots_dir, "monthly_carbonmonitor_country_timeseries.png")
  if (nrow(plot_data) == 0) {
    save_blank_plot(path, "Country monthly Carbon Monitor comparison")
    return(invisible(path))
  }

  n_country <- n_distinct(plot_data$iso2)
  ncol <- min(4, max(1, n_country))
  nrow <- ceiling(n_country / ncol)

  plt <- ggplot(plot_data, aes(date, value_mt, color = series)) +
    geom_line(linewidth = 0.25, alpha = 0.9) +
    facet_wrap(~iso2, scales = "free_y", ncol = ncol) +
    scale_y_continuous(labels = label_number()) +
    labs(
      title = "Country monthly Carbon Monitor comparison",
      subtitle = "Target adjusted total vs Carbon Monitor",
      x = NULL,
      y = "Mt CO2 / month",
      color = NULL
    ) +
    theme_external_compare() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6)
    )

  width <- max(8, ncol * 2.7)
  height <- max(6, nrow * 2.25)
  ggsave(path, plot = plt, width = width, height = height, dpi = 320, bg = "white")
  invisible(path)
}

plot_annual_country_timeseries_per_provider <- function(annual_pairs, subplots_dir) {
  providers <- annual_pairs %>%
    filter(iso2 != "EU", has_external, has_crea) %>%
    distinct(source_id, source_short)

  if (nrow(providers) == 0) {
    return(invisible(character()))
  }

  crea_raw_name <- target_series_name()
  crea_adj_name <- paste(target_series_name(), "adjusted")
  crea_color <- "#1f77b4"
  external_color <- "#d62728"

  map2_chr(providers$source_id, providers$source_short, function(provider_id, provider_name) {
    provider_pairs <- annual_pairs %>%
      filter(source_id == provider_id, iso2 != "EU", has_external, has_crea)
    external_lines <- provider_pairs %>%
      distinct(iso2, date, external_value_mt) %>%
      transmute(iso2, date, value_mt = external_value_mt, series = provider_name)
    target_lines <- provider_pairs %>%
      distinct(iso2, crea_variant, date, crea_value_mt) %>%
      transmute(
        iso2,
        date,
        value_mt = crea_value_mt,
        series = target_variant_name(crea_variant)
      )
    plot_data <- bind_rows(external_lines, target_lines) %>%
      filter(!is.na(value_mt))

    path <- file.path(
      subplots_dir,
      paste0("annual_country_timeseries_", safe_filename(provider_id), ".png")
    )
    if (nrow(plot_data) == 0) {
      save_blank_plot(path, paste("Country annual comparison:", provider_name))
      return(path)
    }

    all_series <- unique(plot_data$series)
    series_colors <- setNames(
      ifelse(all_series %in% c(crea_raw_name, crea_adj_name), crea_color, external_color),
      all_series
    )
    crea_linetypes <- setNames(c("solid", "dashed"), c(crea_raw_name, crea_adj_name))
    external_series <- setdiff(all_series, c(crea_raw_name, crea_adj_name))
    all_linetypes <- c(
      crea_linetypes,
      setNames(rep("solid", length(external_series)), external_series)
    )

    n_country <- n_distinct(plot_data$iso2)
    ncol <- min(4, max(1, n_country))
    nrow <- ceiling(n_country / ncol)

    plt <- ggplot(plot_data, aes(date, value_mt, color = series, linetype = series)) +
      geom_line(linewidth = 0.45, alpha = 0.9) +
      facet_wrap(~iso2, scales = "free_y", ncol = ncol) +
      scale_y_continuous(labels = label_number()) +
      scale_color_manual(values = series_colors) +
      scale_linetype_manual(values = all_linetypes) +
      labs(
        title = paste("Country annual comparison:", provider_name),
        subtitle = "CREA (blue solid/dashed) vs provider (red)",
        x = NULL,
        y = "Mt CO2 / year",
        color = NULL,
        linetype = NULL
      ) +
      theme_external_compare() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6)
      )

    width <- max(8, ncol * 2.7)
    height <- max(6, nrow * 2.1)
    ggsave(path, plot = plt, width = width, height = height, dpi = 320, bg = "white")
    path
  })
}

write_plots <- function(annual_pairs, monthly_pairs, country_totals, comparison_dir) {
  plots_dir <- file.path(comparison_dir, "plots")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  country_by_provider_dir <- file.path(plots_dir, "country_by_provider")
  dir.create(country_by_provider_dir, recursive = TRUE, showWarnings = FALSE)

  plot_annual_eu_timeseries(annual_pairs, plots_dir)
  plot_annual_eu_timeseries_by_provider(annual_pairs, plots_dir)
  plot_country_scatter(annual_pairs, plots_dir)
  plot_country_diff_ranking(country_totals, plots_dir)
  plot_annual_country_timeseries_per_provider(annual_pairs, country_by_provider_dir)
  plot_monthly_carbonmonitor_eu(monthly_pairs, plots_dir)
  plot_monthly_carbonmonitor_countries(monthly_pairs, plots_dir)
}

fmt_number <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", format(round(x, digits), big.mark = ",", nsmall = digits))
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "NA", paste0(fmt_number(x, 2), "%"))
}

md_table <- function(headers, rows) {
  if (nrow(rows) == 0) {
    return("_No rows available._")
  }
  body <- apply(rows, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  paste(
    c(
      paste0("| ", paste(headers, collapse = " | "), " |"),
      paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |"),
      body
    ),
    collapse = "\n"
  )
}

write_summary <- function(comparison_dir, target_label, date_to, eu_totals, country_totals,
                          coverage, source_status) {
  eu_headline <- eu_totals %>%
    filter(period == "annual", crea_variant == "adjusted") %>%
    arrange(desc(abs(diff_mt))) %>%
    mutate(
      `Source` = source_short,
      `Target adjusted (Mt)` = fmt_number(crea_total_mt),
      `External (Mt)` = fmt_number(external_total_mt),
      `Diff (Mt)` = fmt_number(diff_mt),
      `% diff` = fmt_pct(pct_diff),
      `Rows` = as.character(n_compared)
    ) %>%
    select(`Source`, `Target adjusted (Mt)`, `External (Mt)`, `Diff (Mt)`, `% diff`, `Rows`) %>%
    slice_head(n = 8)

  country_headline <- country_totals %>%
    filter(period == "annual", crea_variant == "adjusted", iso2 != "EU", n_compared > 0) %>%
    arrange(desc(abs(diff_mt))) %>%
    mutate(
      `Source / country` = paste(source_short, iso2, sep = " / "),
      `Diff (Mt)` = fmt_number(diff_mt),
      `% diff` = fmt_pct(pct_diff),
      `Rows` = as.character(n_compared)
    ) %>%
    select(`Source / country`, `Diff (Mt)`, `% diff`, `Rows`) %>%
    slice_head(n = 10)

  status_rows <- source_status %>%
    mutate(
      `Source` = short_source_name(source_id, source),
      `Period` = period,
      `Status` = status,
      `Rows` = as.character(rows),
      `Message` = message
    ) %>%
    select(`Source`, `Period`, `Status`, `Rows`, `Message`)

  coverage_rows <- coverage %>%
    filter(crea_variant == "adjusted") %>%
    mutate(
      `Source` = source_short,
      `Period` = period,
      `Compared rows` = as.character(n_compared),
      `Regions` = as.character(n_iso2_compared),
      `Range` = paste(min_date, max_date, sep = " to ")
    ) %>%
    select(`Source`, `Period`, `Compared rows`, `Regions`, `Range`)

  lines <- c(
    "# External CO2 Source Comparison Summary",
    "",
    paste0("**Target:** `", target_label, "`"),
    paste0("**date_to:** `", date_to, "`"),
    "",
    "## EU Annual Adjusted Totals",
    md_table(names(eu_headline), eu_headline),
    "",
    "## Largest Country Differences",
    md_table(names(country_headline), country_headline),
    "",
    "## Coverage",
    md_table(names(coverage_rows), coverage_rows),
    "",
    "## Source Status",
    md_table(names(status_rows), status_rows),
    "",
    "## Key Plots",
    "![annual_eu_timeseries.png](plots/annual_eu_timeseries.png)",
    "![annual_eu_timeseries_by_provider.png](plots/annual_eu_timeseries_by_provider.png)",
    "![annual_country_adjusted_scatter.png](plots/annual_country_adjusted_scatter.png)",
    "![annual_country_adjusted_diff_ranking.png](plots/annual_country_adjusted_diff_ranking.png)",
    "![monthly_carbonmonitor_eu_timeseries.png](plots/monthly_carbonmonitor_eu_timeseries.png)"
  )

  writeLines(lines, file.path(comparison_dir, "summary.md"))
}

validate_outputs <- function(comparison_dir) {
  missing_outputs <- REQUIRED_OUTPUTS[
    !file.exists(file.path(comparison_dir, REQUIRED_OUTPUTS)) |
      file.info(file.path(comparison_dir, REQUIRED_OUTPUTS))$size <= 0
  ]
  if (length(missing_outputs) > 0) {
    stop(
      "External comparison is missing required outputs: ",
      paste(missing_outputs, collapse = ", ")
    )
  }
  if (!dir.exists(file.path(comparison_dir, "plots"))) {
    stop("External comparison is missing plots directory.")
  }
}

run_compare <- function(opts) {
  comparison_dir <- normalizePath(require_option(opts, "comparison_dir"), mustWork = TRUE)
  raw_crea <- normalizePath(require_option(opts, "raw_crea"), mustWork = TRUE)
  external_raw <- normalizePath(require_option(opts, "external_raw"), mustWork = TRUE)
  source_status_path <- normalizePath(require_option(opts, "source_status"), mustWork = TRUE)
  target_label <- require_option(opts, "target_label")
  date_to <- as.Date(require_option(opts, "date_to"))
  if (is.na(date_to)) {
    stop("Invalid --date-to value.")
  }

  external <- read_external_raw(external_raw) %>%
    filter_complete_periods(date_to)
  source_status <- read_csv(source_status_path, show_col_types = FALSE)
  crea_totals <- read_crea_raw(raw_crea) %>%
    normalise_crea_totals(date_to)
  target_coverage <- crea_totals %>%
    distinct(period, iso2, date, year)
  external <- external %>%
    semi_join(target_coverage, by = c("period", "iso2", "date", "year"))

  pairs <- make_pairs(crea_totals, external)
  annual_pairs <- pairs %>% filter(period == "annual")
  monthly_pairs <- pairs %>% filter(period == "monthly")
  eu_totals <- total_comparison(
    pairs %>% filter(iso2 == "EU"),
    c("period", "source_id", "source", "source_short", "crea_variant")
  )
  country_totals <- total_comparison(
    pairs %>% filter(iso2 != "EU"),
    c("period", "source_id", "source", "source_short", "crea_variant", "iso2")
  )
  coverage <- source_coverage(pairs)

  write_csv(external, file.path(comparison_dir, "normalized_external_sources.csv"), na = "")
  write_csv(crea_totals, file.path(comparison_dir, "crea_totals.csv"), na = "")
  write_csv(annual_pairs, file.path(comparison_dir, "annual_pairs.csv"), na = "")
  write_csv(monthly_pairs, file.path(comparison_dir, "monthly_pairs.csv"), na = "")
  write_csv(eu_totals, file.path(comparison_dir, "comparison_totals_eu.csv"), na = "")
  write_csv(country_totals, file.path(comparison_dir, "comparison_totals_country.csv"), na = "")
  write_csv(coverage, file.path(comparison_dir, "source_coverage.csv"), na = "")
  write_csv(source_status, file.path(comparison_dir, "source_status.csv"), na = "")

  write_plots(annual_pairs, monthly_pairs, country_totals, comparison_dir)
  write_summary(
    comparison_dir,
    target_label,
    date_to,
    eu_totals,
    country_totals,
    coverage,
    source_status
  )
  validate_outputs(comparison_dir)

  message("[compare_get_co2_external.R] Wrote external comparison report to ", comparison_dir)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    usage()
    quit(status = 1)
  }

  run_compare(parse_options(args))
}
