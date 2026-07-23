#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

TRUST_DATE_TO <- as.Date("2026-01-31")
TRUST_VALIDATION_YEARS <- 2020:2024
TRUST_COMPARISON_MIN_YEAR <- 1990L
TRUST_COMPARISON_MAX_YEAR <- 2024L
TRUST_OUTPUT_DIR <- file.path(
  "diagnostics",
  "co2_trust_analysis_jan_2026_vintage"
)
TRUST_CACHE_DIR <- file.path(
  "cache",
  "co2_trust_analysis_jan_2026_v2"
)
TRUST_SETTLING_THRESHOLDS_PCT <- c(0.5, 1, 2)
TRUST_TREND_THRESHOLDS_PP <- c(0.5, 1, 2)
TRUST_PLOT_WIDTH <- 12.5
TRUST_PLOT_HEIGHT <- 5.8
TRUST_PLOT_DPI <- 320


trust_prepare_directories <- function() {
  dir.create(file.path(TRUST_OUTPUT_DIR, "charts"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(TRUST_OUTPUT_DIR, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(TRUST_OUTPUT_DIR, "raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(TRUST_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
}


trust_period_totals <- function(all_run_co2, validation_years) {
  eu <- all_run_co2 %>%
    filter(
      iso2 == "EU",
      fuel == "total",
      sector == "all",
      estimate == "central"
    ) %>%
    transmute(
      vintage_month = as.Date(vintage_month),
      date = as.Date(date),
      target_year = year(date),
      target_month_number = month(date),
      value = as.numeric(value)
    )

  period_totals <- bind_rows(
    eu %>% mutate(period = "Annual", expected_months = 12L),
    eu %>%
      filter(target_month_number <= 6L) %>%
      mutate(period = "H1", expected_months = 6L)
  ) %>%
    group_by(period, vintage_month, target_year, expected_months) %>%
    summarise(
      n_months = n_distinct(date),
      period_total = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_months == expected_months)

  plan <- tidyr::expand_grid(
    validation_year = validation_years,
    period = c("Annual", "H1")
  ) %>%
    mutate(reference_vintage = as.Date(sprintf("%d-01-01", validation_year + 2L)))

  target <- plan %>%
    inner_join(
      period_totals,
      by = c("period", "validation_year" = "target_year")
    ) %>%
    mutate(
      vintage_offset = if_else(
        period == "Annual",
        (year(vintage_month) - validation_year - 1L) * 12L + month(vintage_month),
        (year(vintage_month) - validation_year) * 12L + month(vintage_month) - 6L
      ),
      max_offset = if_else(period == "Annual", 12L, 18L)
    ) %>%
    filter(vintage_offset >= 1L, vintage_offset <= max_offset) %>%
    select(-max_offset)

  reference <- plan %>%
    inner_join(
      period_totals,
      by = c(
        "period",
        "validation_year" = "target_year",
        "reference_vintage" = "vintage_month"
      )
    ) %>%
    transmute(
      validation_year,
      period,
      reference_vintage,
      reference_total = period_total
    )

  target %>%
    left_join(reference, by = c("validation_year", "period", "reference_vintage")) %>%
    mutate(
      revision = period_total - reference_total,
      absolute_revision = abs(revision),
      revision_pct_of_total = 100 * revision / reference_total,
      absolute_revision_pct_of_total = abs(revision_pct_of_total)
    ) %>%
    arrange(period, validation_year, vintage_offset)
}


trust_trend_revisions <- function(all_run_co2, validation_years) {
  period_values <- trust_period_totals(all_run_co2, validation_years)
  totals <- all_run_co2 %>%
    filter(
      iso2 == "EU",
      fuel == "total",
      sector == "all",
      estimate == "central"
    ) %>%
    transmute(
      vintage_month = as.Date(vintage_month),
      date = as.Date(date),
      target_year = year(date),
      target_month_number = month(date),
      value = as.numeric(value)
    ) %>%
    {
      bind_rows(
        mutate(., period = "Annual", expected_months = 12L),
        filter(., target_month_number <= 6L) %>%
          mutate(period = "H1", expected_months = 6L)
      )
    } %>%
    group_by(period, vintage_month, target_year, expected_months) %>%
    summarise(
      n_months = n_distinct(date),
      period_total = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_months == expected_months)

  previous <- totals %>%
    transmute(
      period,
      vintage_month,
      validation_year = target_year + 1L,
      previous_total = period_total
    )
  reference_previous <- totals %>%
    transmute(
      period,
      reference_vintage = vintage_month,
      validation_year = target_year + 1L,
      reference_previous_total = period_total
    )

  period_values %>%
    left_join(previous, by = c("period", "vintage_month", "validation_year")) %>%
    left_join(
      reference_previous,
      by = c("period", "reference_vintage", "validation_year")
    ) %>%
    mutate(
      vintage_yoy_pct = 100 * (period_total / previous_total - 1),
      reference_yoy_pct = 100 * (reference_total / reference_previous_total - 1),
      trend_revision_pp = vintage_yoy_pct - reference_yoy_pct,
      absolute_trend_revision_pp = abs(trend_revision_pp),
      direction_switch = vintage_yoy_pct * reference_yoy_pct < 0
    ) %>%
    filter(is.finite(vintage_yoy_pct), is.finite(reference_yoy_pct)) %>%
    arrange(period, validation_year, vintage_offset)
}


trust_settling_sensitivity <- function(level_revisions, thresholds) {
  max_by_offset <- level_revisions %>%
    group_by(period, vintage_offset) %>%
    summarise(
      max_absolute_revision_pct = max(absolute_revision_pct_of_total, na.rm = TRUE),
      n_years = n_distinct(validation_year),
      .groups = "drop"
    )

  tidyr::expand_grid(
    period = unique(max_by_offset$period),
    threshold_pct = thresholds
  ) %>%
    group_by(period, threshold_pct) %>%
    group_modify(function(keys, group_keys) {
      candidates <- max_by_offset %>%
        filter(period == group_keys$period) %>%
        arrange(vintage_offset) %>%
        mutate(
          max_from_here = rev(cummax(rev(max_absolute_revision_pct))),
          remains_below = max_from_here <= group_keys$threshold_pct
        )
      first_offset <- candidates %>%
        filter(remains_below) %>%
        summarise(value = suppressWarnings(min(vintage_offset))) %>%
        pull(value)
      tibble(
        first_settled_offset = if (length(first_offset) == 0 ||
          is.infinite(first_offset)) {
          NA_integer_
        } else {
          as.integer(first_offset)
        },
        n_years = max(candidates$n_years)
      )
    }) %>%
    ungroup() %>%
    mutate(
      first_settled_vintage = map2_chr(
        period,
        first_settled_offset,
        function(period_value, offset) {
          if (is.na(offset)) {
            return(NA_character_)
          }
          if (period_value == "Annual") {
            return(month.abb[[offset]])
          }
          if (offset <= 6L) {
            return(month.abb[[offset + 6L]])
          }
          paste0(month.abb[[offset - 6L]], " y+1")
        }
      )
    )
}


trust_trend_sensitivity <- function(trend_revisions, thresholds) {
  tidyr::crossing(trend_revisions, threshold_pp = thresholds) %>%
    group_by(period, vintage_offset, threshold_pp) %>%
    summarise(
      n_years = n_distinct(validation_year),
      n_at_or_above_threshold = sum(absolute_trend_revision_pp >= threshold_pp),
      share_at_or_above_threshold = n_at_or_above_threshold / n_years,
      n_direction_switches = sum(direction_switch),
      share_direction_switches = n_direction_switches / n_years,
      .groups = "drop"
    )
}


trust_external_summary <- function(annual_pairs) {
  annual_pairs %>%
    filter(crea_variant == "raw", has_external, has_crea) %>%
    distinct(source_id, source_short, iso2, year, external_value_mt, crea_value_mt) %>%
    arrange(source_id, iso2, year) %>%
    group_by(source_id, source_short, iso2) %>%
    mutate(
      external_change = external_value_mt - lag(external_value_mt),
      crea_change = crea_value_mt - lag(crea_value_mt),
      trend_compared = !is.na(external_change) & !is.na(crea_change),
      trend_agrees = sign(external_change) == sign(crea_change),
      pct_difference = 100 * (crea_value_mt - external_value_mt) / external_value_mt
    ) %>%
    ungroup() %>%
    mutate(scope = if_else(iso2 == "EU", "EU", "Countries")) %>%
    group_by(source_id, source_short, scope) %>%
    summarise(
      first_year = min(year),
      last_year = max(year),
      n_pairs = n(),
      n_regions = n_distinct(iso2),
      mean_difference_mt = mean(crea_value_mt - external_value_mt),
      mean_absolute_difference_mt = mean(abs(crea_value_mt - external_value_mt)),
      mean_absolute_pct_difference = mean(abs(pct_difference)),
      correlation = if (n() > 1) cor(crea_value_mt, external_value_mt) else NA_real_,
      n_trends = sum(trend_compared),
      trend_agreement_pct = if_else(
        n_trends > 0,
        100 * sum(trend_agrees & trend_compared) / n_trends,
        NA_real_
      ),
      .groups = "drop"
    )
}


trust_gcb_fuel_comparison <- function(co2, gcb) {
  aviation <- co2 %>%
    filter(
      iso2 == "EU",
      estimate == "central",
      fuel == "oil",
      sector == "transport_international_aviation"
    ) %>%
    transmute(date = floor_date(as.Date(date), "month"), aviation = value)

  crea <- co2 %>%
    filter(
      iso2 == "EU",
      estimate == "central",
      sector == "all",
      fuel %in% c("coal", "coke", "gas", "oil")
    ) %>%
    transmute(
      date = floor_date(as.Date(date), "month"),
      year = year(date),
      fuel = recode(fuel, coke = "coal"),
      value
    ) %>%
    left_join(aviation, by = "date") %>%
    mutate(value = if_else(fuel == "oil", value - coalesce(aviation, 0), value)) %>%
    group_by(year, fuel) %>%
    summarise(crea_value_mt = sum(value, na.rm = TRUE) / 1e6, .groups = "drop") %>%
    bind_rows(
      group_by(., year) %>%
        summarise(crea_value_mt = sum(crea_value_mt), fuel = "total", .groups = "drop")
    )

  gcb %>%
    filter(iso2 == "EU", fuel %in% c("coal", "gas", "oil", "total")) %>%
    transmute(year = as.integer(year), fuel, gcb_value_mt = as.numeric(value)) %>%
    inner_join(crea, by = c("year", "fuel")) %>%
    mutate(
      difference_mt = crea_value_mt - gcb_value_mt,
      pct_difference = 100 * difference_mt / gcb_value_mt
    ) %>%
    arrange(fuel, year)
}


trust_save_png <- function(plot, path) {
  rcrea::quicksave(
    file = path,
    plot = plot,
    width = TRUST_PLOT_WIDTH,
    height = TRUST_PLOT_HEIGHT,
    dpi = TRUST_PLOT_DPI,
    bg = "white",
    preview = FALSE
  )
  path
}


trust_external_artifact_paths <- function() {
  c(
    current_co2 = file.path(TRUST_OUTPUT_DIR, "raw", "crea_co2.csv"),
    external_sources = file.path(TRUST_OUTPUT_DIR, "raw", "external_sources.csv"),
    gcb_raw = file.path(TRUST_OUTPUT_DIR, "raw", "gcb_validation.csv"),
    source_status = file.path(TRUST_OUTPUT_DIR, "source_status.csv"),
    annual_pairs = file.path(TRUST_OUTPUT_DIR, "tables", "external_annual_pairs.csv"),
    external_summary = file.path(TRUST_OUTPUT_DIR, "tables", "external_summary.csv"),
    largest_differences = file.path(
      TRUST_OUTPUT_DIR,
      "tables",
      "external_largest_differences.csv"
    ),
    chart_data = file.path(TRUST_OUTPUT_DIR, "tables", "chart_01_external_series.csv"),
    trend_chart_data = file.path(TRUST_OUTPUT_DIR, "tables", "chart_02_external_trends.csv"),
    gcb_fuel = file.path(TRUST_OUTPUT_DIR, "tables", "gcb_fuel_comparison.csv"),
    gcb_fuel_summary = file.path(TRUST_OUTPUT_DIR, "tables", "gcb_fuel_summary.csv")
  )
}


trust_external_raw_artifact_paths <- function(paths = trust_external_artifact_paths()) {
  paths[c("current_co2", "external_sources", "gcb_raw", "source_status")]
}


trust_vintage_artifact_paths <- function() {
  revision_dir <- file.path(TRUST_OUTPUT_DIR, "raw", "revision_analysis")
  c(
    all_run_co2 = file.path(revision_dir, "all_run_co2.parquet"),
    vintage_co2 = file.path(revision_dir, "vintage_co2.parquet"),
    reference_vintage_co2 = file.path(revision_dir, "reference_vintage_co2.parquet"),
    level_revisions = file.path(TRUST_OUTPUT_DIR, "tables", "chart_02_level_revisions.csv"),
    trend_revisions = file.path(TRUST_OUTPUT_DIR, "tables", "trend_revisions.csv"),
    settling = file.path(TRUST_OUTPUT_DIR, "tables", "settling_sensitivity.csv"),
    trend_sensitivity = file.path(TRUST_OUTPUT_DIR, "tables", "trend_sensitivity.csv")
  )
}


trust_plot_external <- function(plot_data, source_status, path) {
  focus_labels <- c(
    "CREA" = "CREA",
    "Global Carbon Budget 2025" = "Global Carbon Budget"
  )
  focus <- plot_data %>% filter(focus)
  background <- plot_data %>%
    filter(!focus) %>%
    add_count(series, name = "n_years") %>%
    filter(n_years >= 10L) %>%
    select(-n_years)
  focus_colors <- c(
    "CREA" = rcrea::pal_crea[["Dark.red"]],
    "Global Carbon Budget 2025" = rcrea::pal_crea[["Dark.blue"]]
  )

  plt <- ggplot() +
    geom_line(
      data = background,
      aes(year, value_mt, group = series),
      color = rcrea::pal_crea[["Light.gray"]],
      linewidth = 0.75,
      alpha = 0.7
    ) +
    geom_line(
      data = focus,
      aes(year, value_mt, color = series),
      linewidth = 1.8
    ) +
    scale_color_manual(
      values = focus_colors,
      breaks = names(focus_labels),
      labels = focus_labels,
      name = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = "EU emissions datasets show the same broad direction (1990-2024)",
      subtitle = "Annual fossil CO2 emissions (MtCO2) | EU27 | CREA and Global Carbon Budget emphasised",
      x = NULL,
      y = NULL,
      caption = paste(
        str_wrap(
          paste0(
            "Dataset scopes and methods differ; these differences contribute to variation ",
            "between the series."
          ),
          width = 100
        ),
        "Source: CREA analysis.",
        sep = "\n"
      )
    ) +
    rcrea::theme_crea_new(fontsize1 = 24, fontsize2 = 18, fontsize3 = 14, fontsize4 = 12)

  trust_save_png(plt, path)
}


trust_external_trends <- function(plot_data) {
  plot_data %>%
    group_by(series, focus) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      previous_year = lag(year),
      previous_value_mt = lag(value_mt),
      trend_pct = if_else(
        year == previous_year + 1L,
        100 * (value_mt / previous_value_mt - 1),
        NA_real_
      )
    ) %>%
    ungroup() %>%
    filter(is.finite(trend_pct)) %>%
    select(-previous_year, -previous_value_mt)
}


trust_plot_external_trends <- function(plot_data, path) {
  focus_labels <- c(
    "CREA" = "CREA",
    "Global Carbon Budget 2025" = "Global Carbon Budget"
  )
  focus_colors <- c(
    "CREA" = rcrea::pal_crea[["Dark.red"]],
    "Global Carbon Budget 2025" = rcrea::pal_crea[["Dark.blue"]]
  )
  focus <- plot_data %>% filter(focus)
  background <- plot_data %>%
    filter(!focus) %>%
    add_count(series, name = "n_years") %>%
    filter(n_years >= 10L) %>%
    select(-n_years)

  plt <- ggplot() +
    geom_hline(
      yintercept = 0,
      color = rcrea::pal_crea[["Light.gray"]],
      linewidth = 0.75
    ) +
    geom_line(
      data = background,
      aes(year, trend_pct, group = series),
      color = rcrea::pal_crea[["Light.gray"]],
      linewidth = 0.75,
      alpha = 0.7
    ) +
    geom_line(
      data = focus,
      aes(year, trend_pct, color = series),
      linewidth = 1.8
    ) +
    scale_color_manual(
      values = focus_colors,
      breaks = names(focus_labels),
      labels = focus_labels,
      name = NULL
    ) +
    scale_x_continuous(
      limits = c(TRUST_COMPARISON_MIN_YEAR, TRUST_COMPARISON_MAX_YEAR),
      breaks = c(1990L, 2000L, 2010L, 2020L, 2024L),
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
    labs(
      title = "EU emissions datasets show similar annual changes (1990-2024)",
      subtitle = paste0(
        "Year-on-year change (% of previous year) | EU27 | ",
        "CREA and Global Carbon Budget emphasised"
      ),
      x = NULL,
      y = NULL,
      caption = paste(
        str_wrap(
          paste0(
            "Dataset scopes and methods differ; these differences contribute to variation ",
            "between the series."
          ),
          width = 100
        ),
        "Source: CREA analysis.",
        sep = "\n"
      )
    ) +
    rcrea::theme_crea_new(fontsize1 = 24, fontsize2 = 18, fontsize3 = 14, fontsize4 = 12)

  trust_save_png(plt, path)
}


trust_plot_level_revisions <- function(level_revisions, path) {
  vintage_axis <- bind_rows(
    tibble(period = "Annual", vintage_offset = 1:12, month = month.abb),
    tibble(period = "H1", vintage_offset = 1:18, month = month.abb[c(7:12, 1:12)])
  ) %>%
    mutate(
      show_label = case_when(
        period == "Annual" ~ vintage_offset %in% c(1L, 4L, 7L, 10L),
        period == "H1" ~ vintage_offset %in% c(1L, 4L, 7L, 10L, 13L, 16L)
      ),
      vintage_key = paste(period, vintage_offset, sep = "_"),
      vintage_label = case_when(
        !show_label ~ "",
        TRUE ~ paste(vintage_offset, month, sep = "\n")
      )
    )
  vintage_levels <- vintage_axis$vintage_key
  vintage_labels <- setNames(vintage_axis$vintage_label, vintage_axis$vintage_key)
  plot_revisions <- level_revisions %>%
    left_join(vintage_axis, by = c("period", "vintage_offset")) %>%
    mutate(vintage_key = factor(vintage_key, levels = vintage_levels))
  medians <- plot_revisions %>%
    group_by(period, vintage_offset, vintage_key) %>%
    summarise(
      absolute_revision_pct_of_total = median(absolute_revision_pct_of_total),
      .groups = "drop"
    )
  plt <- ggplot(plot_revisions, aes(vintage_key, absolute_revision_pct_of_total)) +
    geom_line(
      aes(group = validation_year),
      color = rcrea::pal_crea[["Light.gray"]],
      linewidth = 0.83
    ) +
    geom_point(color = rcrea::pal_crea[["Light.gray"]], size = 1.5) +
    geom_line(
      data = medians,
      aes(group = period),
      color = rcrea::pal_crea[["Dark.red"]],
      linewidth = 1.8
    ) +
    facet_wrap(
      ~period,
      scales = "free_x",
      labeller = as_labeller(c(
        "Annual" = "Full year (ends in December)",
        "H1" = "First half of year (H1, ends in June)"
      ))
    ) +
    scale_x_discrete(labels = vintage_labels) +
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
    labs(
      title = paste0(
        "CREA's estimates converge in the months after each reporting period ",
        "(2020-2024)"
      ),
      subtitle = "How much each estimate differs from the later reference (%) | EU27 | Median red, individual years grey",
      x = "Time (months) after the reporting period ended",
      y = NULL,
      caption = paste(
        "Each point represents an estimate by month after the reporting period ended.",
        "Source: CREA analysis.",
        sep = "\n"
      )
    ) +
    rcrea::theme_crea_new(fontsize1 = 24, fontsize2 = 18, fontsize3 = 14, fontsize4 = 12)

  trust_save_png(plt, path)
}


trust_render_charts <- function(
  output_dir,
  external_plot_data,
  level_revisions,
  source_status
) {
  dir.create(file.path(output_dir, "charts"), recursive = TRUE, showWarnings = FALSE)
  chart_paths <- c(
    trust_plot_external(
      external_plot_data,
      source_status,
      file.path(output_dir, "charts", "01_eu_external_comparison.png")
    ),
    trust_plot_external_trends(
      trust_external_trends(external_plot_data),
      file.path(output_dir, "charts", "02_eu_external_trends.png")
    ),
    trust_plot_level_revisions(
      level_revisions,
      file.path(output_dir, "charts", "03_revisions_settle_over_time.png")
    )
  )
  readr::write_csv(
    tibble(chart = basename(chart_paths)),
    file.path(output_dir, "charts.csv")
  )
  chart_paths
}


trust_build_external_plot_data <- function(crea_totals, external) {
  gcb_max_year <- external %>%
    filter(source_id == "global-carbon-budget-2025", period == "annual", iso2 == "EU") %>%
    summarise(value = max(year, na.rm = TRUE)) %>%
    pull(value)
  if (length(gcb_max_year) != 1 || !is.finite(gcb_max_year)) {
    stop("Global Carbon Budget EU coverage is required for the external chart.", call. = FALSE)
  }
  if (gcb_max_year < TRUST_COMPARISON_MAX_YEAR) {
    stop(
      "Global Carbon Budget EU coverage does not reach the comparison end year.",
      call. = FALSE
    )
  }

  crea_years <- crea_totals %>%
    filter(crea_variant == "adjusted", period == "annual", iso2 == "EU") %>%
    pull(year)
  if (!all(c(TRUST_COMPARISON_MIN_YEAR, TRUST_COMPARISON_MAX_YEAR) %in% crea_years)) {
    stop(
      "CREA annual coverage does not span the configured comparison years.",
      call. = FALSE
    )
  }

  external_lines <- external %>%
    filter(
      period == "annual",
      iso2 == "EU",
      year >= TRUST_COMPARISON_MIN_YEAR,
      year <= TRUST_COMPARISON_MAX_YEAR
    ) %>%
    transmute(
      year,
      value_mt,
      series = source,
      focus = source_id == "global-carbon-budget-2025"
    ) %>%
    group_by(series) %>%
    filter(n_distinct(year) >= 10L) %>%
    ungroup()
  crea_lines <- crea_totals %>%
    filter(
      crea_variant == "raw",
      period == "annual",
      iso2 == "EU",
      year >= TRUST_COMPARISON_MIN_YEAR,
      year <= TRUST_COMPARISON_MAX_YEAR
    ) %>%
    transmute(
      year,
      value_mt,
      series = "CREA",
      focus = TRUE
    )

  bind_rows(external_lines, crea_lines) %>%
    filter(is.finite(value_mt)) %>%
    distinct(series, year, .keep_all = TRUE) %>%
    arrange(series, year)
}


trust_headline_stats <- function(external_summary, settling, trend_sensitivity) {
  gcb <- external_summary %>%
    filter(source_id == "global-carbon-budget-2025", scope == "EU")
  switches <- trend_sensitivity %>%
    distinct(period, vintage_offset, n_years, n_direction_switches) %>%
    group_by(period) %>%
    summarise(
      metric_value = sum(n_direction_switches),
      observations = sum(n_years),
      .groups = "drop"
    ) %>%
    transmute(
      metric = paste0(tolower(period), "_trend_direction_switches"),
      value = metric_value,
      unit = "cases",
      observations,
      definition = "Vintage and later-reference year-on-year changes have opposite signs."
    )

  bind_rows(
    tibble(
      metric = c("gcb_eu_mean_absolute_difference", "gcb_eu_trend_agreement"),
      value = c(gcb$mean_absolute_pct_difference, gcb$trend_agreement_pct),
      unit = "percent",
      observations = c(gcb$n_pairs, gcb$n_trends),
      definition = c(
        "Mean absolute annual difference between CREA and GCB EU totals.",
        "Share of paired annual changes with the same direction in CREA and GCB."
      )
    ),
    settling %>%
      transmute(
        metric = paste0(tolower(period), "_settled_below_", threshold_pct, "pct"),
        value = first_settled_offset,
        unit = "months after period",
        observations = n_years,
        definition = paste0(
          "First vintage after which every observed year remains within ",
          threshold_pct,
          "% of the later-reference total."
        )
      ),
    switches
  )
}


trust_write_key_numbers <- function(
  external_summary,
  annual_pairs,
  level_revisions,
  trend_revisions,
  settling,
  trend_sensitivity
) {
  gcb <- external_summary %>%
    filter(source_id == "global-carbon-budget-2025", scope == "EU")
  gcb_2024 <- annual_pairs %>%
    filter(
      source_id == "global-carbon-budget-2025",
      iso2 == "EU",
      year == TRUST_COMPARISON_MAX_YEAR,
      crea_variant == "raw"
    ) %>%
    distinct(year, external_value_mt, crea_value_mt, diff_mt, pct_diff)
  if (nrow(gcb) != 1 || nrow(gcb_2024) != 1) {
    stop("Expected one EU Global Carbon Budget summary and one 2024 pair.", call. = FALSE)
  }

  publication_levels <- level_revisions %>%
    filter(
      (period == "Annual" & vintage_offset == 1L) |
        (period == "H1" & vintage_offset == 3L)
    ) %>%
    group_by(period) %>%
    summarise(
      n_years = n_distinct(validation_year),
      median_absolute_revision_pct = median(absolute_revision_pct_of_total),
      maximum_absolute_revision_pct = max(absolute_revision_pct_of_total),
      n_within_one_pct = sum(absolute_revision_pct_of_total <= 1),
      .groups = "drop"
    )
  publication_trends <- trend_revisions %>%
    filter(
      (period == "Annual" & vintage_offset == 1L) |
        (period == "H1" & vintage_offset == 3L)
    ) %>%
    group_by(period) %>%
    summarise(
      n_years = n_distinct(validation_year),
      median_absolute_revision_pp = median(absolute_trend_revision_pp),
      maximum_absolute_revision_pp = max(absolute_trend_revision_pp),
      .groups = "drop"
    )
  direction_switches <- trend_sensitivity %>%
    distinct(period, vintage_offset, n_years, n_direction_switches) %>%
    group_by(period) %>%
    summarise(
      comparisons = sum(n_years),
      switches = sum(n_direction_switches),
      .groups = "drop"
    )
  threshold_counts <- trend_sensitivity %>%
    filter(threshold_pp %in% c(1, 2)) %>%
    group_by(period, threshold_pp) %>%
    summarise(
      comparisons = sum(n_years),
      at_or_above = sum(n_at_or_above_threshold),
      .groups = "drop"
    )

  get_value <- function(data, period_value, column) {
    value <- data %>%
      filter(period == period_value) %>%
      pull(all_of(column))
    if (length(value) != 1) {
      stop("Missing or duplicated key-number input for ", period_value, ".", call. = FALSE)
    }
    value[[1]]
  }
  get_threshold_value <- function(period_value, threshold, column) {
    value <- threshold_counts %>%
      filter(period == period_value, threshold_pp == threshold) %>%
      pull(all_of(column))
    if (length(value) != 1) {
      stop("Missing or duplicated trend threshold input.", call. = FALSE)
    }
    value[[1]]
  }
  get_settling_vintage <- function(period_value, threshold) {
    value <- settling %>%
      filter(period == period_value, threshold_pct == threshold) %>%
      pull(first_settled_vintage)
    if (length(value) != 1) {
      stop("Missing or duplicated settling input.", call. = FALSE)
    }
    vintage <- value[[1]]
    month_abbreviation <- sub(" y[+]1$", "", vintage)
    month_full <- month.name[match(month_abbreviation, month.abb)]
    if (is.na(month_full)) {
      stop("Unexpected settling-vintage label: ", vintage, call. = FALSE)
    }
    if (grepl(" y[+]1$", vintage)) {
      paste(month_full, "of the following year")
    } else {
      month_full
    }
  }

  external_lines <- external_summary %>%
    filter(scope == "EU", n_pairs >= 10L) %>%
    arrange(source_short) %>%
    mutate(
      source_short = if_else(
        source_id == "global-carbon-budget-2025",
        "Global Carbon Budget",
        source_short
      ),
      line = sprintf(
        paste0(
          "- %s: %d-%d; %d paired years; mean absolute difference %.2f%%; ",
          "same trend direction %.1f%% of %d annual changes."
        ),
        source_short,
        first_year,
        last_year,
        n_pairs,
        mean_absolute_pct_difference,
        trend_agreement_pct,
        n_trends
      )
    ) %>%
    pull(line)

  annual_level_years <- get_value(publication_levels, "Annual", "n_years")
  h1_level_years <- get_value(publication_levels, "H1", "n_years")
  annual_switch_comparisons <- get_value(direction_switches, "Annual", "comparisons")
  h1_switch_comparisons <- get_value(direction_switches, "H1", "comparisons")
  gcb_agreeing_trends <- round(gcb$trend_agreement_pct * gcb$n_trends / 100)
  gcb_2024_relation <- if (gcb_2024$diff_mt < 0) "lower" else "higher"

  lines <- c(
    "EU CO2 TRACKER: KEY NUMBERS",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    "SCOPE",
    sprintf(
      "- External comparison: EU annual estimates from %d to %d, using the January 2026 CREA vintage.",
      TRUST_COMPARISON_MIN_YEAR,
      TRUST_COMPARISON_MAX_YEAR
    ),
    sprintf(
      paste0(
        "- Revision analysis: %d reporting years (%d-%d). Reference estimates are ",
        "from January two years after each reporting year, by which point historical ",
        "revisions have generally settled."
      ),
      length(TRUST_VALIDATION_YEARS),
      min(TRUST_VALIDATION_YEARS),
      max(TRUST_VALIDATION_YEARS)
    ),
    "",
    "COMPARISON WITH EXTERNAL DATASETS",
    sprintf(
      "- CREA and the Global Carbon Budget differ by an average of %.2f%% in absolute terms across %d paired annual EU estimates (%d-%d).",
      gcb$mean_absolute_pct_difference,
      gcb$n_pairs,
      gcb$first_year,
      gcb$last_year
    ),
    sprintf(
      "- CREA and the Global Carbon Budget show the same direction of annual change in %d of %d comparisons (%.1f%%).",
      gcb_agreeing_trends,
      gcb$n_trends,
      gcb$trend_agreement_pct
    ),
    sprintf(
      paste0(
        "- In %d, CREA estimates %.0f MtCO2 and the Global Carbon Budget estimates ",
        "%.0f MtCO2: CREA is %.0f MtCO2 (%.2f%%) %s."
      ),
      gcb_2024$year,
      gcb_2024$crea_value_mt,
      gcb_2024$external_value_mt,
      abs(gcb_2024$diff_mt),
      abs(gcb_2024$pct_diff),
      gcb_2024_relation
    ),
    "",
    "EU EXTERNAL-DATASET SUMMARY",
    external_lines,
    "",
    "HISTORICAL REVISIONS AT PUBLICATION TIME",
    sprintf(
      "- Annual estimates published in January: median absolute revision %.2f%%; maximum %.2f%%; %d of %d estimates were within 1%% of the reference estimate.",
      get_value(publication_levels, "Annual", "median_absolute_revision_pct"),
      get_value(publication_levels, "Annual", "maximum_absolute_revision_pct"),
      get_value(publication_levels, "Annual", "n_within_one_pct"),
      annual_level_years
    ),
    sprintf(
      "- H1 estimates published in September: median absolute revision %.2f%%; maximum %.2f%%; %d of %d estimates were within 1%% of the reference estimate.",
      get_value(publication_levels, "H1", "median_absolute_revision_pct"),
      get_value(publication_levels, "H1", "maximum_absolute_revision_pct"),
      get_value(publication_levels, "H1", "n_within_one_pct"),
      h1_level_years
    ),
    sprintf(
      "- Annual estimates remain within 1%% from %s and within 0.5%% from %s.",
      get_settling_vintage("Annual", 1),
      get_settling_vintage("Annual", 0.5)
    ),
    sprintf(
      "- H1 estimates remain within 1%% from %s and within 0.5%% from %s.",
      get_settling_vintage("H1", 1),
      get_settling_vintage("H1", 0.5)
    ),
    "",
    "EFFECT ON REPORTED TRENDS",
    sprintf(
      "- At publication, the median absolute trend revision was %.2f percentage points for annual estimates and %.2f points for H1; the maxima were %.2f and %.2f points.",
      get_value(publication_trends, "Annual", "median_absolute_revision_pp"),
      get_value(publication_trends, "H1", "median_absolute_revision_pp"),
      get_value(publication_trends, "Annual", "maximum_absolute_revision_pp"),
      get_value(publication_trends, "H1", "maximum_absolute_revision_pp")
    ),
    sprintf(
      "- Revisions switched the direction of change in %d of %d annual and %d of %d H1 vintage-year comparisons.",
      get_value(direction_switches, "Annual", "switches"),
      annual_switch_comparisons,
      get_value(direction_switches, "H1", "switches"),
      h1_switch_comparisons
    ),
    sprintf(
      "- Annual trend revisions reached at least 1 percentage point in %d of %d comparisons and at least 2 points in %d of %d.",
      get_threshold_value("Annual", 1, "at_or_above"),
      get_threshold_value("Annual", 1, "comparisons"),
      get_threshold_value("Annual", 2, "at_or_above"),
      get_threshold_value("Annual", 2, "comparisons")
    ),
    sprintf(
      "- H1 trend revisions reached at least 1 percentage point in %d of %d comparisons and at least 2 points in %d of %d.",
      get_threshold_value("H1", 1, "at_or_above"),
      get_threshold_value("H1", 1, "comparisons"),
      get_threshold_value("H1", 2, "at_or_above"),
      get_threshold_value("H1", 2, "comparisons")
    ),
    "",
    "INTERPRETATION",
    "- Historical revisions show how estimates changed as more source data became available. They do not capture every source of uncertainty or prove that an estimate is accurate.",
    paste0(
      "- Reference estimates are later, more mature and relatively stable: historical ",
      "revisions have generally settled by that point. Stable does not mean final, ",
      "error-free or the truth."
    ),
    "- Dataset scopes and methods differ. These differences contribute to variation between the series, but do not explain every difference and should not be interpreted as evidence that either estimate is wrong.",
    "",
    "SOURCE TABLES",
    "- tables/external_summary.csv",
    "- tables/external_annual_pairs.csv",
    "- tables/chart_01_external_series.csv",
    "- tables/chart_02_external_trends.csv",
    "- tables/chart_02_level_revisions.csv",
    "- tables/trend_revisions.csv",
    "- tables/settling_sensitivity.csv",
    "- tables/trend_sensitivity.csv"
  )

  writeLines(lines, file.path(TRUST_OUTPUT_DIR, "key_numbers.txt"), useBytes = TRUE)
}


trust_artifacts_ready <- function(paths) {
  all(file.exists(paths)) && all(file.info(paths)$size > 0)
}


trust_stage_is_done <- function(marker, artifacts) {
  file.exists(marker) && trust_artifacts_ready(artifacts)
}


trust_mark_stage_done <- function(marker, artifacts) {
  if (!trust_artifacts_ready(artifacts)) {
    stop("Refusing to mark an analysis stage complete before all artifacts exist.", call. = FALSE)
  }
  writeLines(format(Sys.time(), tz = "UTC", usetz = TRUE), marker)
}


trust_run_external_comparison <- function(external_env, comparison_env) {
  paths <- trust_external_artifact_paths()
  raw_paths <- trust_external_raw_artifact_paths(paths)
  raw_marker <- file.path(TRUST_OUTPUT_DIR, "external_raw.done")
  if (trust_stage_is_done(raw_marker, raw_paths)) {
    message("External raw data is complete; skipping source collection.")
    current_co2 <- readr::read_csv(raw_paths[["current_co2"]], show_col_types = FALSE)
    external <- readr::read_csv(raw_paths[["external_sources"]], show_col_types = FALSE)
    gcb_raw <- readr::read_csv(raw_paths[["gcb_raw"]], show_col_types = FALSE)
  } else {
    message("Collecting external raw data...")
    current_co2 <- get_co2(
      date_to = TRUST_DATE_TO,
      min_year = TRUST_COMPARISON_MIN_YEAR,
      downscale_daily = FALSE,
      diagnostics_folder = NULL,
      use_cache = TRUE
    )
    external_result <- external_env$collect_external_sources(
      sources = external_env$resolve_sources("all"),
      periods = "annual",
      allow_source_failures = FALSE,
      date_to = TRUST_DATE_TO,
      use_cache = TRUE
    )
    external <- external_result$data
    source_status <- external_result$status
    gcb_raw <- get_validation_data(
      region = get_eu_iso2s(include_eu = TRUE),
      source_name = "Global Carbon Budget 2025"
    )

    readr::write_csv(current_co2, raw_paths[["current_co2"]])
    readr::write_csv(external, raw_paths[["external_sources"]])
    readr::write_csv(gcb_raw, raw_paths[["gcb_raw"]])
    readr::write_csv(source_status, raw_paths[["source_status"]])
    trust_mark_stage_done(raw_marker, raw_paths)
  }

  message("Reprocessing external comparison artifacts...")

  crea_totals <- comparison_env$normalise_crea_totals(current_co2, TRUST_DATE_TO)
  annual_pairs <- comparison_env$make_pairs(crea_totals, external) %>%
    filter(
      period == "annual",
      year >= TRUST_COMPARISON_MIN_YEAR,
      year <= TRUST_COMPARISON_MAX_YEAR
    )
  external_summary <- trust_external_summary(annual_pairs)
  external_plot_data <- trust_build_external_plot_data(crea_totals, external)
  external_trend_plot_data <- trust_external_trends(external_plot_data)
  largest_external_differences <- annual_pairs %>%
    filter(crea_variant == "adjusted", has_external, has_crea) %>%
    mutate(
      absolute_difference_mt = abs(diff_mt),
      absolute_pct_difference = abs(pct_diff)
    ) %>%
    arrange(desc(absolute_difference_mt)) %>%
    slice_head(n = 100)

  gcb_fuel <- trust_gcb_fuel_comparison(current_co2, gcb_raw) %>%
    filter(
      year >= TRUST_COMPARISON_MIN_YEAR,
      year <= TRUST_COMPARISON_MAX_YEAR
    )
  gcb_fuel_summary <- gcb_fuel %>%
    group_by(fuel) %>%
    summarise(
      first_year = min(year),
      last_year = max(year),
      n_years = n(),
      mean_difference_mt = mean(difference_mt),
      mean_absolute_difference_mt = mean(abs(difference_mt)),
      mean_absolute_pct_difference = mean(abs(pct_difference)),
      .groups = "drop"
    )

  readr::write_csv(annual_pairs, paths[["annual_pairs"]])
  readr::write_csv(external_summary, paths[["external_summary"]])
  readr::write_csv(largest_external_differences, paths[["largest_differences"]])
  readr::write_csv(external_plot_data, paths[["chart_data"]])
  readr::write_csv(external_trend_plot_data, paths[["trend_chart_data"]])
  readr::write_csv(gcb_fuel, paths[["gcb_fuel"]])
  readr::write_csv(gcb_fuel_summary, paths[["gcb_fuel_summary"]])
  invisible(paths)
}


trust_run_vintage_analysis <- function() {
  paths <- trust_vintage_artifact_paths()
  marker <- file.path(TRUST_OUTPUT_DIR, "vintage_analysis.done")
  if (trust_stage_is_done(marker, paths)) {
    message("Vintage analysis artifacts are complete; skipping this stage.")
    return(invisible(paths))
  }

  message("Generating vintage analysis artifacts...")
  revision_dir <- file.path(TRUST_OUTPUT_DIR, "raw", "revision_analysis")
  dir.create(revision_dir, recursive = TRUE, showWarnings = FALSE)
  validate_get_co2_revision_analysis(
    output_folder = revision_dir,
    validation_years = TRUST_VALIDATION_YEARS,
    include_country_detail_charts = FALSE,
    use_cache = TRUE,
    reuse_run_cache = TRUE,
    min_year = min(TRUST_VALIDATION_YEARS) - 1L,
    render_diagnostic_charts = FALSE
  )

  all_run_co2 <- arrow::read_parquet(paths[["all_run_co2"]])
  level_revisions <- trust_period_totals(all_run_co2, TRUST_VALIDATION_YEARS)
  trend_revisions <- trust_trend_revisions(all_run_co2, TRUST_VALIDATION_YEARS)
  settling <- trust_settling_sensitivity(
    level_revisions,
    TRUST_SETTLING_THRESHOLDS_PCT
  )
  trend_sensitivity <- trust_trend_sensitivity(
    trend_revisions,
    TRUST_TREND_THRESHOLDS_PP
  )

  readr::write_csv(level_revisions, paths[["level_revisions"]])
  readr::write_csv(trend_revisions, paths[["trend_revisions"]])
  readr::write_csv(settling, paths[["settling"]])
  readr::write_csv(trend_sensitivity, paths[["trend_sensitivity"]])
  trust_mark_stage_done(marker, paths)
  invisible(paths)
}


trust_write_manifest <- function(source_status, cache_dir) {
  git_sha <- suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
  source_versions <- source_status %>%
    filter(status == "ok") %>%
    distinct(source) %>%
    pull(source) %>%
    paste(collapse = "; ")
  manifest <- tibble(
    key = c(
      "created_at_utc",
      "git_sha",
      "date_to",
      "validation_years",
      "comparison_years",
      "reference_vintage",
      "cache_policy",
      "cache_dir",
      "output_dir",
      "successful_external_sources"
    ),
    value = c(
      format(Sys.time(), tz = "UTC", usetz = TRUE),
      git_sha[[1]],
      as.character(TRUST_DATE_TO),
      paste(TRUST_VALIDATION_YEARS, collapse = ","),
      paste0(TRUST_COMPARISON_MIN_YEAR, "-", TRUST_COMPARISON_MAX_YEAR),
      "January 2026 for the 2024 validation year",
      "Persistent cache enabled for source collection and vintage runs",
      cache_dir,
      TRUST_OUTPUT_DIR,
      source_versions
    )
  )
  readr::write_csv(manifest, file.path(TRUST_OUTPUT_DIR, "run_manifest.csv"))
}


trust_main <- function() {
  trust_prepare_directories()
  cache_dir <- normalizePath(TRUST_CACHE_DIR, mustWork = TRUE)
  options(creaco2tracker.cache_dir = cache_dir)
  Sys.setenv(CREACO2TRACKER_CACHE_DIR = cache_dir)

  devtools::load_all(".", quiet = TRUE)
  external_env <- new.env(parent = globalenv())
  comparison_env <- new.env(parent = globalenv())
  sys.source("scripts/compare_lib/collect_external_co2_sources.R", envir = external_env)
  sys.source("scripts/compare_lib/compare_get_co2_external.R", envir = comparison_env)

  external_paths <- trust_run_external_comparison(external_env, comparison_env)
  vintage_paths <- trust_run_vintage_analysis()
  source_status <- readr::read_csv(external_paths[["source_status"]], show_col_types = FALSE)
  external_summary <- readr::read_csv(
    external_paths[["external_summary"]],
    show_col_types = FALSE
  )
  annual_pairs <- readr::read_csv(
    external_paths[["annual_pairs"]],
    show_col_types = FALSE
  )
  external_plot_data <- readr::read_csv(
    external_paths[["chart_data"]],
    show_col_types = FALSE
  )
  level_revisions <- readr::read_csv(
    vintage_paths[["level_revisions"]],
    show_col_types = FALSE
  )
  trend_revisions <- readr::read_csv(
    vintage_paths[["trend_revisions"]],
    show_col_types = FALSE
  )
  settling <- readr::read_csv(vintage_paths[["settling"]], show_col_types = FALSE)
  trend_sensitivity <- readr::read_csv(
    vintage_paths[["trend_sensitivity"]],
    show_col_types = FALSE
  )
  headline_stats <- trust_headline_stats(
    external_summary,
    settling,
    trend_sensitivity
  )

  readr::write_csv(
    headline_stats,
    file.path(TRUST_OUTPUT_DIR, "tables", "headline_stats.csv")
  )

  trust_render_charts(
    output_dir = TRUST_OUTPUT_DIR,
    external_plot_data = external_plot_data,
    level_revisions = level_revisions,
    source_status = source_status
  )
  trust_write_key_numbers(
    external_summary = external_summary,
    annual_pairs = annual_pairs,
    level_revisions = level_revisions,
    trend_revisions = trend_revisions,
    settling = settling,
    trend_sensitivity = trend_sensitivity
  )
  trust_write_manifest(source_status, cache_dir)
  message("Completed CO2 trust analysis: ", TRUST_OUTPUT_DIR)
  invisible(TRUST_OUTPUT_DIR)
}


if (sys.nframe() == 0) {
  trust_main()
}
