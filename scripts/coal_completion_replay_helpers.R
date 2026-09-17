# Internal study runner. Loaded by validate_coal_completion.R after source
# boundaries have been replaced by the frozen inputs.
run_coal_downstream_replays <- function(inputs, root, years) {
  logger::log_threshold(logger::WARN)
  bounds <- as.integer(strsplit(years, ":", fixed = TRUE)[[1]])
  years <- seq.int(min(bounds), max(bounds))
  full_report <- identical(option("--full-report", "true"), "true")
  requested_methods <- strsplit(option("--methods",
    "repaired,previous_year,three_year_average,unchanged"), ",", fixed = TRUE)[[1]]
  if (length(requested_methods) == 0L || any(!requested_methods %in%
    c("repaired", "previous_year", "three_year_average", "unchanged"))) {
    stop("Unsupported replay method selection")
  }
  noncoal <- NULL
  output <- file.path(root, option("--holdout-folder", "holdouts_final"))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file.path(output, "RETIRED_CODE.txt"))) {
    stop("This folder contains results from retired code. Choose a fresh holdout folder.")
  }
  identity_file <- file.path(output, "input_identity.rds")
  input_identity <- vapply(file.path(root, c("inputs.rds", "reported_coal_monthly.parquet")),
    digest::digest, character(1), file = TRUE, algo = "sha256")
  if (file.exists(identity_file) && !identical(readRDS(identity_file), input_identity)) {
    stop("Replay inputs changed: choose a new holdout folder instead of mixing snapshots")
  }
  saveRDS(input_identity, identity_file)
  save_atomic <- function(object, path) {
    temporary <- tempfile(tmpdir = dirname(path))
    saveRDS(object, temporary)
    if (!file.rename(temporary, path)) stop("Could not finish replay artifact: ", path)
  }
  raw <- read_parquet(file.path(root, "reported_coal_monthly.parquet")) %>%
    mutate(iso2 = case_when(geo == "EL" ~ "GR", geo == "EU27_2020" ~ "EU", TRUE ~ geo)) %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = full_report),
      siec %in% COAL_MONTHLY_GAP_FUELS, unit == "THS_T")
  annual <- inputs$consumption$solid$yearly %>%
    filter(iso2 %in% get_eu_iso2s(include_eu = full_report),
      siec %in% COAL_MONTHLY_GAP_FUELS, unit == "THS_T")
  availability <- raw %>% filter(nrg_bal == "GID_CAL", lubridate::month(time) <= 6) %>%
    group_by(iso2, siec, year = lubridate::year(time)) %>%
    summarise(reported_months = sum(!is.na(values)), activity = sum(abs(values), na.rm = TRUE),
      .groups = "drop")
  availability <- tidyr::crossing(iso2 = get_eu_iso2s(include_eu = full_report),
    siec = COAL_MONTHLY_GAP_FUELS,
    year = years) %>% left_join(availability, by = c("iso2", "siec", "year")) %>%
    mutate(exclusion = case_when(
      is.na(reported_months) ~ "no_monthly_series",
      reported_months != 6 ~ "incomplete_reported_H1_truth",
      activity == 0 ~ "zero_activity_separate_cohort",
      TRUE ~ NA_character_
    ))
  write_csv(availability, file.path(output,
    paste0("coverage_", min(years), "_", max(years), ".csv")))
  baseline <- new.env(parent = asNamespace("creaco2tracker"))
  for (name in c("model_eurostat_solid.R", "model_eurostat.R",
    "model_eurostat_coal_annual_backed.R", "model_emissions.R",
    "model_project_orchestration.R", "model_detotalise.R",
    "core_helpers.R", "model_eu_tail_estimates.R")) {
    sys.source(file.path(root, "baseline_source", "R", name), envir = baseline)
  }
  # Conversion clients and proxies remain frozen for both implementations.
  baseline$iea.get_conversion_factors <- function(...) inputs$conversion
  stage <- function(m, a, cutoff, implementation = asNamespace("creaco2tracker"),
    forecast_method = "previous_year", background = NULL, selected_fuel = NULL,
    cutoff_context = NULL, consumption_only = FALSE, overrides = NULL,
    short_filled = NULL) {
    old <- identical(implementation, baseline)
    f <- function(name) get(name, implementation)
    if (!is.null(cutoff_context)) m <- bind_rows(m, cutoff_context)
    reported <- m
    m <- if (is.null(short_filled)) fill_raw_coal_monthly(m, a) else short_filled
    # Baseline forecasts enter after reported-input accounting. They cannot
    # become supply inputs, interpolation endpoints or historical profiles.
    if (!is.null(overrides)) {
      key <- function(x) do.call(paste, c(x[c("iso2", "siec", "unit", "nrg_bal", "time")],
        sep = "\r"))
      index <- match(key(m), key(overrides))
      value <- overrides$values[index]
      m$values[!is.na(value)] <- value[!is.na(value)]
    }
    m <- f("fill_raw_coal_annual_backed")(m, a)
    if (!is.null(selected_fuel)) {
      m <- m %>% filter(siec == selected_fuel)
      reported <- reported %>% filter(siec == selected_fuel)
    }
    monthly <- f("eurostat_split_solid_elec_others")(f("process_solid_monthly")(m, NULL))
    yearly <- f("eurostat_split_solid_elec_others")(f("process_solid_yearly")(a))
    attr(monthly, "coal_reported_monthly") <- f("eurostat_split_solid_elec_others")(
      f("process_solid_monthly")(reported, NULL))
    allocated <- f("apply_seasonal_adjustment")(yearly, monthly)
    cons <- f("resolve_coal_unallocated_totals")(
      f("combine_monthly_yearly_with_cutoff")(allocated, monthly)
    ) %>% select(-source) %>% filter(time <= cutoff)
    attr(cons, "coal_allocation") <- attr(allocated, "coal_allocation")
    if (consumption_only) return(cons)
    if (!old) {
      cons <- coal_prepare_total_forecasts(cons, cutoff, forecast_method = forecast_method)
      if (!is.null(background)) {
        background <- coal_prepare_total_forecasts(background, cutoff)
      }
    }
    if (!is.null(background)) {
      series_keys <- c("iso2", "siec", "unit")
      overlap <- semi_join(distinct(cons, across(all_of(series_keys))),
        distinct(background, across(all_of(series_keys))), by = series_keys)
      if (nrow(overlap) > 0L) stop("Held-out fuel series leaked into the background")
      allocation <- bind_rows(attr(cons, "coal_allocation"), attr(background, "coal_allocation"))
      separate <- bind_rows(attr(cons, "coal_separate_projection"),
        attr(background, "coal_separate_projection"))
      cons <- bind_rows(cons, background)
      attr(cons, "coal_allocation") <- allocation
      attr(cons, "coal_separate_projection") <- separate
    }
    converted <- f("get_co2_from_eurostat_cons")(
      cons, diagnostics_folder = NULL, use_cache = TRUE
    )
    result <- f("project_until_now")(
      converted,
      pwr_generation = inputs$power %>% filter(date <= cutoff),
      gas_demand = inputs$gas %>% filter(date <= cutoff),
      eurostat_indprod = inputs$industry %>% filter(time <= cutoff),
      date_to = cutoff
    )
    if (full_report) {
      result <- bind_rows(result, noncoal) %>%
        split_gas_to_elec_others() %>% recombine_fuels()
      result <- f("detotalise_co2")(result)
      result <- f("add_total_co2")(result)
      result <- f("stabilise_eu_tail_estimates")(result)
    } else {
      result <- add_total_co2(result)
    }
    result <- result %>% filter(date <= cutoff,
      iso2 %in% get_eu_iso2s(include_eu = full_report), estimate == "central")
    result %>% group_by(iso2, date, fuel, sector) %>%
      summarise(value = if (anyNA(value)) NA_real_ else sum(value), .groups = "drop")
  }
  patterns <- c("whole_year", "partial_h1", "annual_only", "missing_sector",
    "missing_coking", "forecast_total", "annual_only_forecast")
  requested <- option("--patterns", "all")
  if (requested != "all") patterns <- intersect(patterns, strsplit(requested, ",")[[1]])
  fuel_option <- option("--fuels", "all")
  selected_fuels <- if (fuel_option == "all") COAL_MONTHLY_GAP_FUELS else
    intersect(COAL_MONTHLY_GAP_FUELS, strsplit(fuel_option, ",", fixed = TRUE)[[1]])
  if (length(selected_fuels) == 0L) stop("No supported coal fuel was requested")
  definition_suffix <- if (fuel_option == "all") "" else
    paste0("_", paste(selected_fuels, collapse = "_"))
  definitions <- list()
  for (yr in years) {
    cutoff <- as.Date(sprintf("%d-06-30", yr))
    # The monthly sample expands from the common source start (2008), retaining
    # every earlier annual observation and never admitting a later observation.
    start <- min(raw$time)
    if (full_report) {
      path <- file.path(root, "noncoal_backgrounds", paste0(yr, "_projected.rds"))
      if (!file.exists(path)) stop("Capture the frozen non-coal background first: ", path)
      noncoal <- readRDS(path)
      stopifnot(all(noncoal$date <= cutoff), !any(noncoal$fuel == FUEL_COAL))
    }
    conversion_at_cutoff <- inputs$conversion %>% filter(year < yr)
    if (nrow(conversion_at_cutoff) == 0) stop("No fuel-quality inputs before cutoff year ", yr)
    replace_binding("iea.get_conversion_factors", function(...) conversion_at_cutoff)
    baseline$iea.get_conversion_factors <- function(...) conversion_at_cutoff
    m0 <- raw %>% filter(time >= start, time <= cutoff)
    # At an H1 reporting cutoff, a complete annual balance can only describe
    # an earlier calendar year. Revised history is allowed, future annual
    # quantities are not. Reconstruction tests mask that previous baseline;
    # forecast tests mask the current H1 without its annual balance.
    a0 <- annual %>% filter(lubridate::year(time) < yr)
    supported <- unique(process_conversion_factors(conversion_at_cutoff)$siec)
    unsupported <- setdiff(COAL_MONTHLY_GAP_FUELS, supported)
    blocked_countries <- bind_rows(
      m0 %>% filter(siec %in% unsupported, values > 0) %>% distinct(iso2),
      a0 %>% filter(siec %in% unsupported, nrg_bal == "IC_CAL", values > 0) %>% distinct(iso2)
    ) %>% distinct() %>% pull(iso2)
    write_csv(tibble(year = yr, siec = unsupported,
      reason = rep("no_pre_cutoff_fuel_quality", length(unsupported))),
      file.path(output, paste0(yr, "_conversion_exclusions.csv")))
    write_csv(tibble(year = yr, iso2 = blocked_countries,
      reason = rep("positive_fuel_without_pre_cutoff_quality", length(blocked_countries))),
      file.path(output, paste0(yr, "_country_exclusions.csv")))
    m0 <- m0 %>% filter(siec %in% supported, !iso2 %in% blocked_countries)
    a0 <- a0 %>% filter(siec %in% supported, !iso2 %in% blocked_countries)
    context_file <- file.path(output, paste0(yr, "_context.rds"))
    if (!file.exists(context_file)) save_atomic(
      stage(m0, a0, cutoff, consumption_only = TRUE), context_file)
    complete_context <- readRDS(context_file) %>%
      filter(siec %in% supported, !iso2 %in% blocked_countries)
    for (fuel in selected_fuels) {
      m <- m0 %>% filter(siec == .env$fuel)
      a <- a0 %>% filter(siec == .env$fuel)
      # Monthly truth must exist in both comparison periods. Annual-only
      # briquettes have no monthly truth and receive sensitivity, not scores.
      eligible <- m %>% filter(nrg_bal == "GID_CAL", lubridate::month(time) <= 6,
        lubridate::year(time) %in% c(yr - 1L, yr)) %>%
        group_by(iso2) %>% summarise(n = sum(!is.na(values)),
          activity = sum(abs(values), na.rm = TRUE), .groups = "drop") %>%
        filter(n == 12, activity > 0) %>% pull(iso2)
      if (length(eligible) == 0) next
      m <- m %>% filter(iso2 %in% eligible)
      a <- a %>% filter(iso2 %in% eligible)
      # All other fuels stay fixed through conversion and joint coal projection.
      # Real rows from those fuels also establish each country's source cutoff.
      background <- coal_replay_background(complete_context, fuel, eligible)
      # Unmasked fuels establish both ends of country source coverage. Keeping
      # only the last month would incorrectly make earlier annual years ineligible
      # when the selected fuel's entire monthly history is hidden.
      seeds <- m0 %>% filter(siec != .env$fuel, !is.na(values), iso2 %in% eligible) %>%
        group_by(iso2) %>% arrange(time, .by_group = TRUE) %>%
        slice(unique(c(1L, n()))) %>% ungroup()
      truth_file <- file.path(output, paste0(yr, "_", fuel, "_truth.rds"))
      if (!file.exists(truth_file)) save_atomic(stage(m, a, cutoff,
        background = background, selected_fuel = fuel, cutoff_context = seeds), truth_file)
      truth <- readRDS(truth_file)
      for (pattern in patterns) {
        if (pattern == "missing_coking" && fuel != SIEC_HARD_COAL) next
        id <- paste(yr, fuel, pattern, sep = "_")
        done <- file.path(output, paste0(id, "_scores.csv"))
        if (file.exists(done) && all(requested_methods %in%
          read_csv(done, show_col_types = FALSE)$method)) next
        message("REPLAY ", id)
        masked <- m
        # The actual baseline failure removes twelve months of the previous
        # year, while the reporting year's H1 remains available.
        masked_year <- if (pattern %in% c("forecast_total", "annual_only_forecast")) {
          yr
        } else yr - 1L
        target <- lubridate::year(masked$time) == masked_year
        hidden <- if (pattern == "missing_coking") masked$nrg_bal == "TI_CO" else
          if (pattern == "missing_sector") masked$nrg_bal == "TI_EHG_MAP" else
            rep(TRUE, nrow(masked))
        if (pattern %in% c("annual_only", "annual_only_forecast")) {
          # Reproduce absence of all usable monthly history, including profiles.
          target <- if (pattern == "annual_only") lubridate::year(masked$time) < yr else
            rep(TRUE, nrow(masked))
        }
        if (pattern == "partial_h1") hidden <- hidden & lubridate::month(masked$time) <= 3
        masked$values[target & hidden] <- NA_real_
        ay <- a
        if (pattern %in% c("forecast_total", "annual_only_forecast")) {
          ay <- ay %>% filter(lubridate::year(time) < yr)
        }
        if (pattern == "missing_sector") ay <- ay %>% filter(!(
          lubridate::year(time) == masked_year & nrg_bal %in% COAL_ANNUAL_POWER_BALANCES
        ))
        if (pattern == "missing_coking") ay <- ay %>% filter(!(
          lubridate::year(time) == masked_year & nrg_bal == "TI_CO_E"
        ))
        save_atomic(list(monthly = masked, annual = ay, cutoff = cutoff,
          cutoff_context = seeds, background_context_file = context_file,
          selected_fuel = fuel, eligible_countries = eligible),
          file.path(output, paste0(id, "_masked.rds")))
        short_filled <- fill_raw_coal_monthly(bind_rows(masked, seeds), ay)
        methods <- unique(requested_methods)
        outputs <- lapply(methods, function(method) {
          candidate <- masked
          if (method %in% c("previous_year", "three_year_average")) {
            count <- if (method == "previous_year") 1L else 3L
            keys <- c("iso2", "siec", "unit", "nrg_bal", "time")
            history <- masked %>% filter(!is.na(values))
            missing <- which(target & hidden & is.na(candidate$values) &
              candidate$nrg_bal %in% COAL_MONTHLY_GAP_BALANCES)
            dates <- candidate$time[missing]
            predictions <- lapply(seq_len(count), function(k) {
              query <- candidate[missing, keys]
              query$time <- dates %m-% lubridate::years(k)
              query %>% left_join(history %>% select(all_of(keys), values), by = keys) %>%
                pull(values)
            })
            candidate$values[missing] <- rowMeans(do.call(cbind, predictions))
            candidate <- candidate[missing, ]
          } else {
            candidate <- NULL
          }
          implementation <- if (method == "unchanged") baseline else asNamespace("creaco2tracker")
          prediction_file <- file.path(output, paste0(id, "_", method, ".rds"))
          prediction <- if (file.exists(prediction_file)) readRDS(prediction_file) else
            tryCatch(stage(masked, ay, cutoff, implementation,
            if (method == "three_year_average") "three_year_average" else "previous_year",
            background = background, selected_fuel = fuel, cutoff_context = seeds,
            overrides = candidate, short_filled = short_filled),
            error = function(error) {
              writeLines(conditionMessage(error),
                file.path(output, paste0(id, "_", method, "_failure.txt")))
              truth[0, ]
            })
          save_atomic(prediction, prediction_file)
          # Strict totals: an unavailable component is never an error of zero.
          totals <- function(data) data %>%
            filter(fuel == FUEL_TOTAL) %>%
            filter(lubridate::month(date) <= 6, lubridate::year(date) %in% c(yr - 1, yr)) %>%
            group_by(iso2, date) %>% summarise(
              value = if (anyNA(value)) NA_real_ else sum(value), .groups = "drop"
            ) %>% mutate(year = lubridate::year(date)) %>% group_by(iso2, year) %>%
            summarise(value = if (n() == 6 && !anyNA(value)) sum(value) else NA_real_,
              .groups = "drop")
          totals(truth) %>% filter(iso2 %in% eligible) %>% rename(actual = value) %>%
            left_join(totals(prediction) %>% rename(predicted = value), by = c("iso2", "year")) %>%
            group_by(iso2) %>% arrange(year) %>% summarise(
              level_error = predicted[year == yr] - actual[year == yr],
              yoy_error = diff(predicted) - diff(actual),
              activity = sum(abs(actual)), .groups = "drop"
            ) %>% mutate(method = method, evaluation_year = yr, siec = fuel, pattern = pattern)
        })
        write_csv(bind_rows(outputs), done, na = "")
        definitions[[length(definitions) + 1L]] <- tibble(
          holdout = id, year = yr, siec = fuel, pattern = pattern,
          countries = length(eligible), cutoff = cutoff,
          mask_start = min(masked$time[target & hidden]),
          mask_end = max(masked$time[target & hidden])
        )
        write_csv(bind_rows(definitions), file.path(output,
          paste0("holdout_definitions_", min(years), "_", max(years), definition_suffix, ".csv")))
        if (length(definitions) >= as.integer(option("--max-cases", "10000"))) {
          return(invisible(NULL))
        }
      }
    }
  }
  files <- list.files(output, pattern = "_scores.csv$", full.names = TRUE)
  scores <- bind_rows(lapply(files, read_csv, show_col_types = FALSE))
  write_csv(scores, file.path(output, "scores.csv"))
  selection <- bind_rows(lapply(2020:2024, function(yr) {
    scores %>% filter(evaluation_year < yr) %>% group_by(siec, pattern, method) %>%
      summarise(mae = mean(abs(yoy_error), na.rm = TRUE),
        coverage = sum(activity[!is.na(yoy_error)]) / sum(activity), .groups = "drop") %>%
      filter(is.finite(mae), coverage >= 0.70) %>% group_by(siec, pattern) %>%
      arrange(mae, method) %>% slice(1) %>% ungroup() %>% mutate(test_year = yr)
  }))
  write_csv(selection, file.path(output, "rolling_selection.csv"))
}
