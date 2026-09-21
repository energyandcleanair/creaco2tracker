# Coking is a required input, not a residual sector. Source conflicts are
# deliberately narrow: positive observations always supersede the conflict rule.
.coking_conflict <- function(iso2, time, value, frequency = "annual") {
  evidence <- get_coal_coking_conflicts()
  index <- match(
    paste(iso2, frequency),
    paste(evidence$iso2, evidence$frequency)
  )
  matched <- !is.na(index)
  matched & !is.na(value) &
    lubridate::year(time) >= evidence$year_from[index] &
    lubridate::year(time) <= evidence$year_to[index] &
    value == evidence$conflicting_value[index]
}

.coking_unique <- function(x) {
  if (length(x) == 1L && is.finite(x) && x >= 0) x else NA_real_
}

.coking_balance <- function(x, code) {
  if (!all(c("iso2", "time", "siec", "unit", "nrg_bal", "values") %in% names(x))) {
    return(tibble::tibble(iso2 = character(), time = as.Date(character()),
      value = double(), duplicate = logical()))
  }
  x %>% filter(siec == SIEC_HARD_COAL, unit == "THS_T", nrg_bal == code) %>%
    group_by(iso2, time) %>% summarise(
      value = .coking_unique(values), duplicate = n() != 1L, .groups = "drop"
    )
}

.coking_activity <- function(monthly, industry) {
  coke <- monthly %>% filter(siec == SIEC_COKE_OVEN_COKE,
    nrg_bal == "IPRD", unit == "THS_T") %>%
    transmute(iso2, time, proxy = "coke_activity", value = values)
  steel <- coke[0, ]
  if (all(c("iso2", "time", "nace_r2", "unit", "s_adj", "values") %in% names(industry))) {
    if ("indic_bt" %in% names(industry)) industry <- filter(industry, indic_bt == "PRD")
    if ("freq" %in% names(industry)) industry <- filter(industry, freq == "M")
    steel <- industry %>% filter(nace_r2 == "C241", s_adj == "NSA",
      grepl("^I[0-9]+$", unit)) %>%
      # The cache can contain overlapping index vintages (I15 and I21). Use
      # one base per country throughout calibration and prediction.
      mutate(index_base = as.integer(sub("^I", "", unit))) %>%
      group_by(iso2) %>% filter(index_base == max(index_base)) %>% ungroup() %>%
      transmute(iso2, time, proxy = "steel_activity", value = values)
  }
  bind_rows(coke, steel) %>% group_by(iso2, time, proxy) %>%
    summarise(value = .coking_unique(value), .groups = "drop")
}

# Only original observations earlier than cutoff may calibrate a prediction.
.coking_candidates <- function(history, dates, activity, cutoff, annual_history = NULL) {
  history <- history %>% filter(time < cutoff, is.finite(value), !conflict, !duplicate)
  out <- tibble::tibble(time = as.Date(dates), previous_year = NA_real_,
    coke_activity = NA_real_, steel_activity = NA_real_)
  previous <- lubridate::`%m-%`(out$time, lubridate::years(1))
  out$previous_year <- history$value[match(previous, history$time)]
  for (proxy_name in c("coke_activity", "steel_activity")) {
    a <- activity %>% filter(proxy == proxy_name)
    paired <- history %>% inner_join(a %>% select(time, activity = value), by = "time") %>%
      filter(is.finite(activity), activity > 0) %>% arrange(desc(time)) %>% slice_head(n = 36)
    # Six original monthly pairs are the minimum evidence for an activity ratio.
    if (nrow(paired) >= 6L) {
      intensity <- sum(paired$value) / sum(paired$activity)
    } else {
      # Annual-only systems calibrate against original complete annual inputs.
      # Holdout scoring does not supply these containing totals.
      if (is.null(annual_history) || nrow(annual_history) == 0L) next
      annual_activity <- a %>% mutate(year = lubridate::year(time)) %>%
        group_by(year) %>% summarise(activity = if (n() == 12L) sum(value) else NA_real_,
          .groups = "drop")
      annual_pairs <- annual_history %>% filter(time < cutoff, !conflict, !duplicate,
        is.finite(value)) %>% mutate(year = lubridate::year(time)) %>%
        inner_join(annual_activity, by = "year") %>%
        filter(is.finite(activity), activity > 0) %>% arrange(desc(time)) %>% slice_head(n = 3)
      if (nrow(annual_pairs) < 2L) next
      intensity <- sum(annual_pairs$value) / sum(annual_pairs$activity)
    }
    out[[proxy_name]] <- intensity * a$value[match(out$time, a$time)]
  }
  out
}

# Six-month holdouts include both recent and three-year-old training cutoffs.
# EU/annual values never enter these folds, so masked targets cannot leak through
# a containing total. Scores compare candidates on the same fully observed folds.
.coking_scores <- function(history, activity, cutoff) {
  methods <- c("coke_activity", "steel_activity", "previous_year")
  observed <- history %>% filter(time < cutoff, is.finite(value), !conflict, !duplicate)
  years <- sort(unique(lubridate::year(observed$time)))
  scores <- list()
  for (year in tail(years, 8)) {
    for (start_month in c(1L, 7L)) {
      start <- as.Date(sprintf("%04d-%02d-01", year, start_month))
      dates <- seq(start, by = "month", length.out = 6)
      truth <- observed$value[match(dates, observed$time)]
      if (anyNA(truth) || max(dates) >= cutoff) next
      for (lag_years in c(0L, 3L)) {
        train_end <- lubridate::`%m-%`(start, lubridate::years(lag_years))
        candidates <- .coking_candidates(history, dates, activity, train_end)
        for (method in methods) {
          prediction <- candidates[[method]]
          if (any(!is.finite(prediction))) next
          scores[[length(scores) + 1L]] <- tibble::tibble(method = method,
            start = start, horizon = if (lag_years == 0L) "short" else "long",
            total_error = abs(sum(prediction) - sum(truth)),
            monthly_error = mean(abs(prediction - truth)),
            signed_error = sum(prediction - truth),
            truth_total = sum(truth), prediction_total = sum(prediction))
        }
      }
    }
  }
  bind_rows(scores)
}

.coking_choose <- function(candidates, scores, horizon) {
  methods <- c("coke_activity", "steel_activity", "previous_year")
  available <- methods[vapply(methods, function(m) all(is.finite(candidates[[m]])), logical(1))]
  if (length(available) == 0L) {
    return(list(method = "unresolved", values = rep(NA_real_, nrow(candidates)),
      evidence = "no_usable_history_or_activity"))
  }
  eligible <- tibble::tibble()
  if (nrow(scores) > 0L) {
    shared <- scores %>% filter(.data$horizon == .env$horizon, method %in% available) %>%
      group_by(start) %>% filter(n_distinct(method) == length(available)) %>% ungroup()
    eligible <- shared %>% group_by(method) %>% summarise(folds = n(),
      total_error = mean(total_error), monthly_error = mean(monthly_error), .groups = "drop") %>%
      filter(folds >= 3L) %>% arrange(total_error, monthly_error, match(method, methods))
  }
  chosen <- if (nrow(eligible)) eligible$method[[1]] else available[[1]]
  list(method = chosen, values = candidates[[chosen]],
    evidence = if (nrow(eligible)) "rolling_holdout_selected" else "insufficient_holdouts_fallback")
}

.coking_apply <- function(x, resolved, code, frequency) {
  if (nrow(resolved) == 0L) return(x)
  # Duplicate observations are deliberately left duplicated; strict accounting
  # rejects them rather than silently choosing one of the source values.
  replacement <- resolved %>% select(iso2, time, resolved_value)
  target <- x$siec == SIEC_HARD_COAL & x$unit == "THS_T" & x$nrg_bal == code
  index <- match(paste(x$iso2, x$time), paste(replacement$iso2, replacement$time))
  changed <- target & !is.na(index)
  x$values[changed] <- replacement$resolved_value[index[changed]]
  absent <- resolved %>% anti_join(x %>% filter(target) %>% select(iso2, time),
    by = c("iso2", "time"))
  if (nrow(absent)) {
    rows <- x[rep(NA_integer_, nrow(absent)), , drop = FALSE]
    rows$iso2 <- absent$iso2
    rows$time <- absent$time
    rows$siec <- SIEC_HARD_COAL
    rows$unit <- "THS_T"
    rows$nrg_bal <- code
    rows$values <- absent$resolved_value
    if ("freq" %in% names(rows)) rows$freq <- frequency
    if ("geo" %in% names(rows)) {
      rows$geo <- ifelse(rows$iso2 == "EU", "EU27_2020", rows$iso2)
    }
    x <- bind_rows(x, rows)
  }
  x
}

.coking_eu <- function(resolved, original, frequency) {
  members <- get_eu_iso2s()
  # Establish omissions using ORIGINAL complete member coverage, excluding FR.
  checks <- original %>% filter(iso2 %in% setdiff(members, "FR")) %>%
    group_by(time) %>% summarise(n = sum(is.finite(value) & !duplicate),
      subtotal = sum(value), .groups = "drop") %>%
    inner_join(original %>% filter(iso2 == "EU") %>% select(time, eu = value), by = "time") %>%
    filter(n == length(members) - 1L, is.finite(eu),
      abs(eu - subtotal) <= pmax(0.001, abs(eu) * 1e-6))
  for (index in which(resolved$iso2 == "EU")) {
    date <- resolved$time[[index]]
    countries <- resolved %>% filter(iso2 %in% members, time == date)
    complete <- nrow(countries) == length(members) && all(is.finite(countries$resolved_value)) &&
      !any(countries$duplicate)
    fr <- countries %>% filter(iso2 == "FR")
    prior_checks <- checks %>% filter(time <= date,
      time >= lubridate::`%m-%`(date, lubridate::years(2)))
    verified <- date >= as.Date("2021-01-01") &&
      (date %in% checks$time || (frequency == "monthly" && nrow(prior_checks) >= 3L))
    if (resolved$duplicate[[index]]) next
    if (!is.finite(resolved$original_value[[index]]) && complete) {
      resolved$resolved_value[[index]] <- sum(countries$resolved_value)
      resolved$method[[index]] <- "resolved_member_sum"
      resolved$evidence[[index]] <- "all_members_resolved"
    } else if (is.finite(resolved$original_value[[index]]) && verified && nrow(fr) == 1L &&
      is.finite(fr$resolved_value) && !fr$duplicate) {
      resolved$resolved_value[[index]] <- resolved$original_value[[index]] + fr$resolved_value
      resolved$method[[index]] <- "verified_omitted_member"
      resolved$evidence[[index]] <- "reported_eu_equals_members_without_france"
      resolved$conflict[[index]] <- TRUE
    }
  }
  resolved
}

# Input and resolution-state construction ----------------------------------

.coking_prepare_inputs <- function(monthly, annual) {
  previous_monthly <- attr(monthly, "coal_coking_original")
  if (!is.null(previous_monthly)) monthly <- previous_monthly
  previous_annual <- attr(annual, "coal_coking_original")
  if (!is.null(previous_annual)) annual <- previous_annual

  source_monthly <- monthly
  provenance <- attr(monthly, "coal_gap_provenance")
  if (!is.null(provenance) && nrow(provenance)) {
    original <- provenance %>%
      filter(siec == SIEC_HARD_COAL, nrg_bal == "TI_CO", unit == "THS_T")
    index <- match(
      paste(monthly$iso2, monthly$time),
      paste(original$iso2, original$time)
    )
    replace <- monthly$siec == SIEC_HARD_COAL &
      monthly$nrg_bal == "TI_CO" &
      monthly$unit == "THS_T" &
      !is.na(index)
    monthly$values[replace] <- original$original_value[index[replace]]
  }

  list(
    monthly = monthly,
    annual = annual,
    source_monthly = source_monthly,
    source_annual = annual
  )
}

.coking_observations <- function(monthly, annual) {
  list(
    monthly = .coking_balance(monthly, "TI_CO") %>%
      mutate(conflict = .coking_conflict(iso2, time, value, "monthly")),
    annual = .coking_balance(annual, "TI_CO_E") %>%
      mutate(conflict = .coking_conflict(iso2, time, value))
  )
}

.coking_add_eu_keys <- function(keys) {
  members <- get_eu_iso2s()
  eu_keys <- keys %>%
    filter(iso2 %in% members) %>%
    count(time) %>%
    filter(n == length(members)) %>%
    transmute(iso2 = "EU", time)
  bind_rows(keys, eu_keys) %>% distinct()
}

.coking_initialise_state <- function(keys, observations) {
  keys %>%
    left_join(observations, by = c("iso2", "time")) %>%
    mutate(
      duplicate = coalesce(duplicate, FALSE),
      conflict = coalesce(conflict, FALSE),
      original_value = value,
      resolved_value = if_else(conflict | duplicate, NA_real_, value),
      method = if_else(is.finite(resolved_value), "reported", "unresolved"),
      evidence = if_else(
        conflict,
        "documented_french_reporting_break",
        "original_source"
      ),
      bounded = FALSE,
      constraint = "none",
      training_start = as.Date(NA),
      training_end = as.Date(NA)
    ) %>%
    select(-value)
}

.coking_resolution_states <- function(monthly, annual, observations) {
  monthly_keys <- monthly %>%
    filter(
      siec == SIEC_HARD_COAL,
      unit == "THS_T",
      nrg_bal %in% COAL_MONTHLY_GAP_BALANCES
    ) %>%
    distinct(iso2, time)
  if (nrow(monthly_keys)) {
    monthly_keys <- monthly_keys %>%
      group_by(iso2) %>%
      reframe(time = seq(min(time), max(time), by = "month"))
  }

  annual_keys <- annual %>%
    filter(
      siec == SIEC_HARD_COAL,
      unit == "THS_T",
      nrg_bal %in% c("FC_E", "TI_E", "TI_CO_E")
    ) %>%
    distinct(iso2, time)

  list(
    monthly = .coking_initialise_state(
      .coking_add_eu_keys(monthly_keys),
      observations$monthly
    ),
    annual = .coking_initialise_state(
      .coking_add_eu_keys(annual_keys),
      observations$annual
    )
  )
}

# Country-year estimation --------------------------------------------------

.coking_select_predictions <- function(candidates, scores, horizon, requested) {
  choice <- .coking_choose(candidates[requested, ], scores, horizon)
  prediction <- if (choice$method != "unresolved") {
    candidates[[choice$method]]
  } else {
    rep(NA_real_, nrow(candidates))
  }
  methods <- rep(choice$method, nrow(candidates))
  evidence <- rep(choice$evidence, nrow(candidates))

  # A proxy gap in one month must not discard evidence for another month.
  if (choice$method == "unresolved") {
    for (month in requested) {
      partial <- .coking_choose(candidates[month, ], scores, horizon)
      prediction[month] <- partial$values
      methods[month] <- partial$method
      evidence[month] <- partial$evidence
    }
  }

  list(
    choice = choice,
    values = prediction,
    methods = methods,
    evidence = evidence
  )
}

.coking_monthly_observed_values <- function(observed, country, dates) {
  index <- match(
    paste(country, dates),
    paste(observed$iso2, observed$time)
  )
  values <- observed$value[index]
  conflicts <- observed$conflict[index]
  values[!is.na(conflicts) & conflicts] <- NA_real_
  values
}

.coking_residual_weights <- function(prediction, activity, dates) {
  if (all(is.finite(prediction)) && sum(prediction) > 0) return(prediction)

  for (proxy_name in c("coke_activity", "steel_activity")) {
    proxy <- activity %>% filter(proxy == proxy_name)
    weights <- proxy$value[match(dates, proxy$time)]
    if (all(is.finite(weights)) && sum(weights) > 0) return(weights)
  }
  rep(1, length(dates))
}

.coking_apply_annual_residual <- function(
  prediction,
  methods,
  evidence,
  choice,
  known,
  annual_row,
  activity,
  dates
) {
  if (nrow(annual_row) != 1L || !anyNA(known)) {
    return(list(
      values = prediction,
      methods = methods,
      evidence = evidence,
      choice = choice
    ))
  }

  remaining <- annual_row$value - sum(known, na.rm = TRUE)
  weights <- .coking_residual_weights(prediction, activity, dates)
  missing <- is.na(known)
  if (remaining >= 0 && sum(weights[missing]) > 0) {
    prediction[missing] <- remaining * weights[missing] / sum(weights[missing])
    choice$method <- "annual_residual"
    choice$evidence <- if (all(weights == 1)) {
      "reported_annual_uniform_fallback"
    } else {
      "reported_annual_activity_profile"
    }
    methods[] <- choice$method
    evidence[] <- choice$evidence
  }

  list(
    values = prediction,
    methods = methods,
    evidence = evidence,
    choice = choice
  )
}

.coking_record_training <- function(
  state,
  missing,
  start,
  history,
  activity,
  annual_history
) {
  valid_history <- history %>%
    filter(time < start, is.finite(value), !conflict, !duplicate)
  for (selected in unique(state$method[missing])) {
    supported <- missing[state$method[missing] == selected]
    if (selected == "unresolved") next
    if (selected == "annual_residual") {
      state$training_start[supported] <- start
      state$training_end[supported] <- as.Date(paste0(lubridate::year(start), "-12-01"))
      next
    }

    training <- valid_history
    if (nrow(training)) {
      if (selected %in% c("coke_activity", "steel_activity")) {
        training <- training %>%
          inner_join(
            activity %>%
              filter(proxy == selected, is.finite(value), value > 0) %>%
              select(time),
            by = "time"
          ) %>%
          arrange(desc(time)) %>%
          slice_head(n = 36)
      } else if (selected == "previous_year") {
        previous_dates <- lubridate::`%m-%`(
          state$time[supported],
          lubridate::years(1)
        )
        training <- filter(training, time %in% previous_dates)
      }
      if (nrow(training)) {
        state$training_start[supported] <- min(training$time)
        state$training_end[supported] <- max(training$time)
      }
      next
    }

    annual_training <- annual_history %>%
      filter(time < start, is.finite(value), !conflict, !duplicate) %>%
      arrange(desc(time)) %>%
      slice_head(n = 3)
    if (nrow(annual_training) && selected %in% c("coke_activity", "steel_activity")) {
      state$training_start[supported] <- min(annual_training$time)
      state$training_end[supported] <- max(annual_training$time)
      state$evidence[supported] <- paste0(
        state$evidence[supported],
        "_annual_calibration"
      )
    }
  }
  state
}

.coking_resolve_annual <- function(
  state,
  indices,
  required,
  prediction,
  known,
  choice,
  annual_observed,
  country,
  start
) {
  if (!required) return(state)

  complete_year <- prediction
  complete_year[!is.na(known)] <- known[!is.na(known)]
  if (all(is.finite(complete_year))) {
    state$resolved_value[indices] <- sum(complete_year)
    state$method[indices] <- "resolved_monthly_sum"
    state$evidence[indices] <- if (choice$method == "unresolved") {
      "mixed_monthly_candidates"
    } else {
      choice$evidence
    }
    return(state)
  }

  past <- annual_observed %>%
    filter(
      iso2 == country,
      time < start,
      !conflict,
      !duplicate,
      is.finite(value)
    ) %>%
    arrange(desc(time))
  previous_year <- as.Date(paste0(lubridate::year(start) - 1L, "-01-01"))
  if (nrow(past) && past$time[[1]] == previous_year) {
    state$resolved_value[indices] <- past$value[[1]]
    state$method[indices] <- "previous_year_annual"
    state$evidence[indices] <- "insufficient_monthly_evidence_fallback"
  }
  state
}

.coking_resolve_year <- function(
  monthly_state,
  annual_state,
  observed,
  annual_observed,
  activity,
  country,
  year
) {
  start <- as.Date(paste0(year, "-01-01"))
  dates <- seq(start, by = "month", length.out = 12)
  monthly_indices <- which(monthly_state$iso2 == country & monthly_state$time %in% dates)
  annual_indices <- which(annual_state$iso2 == country & annual_state$time == start)
  monthly_missing <- monthly_indices[
    !is.finite(monthly_state$resolved_value[monthly_indices]) &
      !monthly_state$duplicate[monthly_indices]
  ]
  annual_required <- length(annual_indices) &&
    !is.finite(annual_state$resolved_value[annual_indices]) &&
    !annual_state$duplicate[annual_indices]
  if (!length(monthly_missing) && !annual_required) {
    return(list(monthly = monthly_state, annual = annual_state, validation = NULL))
  }

  annual_history <- annual_observed %>% filter(iso2 == country)
  candidates <- .coking_candidates(observed, dates, activity, start, annual_history)
  scores <- .coking_scores(observed, activity, start)
  validation <- if (nrow(scores)) {
    mutate(scores, iso2 = country, target_year = year)
  } else {
    NULL
  }
  valid_history <- observed %>%
    filter(time < start, is.finite(value), !conflict, !duplicate)
  recent <- nrow(valid_history) &&
    max(valid_history$time) >= lubridate::`%m-%`(start, lubridate::years(1))
  horizon <- if (recent) "short" else "long"
  requested <- if (length(monthly_missing)) {
    match(monthly_state$time[monthly_missing], candidates$time)
  } else {
    seq_len(12)
  }

  prediction <- .coking_select_predictions(candidates, scores, horizon, requested)
  known <- .coking_monthly_observed_values(observed, country, dates)
  annual_row <- annual_observed %>%
    filter(iso2 == country, time == start, !conflict, !duplicate, is.finite(value))
  prediction <- .coking_apply_annual_residual(
    prediction = prediction$values,
    methods = prediction$methods,
    evidence = prediction$evidence,
    choice = prediction$choice,
    known = known,
    annual_row = annual_row,
    activity = activity,
    dates = dates
  )

  monthly_state$resolved_value[monthly_missing] <- prediction$values[
    match(monthly_state$time[monthly_missing], dates)
  ]
  monthly_state$method[monthly_missing] <- prediction$methods[
    match(monthly_state$time[monthly_missing], dates)
  ]
  monthly_state$evidence[monthly_missing] <- prediction$evidence[
    match(monthly_state$time[monthly_missing], dates)
  ]
  monthly_state <- .coking_record_training(
    monthly_state,
    monthly_missing,
    start,
    observed,
    activity,
    annual_history
  )
  annual_state <- .coking_resolve_annual(
    annual_state,
    annual_indices,
    annual_required,
    prediction$values,
    known,
    prediction$choice,
    annual_observed,
    country,
    start
  )

  list(monthly = monthly_state, annual = annual_state, validation = validation)
}

.coking_resolve_countries <- function(states, observations, activity) {
  validation <- list()
  countries <- setdiff(unique(c(states$monthly$iso2, states$annual$iso2)), "EU")
  for (country in countries) {
    monthly_observed <- observations$monthly %>% filter(iso2 == country)
    annual_observed <- observations$annual %>% filter(iso2 == country)
    country_activity <- activity %>% filter(iso2 == country)
    years <- sort(unique(lubridate::year(c(
      states$monthly$time[states$monthly$iso2 == country],
      states$annual$time[states$annual$iso2 == country]
    ))))
    for (year in years) {
      resolved <- .coking_resolve_year(
        states$monthly,
        states$annual,
        monthly_observed,
        annual_observed,
        country_activity,
        country,
        year
      )
      states$monthly <- resolved$monthly
      states$annual <- resolved$annual
      if (!is.null(resolved$validation)) {
        validation[[length(validation) + 1L]] <- resolved$validation
      }
    }
  }
  states$validation <- bind_rows(validation)
  states
}

# Accounting constraints and output ---------------------------------------

.coking_bound_estimates <- function(resolved, raw, frequency) {
  total_code <- if (frequency == "monthly") "GID_CAL" else "TI_E"
  totals <- .coking_balance(raw, total_code)
  maximum <- totals$value[match(
    paste(resolved$iso2, resolved$time),
    paste(totals$iso2, totals$time)
  )]
  if (frequency == "monthly") {
    power <- .coking_balance(raw, "TI_EHG_MAP")
    electricity <- power$value[match(
      paste(resolved$iso2, resolved$time),
      paste(power$iso2, power$time)
    )]
    maximum <- pmin(
      maximum,
      (maximum - coalesce(electricity, 0)) / (1 - HARDCOAL_COKING_RATE_FACTOR)
    )
  }

  estimated <- resolved$method != "reported" & is.finite(resolved$resolved_value)
  reported_conflict <- resolved$method == "reported" &
    is.finite(maximum) &
    is.finite(resolved$resolved_value) &
    resolved$resolved_value > pmax(0, maximum)
  resolved$constraint[reported_conflict] <- "reported_accounting_conflict"

  exceeded <- estimated &
    is.finite(maximum) &
    resolved$resolved_value > pmax(0, maximum)
  resolved$bounded[exceeded] <- TRUE
  resolved$constraint[exceeded] <- "accounting_bound"

  # A zero/partial annual transformation total cannot erase positive monthly
  # coking observations. Preserve their sum and expose the conflict instead.
  derived <- exceeded &
    frequency == "annual" &
    resolved$method == "resolved_monthly_sum"
  resolved$bounded[derived] <- FALSE
  resolved$constraint[derived] <- "annual_transformation_conflict"

  # Annual anchors that violate the bound remain unresolved.
  constrained <- exceeded & resolved$method == "annual_residual"
  resolved$resolved_value[exceeded & !derived] <- pmax(0, maximum[exceeded & !derived])
  resolved$resolved_value[constrained] <- NA_real_
  resolved$evidence[constrained] <- "annual_residual_exceeds_monthly_bound"
  resolved
}

.coking_recompute_annual_sums <- function(annual, monthly) {
  for (index in which(annual$method == "resolved_monthly_sum")) {
    rows <- monthly %>%
      filter(
        iso2 == annual$iso2[[index]],
        lubridate::year(time) == lubridate::year(annual$time[[index]])
      )
    if (nrow(rows) == 12L) annual$resolved_value[[index]] <- sum(rows$resolved_value)
  }
  annual
}

.coking_diagnostics <- function(monthly, annual) {
  conflict_evidence <- get_coal_coking_conflicts()
  annual_check <- monthly %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(iso2, year) %>%
    summarise(
      monthly_total = if (n() == 12L) sum(resolved_value) else NA_real_,
      .groups = "drop"
    ) %>%
    inner_join(
      annual %>%
        transmute(
          iso2,
          year = lubridate::year(time),
          annual_total = resolved_value
        ),
      by = c("iso2", "year")
    ) %>%
    mutate(annual_difference = monthly_total - annual_total)

  bind_rows(
    mutate(monthly, frequency = "monthly"),
    mutate(annual, frequency = "annual")
  ) %>%
    mutate(
      siec = SIEC_HARD_COAL,
      unit = "THS_T",
      deduction = (1 - HARDCOAL_COKING_RATE_FACTOR) * resolved_value,
      status = case_when(
        !is.finite(resolved_value) ~ "unresolved",
        method == "reported" ~ "reported",
        TRUE ~ "estimated"
      )
    ) %>%
    left_join(
      conflict_evidence %>% select(iso2, frequency, conflict_source = source),
      by = c("iso2", "frequency")
    ) %>%
    mutate(conflict_source = if_else(conflict, conflict_source, NA_character_)) %>%
    mutate(year = lubridate::year(time)) %>%
    left_join(
      annual_check %>% select(iso2, year, annual_difference),
      by = c("iso2", "year")
    )
}

.coking_attach_result <- function(source, resolved, diagnostics, code, frequency) {
  result <- .coking_apply(source, resolved, code, frequency)
  attr(result, "coal_coking_original") <- source
  attr(result, "coal_coking_resolved") <- TRUE
  attr(result, "coal_coking_provenance") <- diagnostics
  result
}

#' Resolve coking before coal combustion accounting
#'
#' Original measurements, estimates and evidenced source conflicts remain
#' distinguishable. Annual constraints apply only to trustworthy observations.
#' @keywords internal
#' @noRd
.resolve_coal_coking <- function(monthly, annual, industry = tibble::tibble()) {
  inputs <- .coking_prepare_inputs(monthly, annual)
  observations <- .coking_observations(inputs$monthly, inputs$annual)
  activity <- .coking_activity(inputs$monthly, industry)
  states <- .coking_resolution_states(
    inputs$monthly,
    inputs$annual,
    observations
  )
  states <- .coking_resolve_countries(states, observations, activity)

  states$monthly <- .coking_bound_estimates(states$monthly, inputs$monthly, "monthly")
  states$annual <- .coking_recompute_annual_sums(states$annual, states$monthly)
  states$annual <- .coking_bound_estimates(states$annual, inputs$annual, "annual")
  states$monthly <- .coking_eu(states$monthly, observations$monthly, "monthly")
  states$annual <- .coking_eu(states$annual, observations$annual, "annual")

  diagnostics <- .coking_diagnostics(states$monthly, states$annual)
  list(
    monthly = .coking_attach_result(
      inputs$source_monthly,
      states$monthly,
      diagnostics,
      "TI_CO",
      "M"
    ),
    yearly = .coking_attach_result(
      inputs$source_annual,
      states$annual,
      diagnostics,
      "TI_CO_E",
      "A"
    ),
    diagnostics = diagnostics,
    validation = states$validation
  )
}
