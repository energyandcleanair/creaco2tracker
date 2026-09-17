# Coking is a required input, not a residual sector. These source conflicts are
# deliberately narrow: positive observations always supersede the conflict rule.
# SDES documents continued operation of the French coke ovens in 2023 and 2024:
# https://www.statistiques.developpement-durable.gouv.fr/media/9074/download?inline=
# https://www.statistiques.developpement-durable.gouv.fr/media/9087/download?inline=
.coking_conflict <- function(iso2, time, value, frequency = "annual") {
  frequency == "annual" & iso2 == "FR" & lubridate::year(time) %in% 2021:2024 &
    !is.na(value) & value == 0
}

.coking_source <- function() {
  "https://www.statistiques.developpement-durable.gouv.fr/media/9074/download?inline="
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

#' Resolve coking before coal combustion accounting
#'
#' Original measurements, estimates and evidenced source conflicts remain
#' distinguishable. Annual constraints apply only to trustworthy observations.
#' @keywords internal
#' @noRd
.resolve_coal_coking <- function(monthly, annual, industry = tibble::tibble()) {
  previous <- attr(monthly, "coal_coking_original")
  if (!is.null(previous)) monthly <- previous
  previous <- attr(annual, "coal_coking_original")
  if (!is.null(previous)) annual <- previous
  source_monthly <- monthly
  source_annual <- annual
  provenance <- attr(monthly, "coal_gap_provenance")
  if (!is.null(provenance) && nrow(provenance)) {
    original <- provenance %>% filter(siec == SIEC_HARD_COAL, nrg_bal == "TI_CO",
      unit == "THS_T")
    idx <- match(paste(monthly$iso2, monthly$time), paste(original$iso2, original$time))
    replace <- monthly$siec == SIEC_HARD_COAL & monthly$nrg_bal == "TI_CO" &
      monthly$unit == "THS_T" & !is.na(idx)
    monthly$values[replace] <- original$original_value[idx[replace]]
  }
  observed <- .coking_balance(monthly, "TI_CO") %>%
    mutate(conflict = .coking_conflict(iso2, time, value, "monthly"))
  annual_observed <- .coking_balance(annual, "TI_CO_E") %>%
    mutate(conflict = .coking_conflict(iso2, time, value))
  activity <- .coking_activity(monthly, industry)
  monthly_keys <- monthly %>% filter(siec == SIEC_HARD_COAL, unit == "THS_T",
    nrg_bal %in% COAL_MONTHLY_GAP_BALANCES) %>% distinct(iso2, time)
  if (nrow(monthly_keys)) monthly_keys <- monthly_keys %>% group_by(iso2) %>%
    reframe(time = seq(min(time), max(time), by = "month"))
  annual_keys <- annual %>% filter(siec == SIEC_HARD_COAL, unit == "THS_T",
    nrg_bal %in% c("FC_E", "TI_E", "TI_CO_E")) %>% distinct(iso2, time)
  initialise <- function(keys, observations) keys %>% left_join(observations,
    by = c("iso2", "time")) %>% mutate(
      duplicate = coalesce(duplicate, FALSE), conflict = coalesce(conflict, FALSE),
      original_value = value,
      resolved_value = if_else(conflict | duplicate, NA_real_, value),
      method = if_else(is.finite(resolved_value), "reported", "unresolved"),
      evidence = if_else(conflict, "documented_french_reporting_break", "original_source"),
      bounded = FALSE, constraint = "none",
      training_start = as.Date(NA), training_end = as.Date(NA)
    ) %>% select(-value)
  # EU coking keys must exist even when Eurostat omits the complete EU row.
  eu_keys <- function(keys) keys %>% filter(iso2 %in% get_eu_iso2s()) %>%
    count(time) %>% filter(n == length(get_eu_iso2s())) %>%
    transmute(iso2 = "EU", time)
  monthly_keys <- bind_rows(monthly_keys, eu_keys(monthly_keys)) %>% distinct()
  annual_keys <- bind_rows(annual_keys, eu_keys(annual_keys)) %>% distinct()
  m <- initialise(monthly_keys, observed)
  a <- initialise(annual_keys, annual_observed)
  validation <- list()
  for (country in setdiff(unique(c(m$iso2, a$iso2)), "EU")) {
    history <- observed %>% filter(iso2 == country)
    act <- activity %>% filter(iso2 == country)
    years <- sort(unique(lubridate::year(c(m$time[m$iso2 == country],
      a$time[a$iso2 == country]))))
    for (year in years) {
      start <- as.Date(paste0(year, "-01-01"))
      dates <- seq(start, by = "month", length.out = 12)
      mi <- which(m$iso2 == country & m$time %in% dates)
      ai <- which(a$iso2 == country & a$time == start)
      need_monthly <- length(mi) && any(!is.finite(m$resolved_value[mi]) & !m$duplicate[mi])
      need_annual <- length(ai) && !is.finite(a$resolved_value[ai]) && !a$duplicate[ai]
      if (!need_monthly && !need_annual) next
      annual_history <- annual_observed %>% filter(iso2 == country)
      candidates <- .coking_candidates(history, dates, act, start, annual_history)
      scores <- .coking_scores(history, act, start)
      if (nrow(scores)) validation[[length(validation) + 1L]] <-
        mutate(scores, iso2 = country, target_year = year)
      valid_history <- history %>% filter(time < start, is.finite(value), !conflict, !duplicate)
      recent <- nrow(valid_history) && max(valid_history$time) >=
        lubridate::`%m-%`(start, lubridate::years(1))
      horizon <- if (recent) "short" else "long"
      # Choose on the requested months; future proxy availability cannot affect
      # which method is selected for the observed portion of a current year.
      missing_months <- mi[!is.finite(m$resolved_value[mi]) & !m$duplicate[mi]]
      requested <- if (length(missing_months)) match(m$time[missing_months], candidates$time) else
        seq_len(12)
      choice <- .coking_choose(candidates[requested, ], scores, horizon)
      prediction <- if (choice$method != "unresolved") candidates[[choice$method]] else
        rep(NA_real_, 12)
      methods <- rep(choice$method, 12)
      evidence <- rep(choice$evidence, 12)
      # A proxy gap in one month must not discard evidence for another month.
      # If no candidate covers the whole gap, select on each available subset.
      if (choice$method == "unresolved") {
        for (month in requested) {
          partial <- .coking_choose(candidates[month, ], scores, horizon)
          prediction[month] <- partial$values
          methods[month] <- partial$method
          evidence[month] <- partial$evidence
        }
      }
      annual_row <- annual_observed %>% filter(iso2 == country, time == start,
        !conflict, !duplicate, is.finite(value))
      known <- observed$value[match(paste(country, dates), paste(observed$iso2, observed$time))]
      bad <- observed$conflict[match(paste(country, dates), paste(observed$iso2, observed$time))]
      known[!is.na(bad) & bad] <- NA_real_
      if (nrow(annual_row) == 1L && anyNA(known)) {
        remaining <- annual_row$value - sum(known, na.rm = TRUE)
        weights <- prediction
        if (any(!is.finite(weights)) || sum(weights, na.rm = TRUE) <= 0) {
          weights <- act %>% filter(proxy == "coke_activity")
          weights <- weights$value[match(dates, weights$time)]
        }
        if (any(!is.finite(weights)) || sum(weights, na.rm = TRUE) <= 0) {
          steel <- act %>% filter(proxy == "steel_activity")
          weights <- steel$value[match(dates, steel$time)]
        }
        if (any(!is.finite(weights)) || sum(weights, na.rm = TRUE) <= 0) weights <- rep(1, 12)
        if (remaining >= 0 && sum(weights[is.na(known)]) > 0) {
          prediction[is.na(known)] <- remaining * weights[is.na(known)] /
            sum(weights[is.na(known)])
          choice$method <- "annual_residual"
          choice$evidence <- if (all(weights == 1)) "reported_annual_uniform_fallback" else
            "reported_annual_activity_profile"
          methods[] <- choice$method
          evidence[] <- choice$evidence
        }
      }
      missing <- mi[!is.finite(m$resolved_value[mi]) & !m$duplicate[mi]]
      m$resolved_value[missing] <- prediction[match(m$time[missing], dates)]
      m$method[missing] <- methods[match(m$time[missing], dates)]
      m$evidence[missing] <- evidence[match(m$time[missing], dates)]
      for (selected in unique(m$method[missing])) {
        supported <- missing[m$method[missing] == selected]
        if (selected == "unresolved") next
        if (selected == "annual_residual") {
          m$training_start[supported] <- start
          m$training_end[supported] <- as.Date(paste0(year, "-12-01"))
          next
        }
        if (nrow(valid_history)) {
          training <- valid_history
          if (selected %in% c("coke_activity", "steel_activity")) {
            training <- training %>% inner_join(act %>% filter(proxy == selected,
              is.finite(value), value > 0) %>% select(time), by = "time") %>%
              arrange(desc(time)) %>% slice_head(n = 36)
          } else if (selected == "previous_year") {
            training <- filter(training, time %in% lubridate::`%m-%`(m$time[supported],
              lubridate::years(1)))
          }
          if (nrow(training)) {
            m$training_start[supported] <- min(training$time)
            m$training_end[supported] <- max(training$time)
          }
        } else {
          annual_training <- annual_history %>% filter(time < start, is.finite(value),
            !conflict, !duplicate) %>% arrange(desc(time)) %>% slice_head(n = 3)
          if (nrow(annual_training) && selected %in% c("coke_activity", "steel_activity")) {
            m$training_start[supported] <- min(annual_training$time)
            m$training_end[supported] <- max(annual_training$time)
            m$evidence[supported] <- paste0(m$evidence[supported], "_annual_calibration")
          }
        }
      }
      # Annual estimates use all twelve resolved months, or a separate annual
      # history fallback when the monthly system does not exist.
      if (need_annual) {
        complete_year <- prediction
        complete_year[!is.na(known)] <- known[!is.na(known)]
        if (all(is.finite(complete_year))) {
          a$resolved_value[ai] <- sum(complete_year)
          a$method[ai] <- "resolved_monthly_sum"
          a$evidence[ai] <- if (choice$method == "unresolved") "mixed_monthly_candidates" else
            choice$evidence
        } else {
          past <- annual_observed %>% filter(iso2 == country, time < start,
            !conflict, !duplicate, is.finite(value)) %>% arrange(desc(time))
          if (nrow(past) && past$time[[1]] == as.Date(paste0(year - 1L, "-01-01"))) {
            a$resolved_value[ai] <- past$value[[1]]
            a$method[ai] <- "previous_year_annual"
            a$evidence[ai] <- "insufficient_monthly_evidence_fallback"
          }
        }
      }
    }
  }
  # Bounds only affect estimates. Reported conflicts remain visible downstream.
  bound <- function(resolved, raw, frequency) {
    total_code <- if (frequency == "monthly") "GID_CAL" else "TI_E"
    totals <- .coking_balance(raw, total_code)
    maximum <- totals$value[match(paste(resolved$iso2, resolved$time),
      paste(totals$iso2, totals$time))]
    if (frequency == "monthly") {
      power <- .coking_balance(raw, "TI_EHG_MAP")
      electricity <- power$value[match(paste(resolved$iso2, resolved$time),
        paste(power$iso2, power$time))]
      maximum <- pmin(maximum, (maximum - coalesce(electricity, 0)) /
        (1 - HARDCOAL_COKING_RATE_FACTOR))
    }
    estimated <- resolved$method != "reported" & is.finite(resolved$resolved_value)
    reported_conflict <- resolved$method == "reported" & is.finite(maximum) &
      is.finite(resolved$resolved_value) & resolved$resolved_value > pmax(0, maximum)
    resolved$constraint[reported_conflict] <- "reported_accounting_conflict"
    exceeded <- estimated & is.finite(maximum) & resolved$resolved_value > pmax(0, maximum)
    resolved$bounded[exceeded] <- TRUE
    resolved$constraint[exceeded] <- "accounting_bound"
    derived <- exceeded & frequency == "annual" & resolved$method == "resolved_monthly_sum"
    # A zero/partial annual transformation total cannot erase positive monthly
    # coking observations. Keep their sum; annual energy accounting rejects the
    # inconsistent transformation total instead of declaring coking to be zero.
    resolved$bounded[derived] <- FALSE
    resolved$constraint[derived] <- "annual_transformation_conflict"
    # Annual anchors cannot be reconciled by silently truncating their monthly
    # residual: flag that case as unresolved instead.
    constrained <- exceeded & resolved$method == "annual_residual"
    resolved$resolved_value[exceeded & !derived] <- pmax(0, maximum[exceeded & !derived])
    resolved$resolved_value[constrained] <- NA_real_
    resolved$evidence[constrained] <- "annual_residual_exceeds_monthly_bound"
    resolved
  }
  m <- bound(m, monthly, "monthly")
  # Recompute estimated annual values from bounded monthly estimates when complete.
  for (index in which(a$method == "resolved_monthly_sum")) {
    rows <- m %>% filter(iso2 == a$iso2[[index]],
      lubridate::year(time) == lubridate::year(a$time[[index]]))
    if (nrow(rows) == 12L) a$resolved_value[[index]] <- sum(rows$resolved_value)
  }
  a <- bound(a, annual, "annual")
  m <- .coking_eu(m, observed, "monthly")
  a <- .coking_eu(a, annual_observed, "annual")
  diagnostics <- bind_rows(mutate(m, frequency = "monthly"), mutate(a, frequency = "annual")) %>%
    mutate(siec = SIEC_HARD_COAL, unit = "THS_T",
      deduction = (1 - HARDCOAL_COKING_RATE_FACTOR) * resolved_value,
      status = case_when(!is.finite(resolved_value) ~ "unresolved",
        method == "reported" ~ "reported", TRUE ~ "estimated"),
      conflict_source = if_else(conflict, .coking_source(), NA_character_))
  annual_check <- m %>% mutate(year = lubridate::year(time)) %>%
    group_by(iso2, year) %>% summarise(
      monthly_total = if (n() == 12L) sum(resolved_value) else NA_real_, .groups = "drop"
    ) %>% inner_join(a %>% transmute(iso2, year = lubridate::year(time),
      annual_total = resolved_value), by = c("iso2", "year")) %>%
    mutate(annual_difference = monthly_total - annual_total)
  diagnostics <- diagnostics %>% mutate(year = lubridate::year(time)) %>%
    left_join(annual_check %>% select(iso2, year, annual_difference), by = c("iso2", "year"))
  monthly <- .coking_apply(source_monthly, m, "TI_CO", "M")
  annual <- .coking_apply(source_annual, a, "TI_CO_E", "A")
  attr(monthly, "coal_coking_original") <- source_monthly
  attr(annual, "coal_coking_original") <- source_annual
  attr(monthly, "coal_coking_resolved") <- TRUE
  attr(annual, "coal_coking_resolved") <- TRUE
  attr(monthly, "coal_coking_provenance") <- diagnostics
  attr(annual, "coal_coking_provenance") <- diagnostics
  list(monthly = monthly, yearly = annual, diagnostics = diagnostics,
    validation = bind_rows(validation))
}
