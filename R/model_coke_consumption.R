.coke_unique <- function(x) {
  if (length(x) == 1L && is.finite(x) && x >= 0) x else NA_real_
}

.coke_balance <- function(x, code) {
  required <- c("iso2", "time", "siec", "unit", "nrg_bal", "values")
  if (!all(required %in% names(x))) {
    return(tibble::tibble(iso2 = character(), time = as.Date(character()),
      value = double(), duplicate = logical()))
  }
  x %>%
    filter(siec == SIEC_COKE_OVEN_COKE, unit == "THS_T", nrg_bal == code) %>%
    group_by(iso2, time) %>%
    summarise(value = .coke_unique(values), duplicate = n() != 1L, .groups = "drop")
}

.coke_activity <- function(industry) {
  required <- c("iso2", "time", "nace_r2", "unit", "s_adj", "values")
  if (!all(required %in% names(industry))) {
    return(tibble::tibble(iso2 = character(), time = as.Date(character()), activity = double()))
  }
  x <- industry
  if ("indic_bt" %in% names(x)) x <- filter(x, indic_bt == "PRD")
  if ("freq" %in% names(x)) x <- filter(x, freq == "M")
  x %>%
    filter(nace_r2 == "C24", grepl("^I[0-9]+$", unit), is.finite(values)) %>%
    mutate(index_base = as.integer(sub("^I", "", unit)),
      adjustment_rank = match(s_adj, c("CA", "SCA", "NSA")),
      adjustment_rank = coalesce(adjustment_rank, 99L)) %>%
    group_by(iso2) %>%
    filter(index_base == max(index_base), adjustment_rank == min(adjustment_rank)) %>%
    group_by(iso2, time) %>%
    summarise(activity = .coke_unique(values), .groups = "drop")
}

.coke_candidates <- function(history, activity, dates, cutoff) {
  training_start <- lubridate::`%m-%`(cutoff, lubridate::years(3))
  train <- history %>%
    filter(time >= training_start, time < cutoff, is.finite(value), !duplicate)
  target <- tibble::tibble(time = as.Date(dates),
    previous_year = NA_real_, seasonal = NA_real_, c24_ratio = NA_real_, c24_yoy = NA_real_)
  previous_dates <- lubridate::`%m-%`(target$time, lubridate::years(1))
  target$previous_year <- train$value[match(previous_dates, train$time)]
  seasonal <- train %>%
    mutate(month = lubridate::month(time)) %>%
    group_by(month) %>%
    summarise(value = mean(value), .groups = "drop")
  target$seasonal <- seasonal$value[match(lubridate::month(target$time), seasonal$month)]

  paired <- train %>% inner_join(activity, by = "time") %>%
    filter(is.finite(activity), activity > 0)
  current_activity <- activity$activity[match(target$time, activity$time)]
  if (nrow(paired) >= 6L && sum(paired$activity) > 0) {
    target$c24_ratio <- current_activity * sum(paired$value) / sum(paired$activity)
  }
  previous_activity <- activity$activity[match(previous_dates, activity$time)]
  target$c24_yoy <- target$previous_year * current_activity / previous_activity
  target$c24_yoy[!is.finite(target$c24_yoy) | previous_activity <= 0] <- NA_real_
  target
}

.coke_scores <- function(history, activity, months, cutoff) {
  methods <- c("previous_year", "seasonal", "c24_ratio", "c24_yoy")
  years <- sort(unique(lubridate::year(history$time[history$time < cutoff &
    is.finite(history$value) & !history$duplicate])))
  years <- tail(years, 9L)
  scores <- list()
  for (target_year in years) {
    dates <- as.Date(sprintf("%04d-%02d-01", target_year, months))
    truth <- history$value[match(dates, history$time)]
    if (any(!is.finite(truth)) || max(dates) >= cutoff) next
    candidates <- .coke_candidates(history, activity, dates,
      as.Date(sprintf("%04d-01-01", target_year)))
    for (method in methods) {
      prediction <- candidates[[method]]
      if (any(!is.finite(prediction))) next
      scores[[length(scores) + 1L]] <- tibble::tibble(
        method = method, target_year = target_year,
        block_error = abs(sum(prediction) - sum(truth)),
        monthly_error = mean(abs(prediction - truth)),
        signed_error = sum(prediction - truth), truth_total = sum(truth),
        prediction_total = sum(prediction))
    }
  }
  bind_rows(scores)
}

.coke_choose <- function(candidates, scores) {
  methods <- c("previous_year", "seasonal", "c24_ratio", "c24_yoy")
  available <- methods[vapply(methods,
    function(method) all(is.finite(candidates[[method]])), logical(1))]
  if (!length(available)) {
    return(list(method = "unresolved", values = rep(NA_real_, nrow(candidates)),
      evidence = "no_complete_candidate"))
  }
  if (!nrow(scores)) {
    return(list(method = "unresolved", values = rep(NA_real_, nrow(candidates)),
      evidence = "fewer_than_three_matched_holdouts"))
  }
  shared <- scores %>% filter(method %in% available) %>%
    group_by(target_year) %>% filter(n_distinct(method) == length(available)) %>% ungroup()
  ranked <- shared %>% group_by(method) %>% summarise(
    folds = n(), block_error = mean(block_error), monthly_error = mean(monthly_error),
    .groups = "drop") %>%
    filter(folds >= 3L) %>% arrange(block_error, monthly_error, match(method, methods))
  if (!nrow(ranked)) {
    return(list(method = "unresolved", values = rep(NA_real_, nrow(candidates)),
      evidence = "fewer_than_three_matched_holdouts"))
  }
  method <- ranked$method[[1]]
  list(method = method, values = candidates[[method]], evidence = "matched_holdout_selected")
}

.coke_apply <- function(x, resolved, code = "GID_CAL") {
  if (!nrow(resolved)) return(x)
  target <- x$siec == SIEC_COKE_OVEN_COKE & x$unit == "THS_T" & x$nrg_bal == code
  key <- paste(resolved$iso2, resolved$time)
  index <- match(paste(x$iso2, x$time), key)
  replace <- target & !is.na(index)
  x$values[replace] <- resolved$resolved_value[index[replace]]
  absent <- resolved %>% filter(is.finite(resolved_value)) %>%
    anti_join(x %>% filter(target) %>% select(iso2, time), by = c("iso2", "time"))
  if (nrow(absent)) {
    rows <- x[rep(NA_integer_, nrow(absent)), , drop = FALSE]
    rows$iso2 <- absent$iso2
    rows$time <- absent$time
    rows$siec <- SIEC_COKE_OVEN_COKE
    rows$unit <- "THS_T"
    rows$nrg_bal <- code
    rows$values <- absent$resolved_value
    if ("freq" %in% names(rows)) rows$freq <- "M"
    if ("geo" %in% names(rows)) rows$geo <- rows$iso2
    x <- bind_rows(x, rows)
  }
  x
}

.coke_zero_power_evidence <- function(annual, iso2, target_year) {
  codes <- c("TI_EHG_MAPE_E", "TI_EHG_MAPCHP_E")
  rows <- annual %>% filter(.data$iso2 == .env$iso2,
    siec == SIEC_COKE_OVEN_COKE, unit == "THS_T", nrg_bal %in% codes,
    lubridate::year(time) < target_year) %>%
    group_by(year = lubridate::year(time), nrg_bal) %>%
    summarise(value = .coke_unique(values), .groups = "drop")
  if (nrow(rows) < 6L || !all(codes %in% rows$nrg_bal)) return(FALSE)
  rows <- rows %>% tidyr::pivot_wider(names_from = nrg_bal, values_from = value) %>%
    filter(if_all(all_of(codes), is.finite)) %>% arrange(year) %>% slice_tail(n = 3)
  nrow(rows) == 3L && identical(diff(rows$year), c(1, 1)) &&
    all(abs(unlist(rows[codes], use.names = FALSE)) <= 0.01)
}

.coke_add_power_zeros <- function(monthly, annual, diagnostics) {
  totals <- diagnostics %>% filter(is.finite(resolved_value)) %>%
    mutate(target_year = lubridate::year(time))
  power <- .coke_balance(monthly, "TI_EHG_MAP")
  evidence <- totals %>% distinct(iso2, target_year)
  codes <- c("TI_EHG_MAPE_E", "TI_EHG_MAPCHP_E")
  annual_power <- annual %>% filter(siec == SIEC_COKE_OVEN_COKE, unit == "THS_T",
    nrg_bal %in% codes) %>%
    group_by(iso2, year = lubridate::year(time), nrg_bal) %>%
    summarise(value = .coke_unique(values), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = nrg_bal, values_from = value) %>%
    add_missing_cols(codes)
  evidence$supported <- vapply(seq_len(nrow(evidence)), function(index) {
    rows <- annual_power[annual_power$iso2 == evidence$iso2[[index]] &
      annual_power$year < evidence$target_year[[index]], , drop = FALSE]
    rows <- rows[is.finite(rows[[codes[[1]]]]) & is.finite(rows[[codes[[2]]]]), , drop = FALSE]
    rows <- tail(rows[order(rows$year), , drop = FALSE], 3)
    nrow(rows) == 3L && identical(diff(rows$year), c(1, 1)) &&
      all(abs(unlist(rows[codes], use.names = FALSE)) <= 0.01)
  }, logical(1))
  power_keys <- power %>% filter(is.finite(value)) %>% select(iso2, time)
  method <- diagnostics %>%
    mutate(target_year = lubridate::year(time)) %>%
    left_join(evidence, by = c("iso2", "target_year")) %>%
    mutate(reported_power = paste(iso2, time) %in% paste(power_keys$iso2, power_keys$time),
      electricity_method = case_when(
        reported_power ~ "reported",
        coalesce(supported, FALSE) ~ "three_annual_zero_or_negligible_pairs",
        TRUE ~ "unresolved"
      ))
  diagnostics$electricity_method <- method$electricity_method
  additions <- totals %>%
    left_join(evidence, by = c("iso2", "target_year")) %>%
    filter(supported) %>%
    anti_join(power_keys, by = c("iso2", "time")) %>%
    transmute(iso2, time, resolved_value = 0)
  if (nrow(additions)) {
    monthly <- .coke_apply(monthly, additions, "TI_EHG_MAP")
  }
  list(monthly = monthly, diagnostics = diagnostics)
}

#' Resolve short internal gaps in monthly coke consumption
#'
#' Original observations remain distinguishable from estimates. Only gaps of at
#' most six months bounded by reported coke consumption are eligible.
#' @keywords internal
#' @noRd
.resolve_coke_consumption <- function(monthly, annual, industry = tibble::tibble()) {
  previous <- attr(monthly, "coke_consumption_original")
  if (!is.null(previous)) monthly <- previous
  source_monthly <- monthly
  observed <- .coke_balance(monthly, "GID_CAL")
  activity <- .coke_activity(industry)
  diagnostics <- list()
  validation <- list()

  for (country in unique(observed$iso2)) {
    country_observed <- observed %>% filter(iso2 == country) %>% arrange(time)
    if (!nrow(country_observed)) next
    dates <- seq(min(country_observed$time), max(country_observed$time), by = "month")
    d <- tibble::tibble(iso2 = country, time = dates) %>%
      left_join(country_observed, by = c("iso2", "time")) %>%
      mutate(duplicate = coalesce(duplicate, FALSE), original_value = value,
        resolved_value = if_else(duplicate, NA_real_, value),
        method = if_else(is.finite(resolved_value), "reported", "unresolved"),
        evidence = if_else(duplicate, "duplicate_source_rows", "original_source"),
        eligible_gap = FALSE, electricity_method = "reported_or_not_applicable") %>%
      select(-value)
    missing <- !is.finite(d$resolved_value) & !d$duplicate
    # Work directly over consecutive missing indices to avoid treating explicit
    # NA and absent rows differently.
    starts <- which(missing & !lag(missing, default = FALSE))
    for (start_index in starts) {
      end_index <- start_index
      while (end_index < nrow(d) && missing[[end_index + 1L]]) end_index <- end_index + 1L
      indices <- start_index:end_index
      bounded <- start_index > 1L && end_index < nrow(d) &&
        is.finite(d$resolved_value[[start_index - 1L]]) &&
        is.finite(d$resolved_value[[end_index + 1L]]) && length(indices) <= 6L &&
        any(lubridate::year(d$time[indices]) == lubridate::year(max(d$time)))
      if (!bounded) next
      d$eligible_gap[indices] <- TRUE
      act <- activity %>% filter(iso2 == country)
      candidates <- .coke_candidates(country_observed, act, d$time[indices],
        as.Date(sprintf("%04d-01-01", lubridate::year(d$time[[start_index]]))))
      scores <- .coke_scores(country_observed, act, lubridate::month(d$time[indices]),
        min(d$time[indices]))
      if (nrow(scores)) validation[[length(validation) + 1L]] <-
        mutate(scores, iso2 = country, target_start = min(d$time[indices]),
          target_end = max(d$time[indices]))
      choice <- .coke_choose(candidates, scores)
      d$resolved_value[indices] <- choice$values
      d$method[indices] <- choice$method
      d$evidence[indices] <- choice$evidence
    }
    diagnostics[[length(diagnostics) + 1L]] <- d
  }
  diagnostics <- bind_rows(diagnostics) %>%
    mutate(status = case_when(!is.finite(resolved_value) ~ "unresolved",
      method == "reported" ~ "reported", TRUE ~ "estimated"),
      siec = SIEC_COKE_OVEN_COKE, unit = "THS_T")
  monthly <- .coke_apply(source_monthly, diagnostics)
  allocated <- .coke_add_power_zeros(monthly, annual, diagnostics)
  monthly <- allocated$monthly
  diagnostics <- allocated$diagnostics
  attr(monthly, "coke_consumption_original") <- source_monthly
  attr(monthly, "coke_consumption_resolved") <- TRUE
  attr(monthly, "coke_consumption_provenance") <- diagnostics
  list(monthly = monthly, diagnostics = diagnostics, validation = bind_rows(validation))
}
