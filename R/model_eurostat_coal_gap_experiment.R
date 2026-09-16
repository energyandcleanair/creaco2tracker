# Internal helpers for the raw-Eurostat coal gap-filling study.  These functions
# deliberately do not participate in the production data-access path.

.coal_gap_fuels <- c("C0100", "C0200", "C0330", "S2000")
.coal_gap_nonnegative_balances <- c(
  "GID_CAL", "GID_OBS", "IPRD", "IMP", "EXP", "TI_EHG_MAP", "TI_CO", "FC_IND", "FC_OTH"
)

.coal_gap_keys <- function() c("iso2", "siec", "nrg_bal", "unit")

.coal_gap_prepare <- function(monthly, annual, eu_iso2s = get_eu_iso2s(FALSE)) {
  if (!"flags" %in% names(monthly)) monthly$flags <- rep("", nrow(monthly))
  monthly <- monthly %>%
    add_iso2() %>%
    filter(iso2 %in% eu_iso2s, siec %in% .coal_gap_fuels, unit == "THS_T") %>%
    transmute(iso2, siec, nrg_bal, unit, time = as.Date(time), value = values,
      reported = !is.na(values), source_flag = dplyr::coalesce(flags, ""))
  annual <- annual %>%
    add_iso2() %>%
    filter(iso2 %in% eu_iso2s, siec %in% .coal_gap_fuels, unit == "THS_T") %>%
    transmute(iso2, siec, nrg_bal, unit, year = lubridate::year(time), annual_value = values)
  list(monthly = monthly, annual = annual)
}

.coal_gap_complete_series <- function(x) {
  keys <- .coal_gap_keys()
  bounds <- x %>% group_by(across(all_of(keys))) %>%
    summarise(first = min(time[reported]), last = max(time[reported]), .groups = "drop")
  x %>% right_join(bounds, by = keys) %>% group_by(across(all_of(keys))) %>%
    tidyr::complete(time = seq(first[1], last[1], by = "month")) %>%
    ungroup() %>% mutate(reported = dplyr::coalesce(reported, FALSE),
      source_flag = dplyr::coalesce(source_flag, ""), value = if_else(reported, value, NA_real_)) %>%
    select(-first, -last)
}

.coal_gap_runs <- function(x) {
  keys <- .coal_gap_keys()
  x %>% arrange(across(all_of(keys)), time) %>% group_by(across(all_of(keys))) %>%
    mutate(run = cumsum(reported | lag(reported, default = TRUE))) %>%
    filter(!reported) %>% group_by(across(all_of(c(keys, "run")))) %>%
    summarise(start = min(time), end = max(time), length = n(), .groups = "drop") %>%
    mutate(kind = "actual")
}

.coal_gap_mask <- function(x, start, length, scenario = c("internal", "forecast")) {
  scenario <- match.arg(scenario)
  end <- lubridate::`%m+%`(start, lubridate::period(length - 1, "month"))
  if (scenario == "forecast") end <- max(x$time)
  target <- x %>% filter(time >= start, time <= end)
  if (nrow(target) == 0 || any(!target$reported)) return(NULL)
  x %>% mutate(masked = time >= start & time <= end, observed_value = value,
    value = if_else(masked, NA_real_, value), reported = reported & !masked)
}

.coal_gap_calendar_baseline <- function(x, start, length, method) {
  target <- seq(start, by = "month", length.out = length)
  if (method == "previous_year") {
    y <- x$value[match(lubridate::`%m-%`(target, lubridate::period(1, "year")), x$time)]
    if (anyNA(y)) return(NULL)
    return(y)
  }
  years <- 1:3
  values <- vapply(years, function(n) x$value[match(lubridate::`%m-%`(target, lubridate::period(n, "year")), x$time)], numeric(length))
  if (anyNA(values)) return(NULL)
  rowMeans(matrix(values, nrow = length))
}

.coal_gap_linear <- function(x, start, length) {
  target <- seq(start, by = "month", length.out = length)
  out <- vapply(target, function(date) {
    prior <- x %>% filter(time < start, lubridate::month(time) == lubridate::month(date), reported)
    prior <- tail(prior, 5)
    if (nrow(prior) < 4) return(NA_real_)
    stats::predict(stats::lm(value ~ lubridate::year(time), data = prior),
      newdata = data.frame(time = date))
  }, numeric(1))
  if (anyNA(out)) NULL else out
}

.coal_gap_ets_side <- function(x, boundary, length, direction = c("forward", "backward")) {
  direction <- match.arg(direction)
  side <- if (direction == "forward") x %>% filter(time < boundary, reported) else x %>% filter(time >= boundary, reported)
  side <- if (direction == "forward") side %>% arrange(desc(time)) else side %>% arrange(time)
  consecutive <- cumsum(c(TRUE, diff(if (direction == "forward") -as.numeric(side$time) else as.numeric(side$time)) != 28 &
    diff(if (direction == "forward") -as.numeric(side$time) else as.numeric(side$time)) != 29 &
    diff(if (direction == "forward") -as.numeric(side$time) else as.numeric(side$time)) != 30 &
    diff(if (direction == "forward") -as.numeric(side$time) else as.numeric(side$time)) != 31))
  side <- side[consecutive == consecutive[1], , drop = FALSE] %>% arrange(time)
  if (nrow(side) < 36) return(NULL)
  side <- tail(side, 60)
  values <- side$value
  if (direction == "backward") values <- rev(values)
  # Some degenerate zero-heavy series make automatic ETS selection extremely
  # slow. A timed-out fit is an ineligible fit, never a reason to stall the
  # experiment or to fall back to an interpolated training series.
  fit <- tryCatch({
    setTimeLimit(elapsed = 2, transient = TRUE)
    suppressWarnings(forecast::ets(stats::ts(values, frequency = 12)))
  }, error = identity)
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  if (inherits(fit, "error")) return(list(error = conditionMessage(fit)))
  prediction <- as.numeric(forecast::forecast(fit, h = length)$mean)
  if (direction == "backward") prediction <- rev(prediction)
  list(values = prediction, specification = fit$method, train_start = min(side$time), train_end = max(side$time))
}

.coal_gap_interpolate <- function(x, start, length) {
  targets <- seq(start, by = "month", length.out = length)
  before <- x %>% filter(time < start, reported) %>% slice_tail(n = 1)
  after <- x %>% filter(time > max(targets), reported) %>% slice_head(n = 1)
  if (nrow(before) == 0 || nrow(after) == 0) return(NULL)
  positions <- match(c(before$time, after$time), x$time)
  stats::approx(positions, c(before$value, after$value), xout = match(targets, x$time), rule = 1)$y
}

.coal_gap_accounting <- function(x, annual, start, length, tolerance = .05) {
  if (!identical(unique(x$nrg_bal), "GID_CAL")) return(NULL)
  keys <- c("iso2", "siec", "unit")
  inputs <- attr(x, "supply_inputs")
  if (is.null(inputs)) return(NULL)
  target_years <- sort(unique(lubridate::year(inputs$time[inputs$reported])))
  annual_years <- annual %>% filter(nrg_bal == "IC_CAL") %>% pull(year)
  complete <- tail(intersect(target_years, annual_years), 3)
  if (length(complete) < 3) return(NULL)
  history <- inputs %>% filter(lubridate::year(time) %in% complete, reported) %>%
    group_by(year = lubridate::year(time)) %>%
    summarise(supply = sum(dplyr::case_when(nrg_bal == "IPRD" ~ value, nrg_bal == "IMP" ~ value,
      nrg_bal == "EXP" ~ -value, nrg_bal == "STK_CHG" ~ value, TRUE ~ 0)), n = n(), .groups = "drop")
  annual_target <- annual %>% filter(nrg_bal == "IC_CAL", year %in% complete) %>% select(year, annual_value)
  check <- history %>% inner_join(annual_target, by = "year") %>%
    mutate(error = abs(supply - annual_value) / pmax(abs(annual_value), 1))
  if (nrow(check) != 3 || any(check$n != 12 * 4) || any(check$error > tolerance)) return(NULL)
  dates <- seq(start, by = "month", length.out = length)
  prediction <- inputs %>% filter(time %in% dates, reported) %>% group_by(time) %>%
    summarise(value = sum(dplyr::case_when(nrg_bal == "IPRD" ~ value, nrg_bal == "IMP" ~ value,
      nrg_bal == "EXP" ~ -value, nrg_bal == "STK_CHG" ~ value, TRUE ~ 0)), .groups = "drop")
  if (nrow(prediction) != length) return(NULL)
  list(values = prediction$value, reconciliation = check)
}

.coal_gap_predict <- function(x, annual, start, length, scenario, method) {
  result <- switch(method,
    previous_year = .coal_gap_calendar_baseline(x, start, length, "previous_year"),
    historical_average = .coal_gap_calendar_baseline(x, start, length, "historical_average"),
    linear = .coal_gap_linear(x, start, length),
    forward_ets = .coal_gap_ets_side(x, start, length, "forward"),
    interpolation = if (scenario == "internal") .coal_gap_interpolate(x, start, length) else NULL,
    bidirectional_ets = if (scenario == "internal") {
      forward <- .coal_gap_ets_side(x, start, length, "forward")
      backward <- .coal_gap_ets_side(x, lubridate::`%m+%`(start, lubridate::period(length, "month")), length, "backward")
      if (is.null(forward) || is.null(backward) || is.null(forward$values) || is.null(backward$values)) NULL else {
        k <- seq_len(length); list(values = forward$values * (length + 1 - k) / (length + 1) + backward$values * k / (length + 1),
          specification = paste(forward$specification, backward$specification, sep = " | "))
      }
    } else NULL,
    accounting = .coal_gap_accounting(x, annual, start, length), NULL)
  if (is.null(result)) return(NULL)
  values <- if (is.list(result)) result$values else result
  if (is.null(values) || anyNA(values)) return(NULL)
  clipped <- unique(x$nrg_bal) %in% .coal_gap_nonnegative_balances && any(values < 0)
  values <- if (unique(x$nrg_bal) %in% .coal_gap_nonnegative_balances) pmax(values, 0) else values
  list(values = values, method = method, clipped = clipped,
    specification = if (is.list(result)) result$specification %||% NA_character_ else NA_character_)
}

.coal_gap_apply_sequence <- function(x, annual, start, length, scenario, methods) {
  for (method in methods) {
    prediction <- .coal_gap_predict(x, annual, start, length, scenario, method)
    if (!is.null(prediction)) return(prediction)
  }
  list(values = rep(NA_real_, length), method = "unresolved", clipped = FALSE, specification = NA_character_)
}

.coal_gap_reconcile <- function(x, annual, tolerance = .05) {
  keys <- .coal_gap_keys()
  x %>% mutate(year = lubridate::year(time), annual_nrg_bal = if_else(nrg_bal == "GID_CAL", "IC_CAL", nrg_bal)) %>%
    group_by(across(all_of(c(keys, "annual_nrg_bal", "year")))) %>%
    summarise(months = n(), monthly_sum = if (all(!is.na(value))) sum(value) else NA_real_,
      source = dplyr::case_when(all(reported) ~ "reported", all(!reported) ~ "imputed", TRUE ~ "mixed"), .groups = "drop") %>%
    select(-nrg_bal) %>% left_join(annual %>% rename(annual_nrg_bal = nrg_bal, annual_balance = annual_value),
      by = c("iso2", "siec", "unit", "annual_nrg_bal", "year")) %>%
    mutate(relative_difference = if_else(is.na(annual_balance) | annual_balance == 0,
      NA_real_, abs(monthly_sum - annual_balance) / abs(annual_balance)),
      warning = months == 12 & !is.na(relative_difference) & relative_difference > tolerance,
      zero_denominator = !is.na(annual_balance) & annual_balance == 0)
}

.coal_gap_score <- function(predictions) {
  predictions %>% filter(!is.na(predicted), !is.na(actual)) %>% group_by(method, scenario, gap_id) %>%
    summarise(monthly_mae = mean(abs(predicted - actual)), monthly_rmse = sqrt(mean((predicted - actual)^2)),
      signed_bias = mean(predicted - actual), gap_total_error = sum(predicted - actual), .groups = "drop")
}

.coal_gap_downstream_solid <- function(raw_monthly) {
  # Deliberately delegates to the unchanged production transformation so a
  # caller can compare a raw fill with its actual downstream treatment.
  process_solid_monthly(raw_monthly %>% add_iso2() %>% filter(!is.na(iso2)), tibble::tibble())
}
