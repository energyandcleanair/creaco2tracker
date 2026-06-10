log_timed_stage <- function(stage_name, expr) {
    started_at <- Sys.time()
    log_info(glue::glue("START {stage_name}"))
    value <- expr
    ended_at <- Sys.time()
    elapsed_s <- as.numeric(difftime(ended_at, started_at, units = "secs"))
    log_info(glue::glue("DONE {stage_name} ({round(elapsed_s, 2)}s)"))
    value
}

log_object <- function(x, level = c("info", "debug", "warn", "error")) {
    level <- match.arg(level)
    lines <- capture.output(x)
    log_fn <- switch(level,
        info = log_info,
        debug = log_debug,
        warn = log_warn,
        error = log_error
    )
    invisible(lapply(lines, log_fn))
}
