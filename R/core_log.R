configure_logger_layout <- function() {
  logger::log_layout(
    logger::layout_glue_generator(
      format = "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}] {level} {msg}"
    )
  )
}

configure_logger_threshold <- function() {
  logger::log_threshold(logger::TRACE)
}

configure_logger <- function() {
  configure_logger_layout()
  configure_logger_threshold()
}

.onLoad <- function(libname, pkgname) {
  configure_logger()
}

.onAttach <- function(libname, pkgname) {
  configure_logger()
}
