configure_logger_layout <- function() {
  logger::log_layout(
    logger::layout_glue_generator(
      format = "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}] {level} {msg}"
    )
  )
}

.onLoad <- function(libname, pkgname) {
  configure_logger_layout()
}

.onAttach <- function(libname, pkgname) {
  configure_logger_layout()
}
