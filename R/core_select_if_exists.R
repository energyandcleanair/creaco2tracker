select_if_exists <- function(data, ...) {
  if (is.null(data) || nrow(data)==0) {
    return(NULL)
  }

  dplyr::select(data, ...)
}
