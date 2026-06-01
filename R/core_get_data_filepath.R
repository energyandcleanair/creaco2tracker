#' Get path to package data file
#'
#' This function returns the path to a data file, either from the local inst/extdata
#' directory during development or from the installed package's extdata directory.
#'
#' @param filename Name of the file to locate
#' @return Character string with the full path to the file
#' @export
#'
#' @examples
#' \dontrun{
#' filepath <- get_data_file("my_data.csv")
#' }
get_data_filepath <- function(filename) {
  # First try development location (inst/extdata)
  dev_path <- file.path("inst", "extdata", filename)
  if (file.exists(dev_path)) {
    return(dev_path)
  }
  
  # Then try installed package location
  pkg_path <- system.file("extdata", filename, package = "creaco2tracker")
  if (pkg_path != "") {
    return(pkg_path)
  }
  
  stop(sprintf("Could not find data file '%s' in package", filename))
}