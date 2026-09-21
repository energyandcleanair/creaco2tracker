#' Fetch the EMBER annual data catalogue used by validation
#'
#' @param filepath Local cache path.
#'
#' @return Raw EMBER catalogue data.
#' @keywords internal
validation_data_access_get_ember_catalogue <- function(
  filepath = "data/ember_yearly_full_release_long_format.csv"
) {
  url <- paste0(
    "https://storage.googleapis.com/emb-prod-bkt-publicdata/",
    "public-downloads/yearly_full_release_long_format.csv"
  )
  if (!file.exists(filepath)) {
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
    utils::download.file(url, filepath)
  }
  readr::read_csv(filepath, show_col_types = FALSE)
}


#' Fetch the Carbon Monitor EU dataset used by monthly validation
#'
#' @param filepath Local cache path.
#'
#' @return Raw Carbon Monitor observations.
#' @keywords internal
validation_data_access_get_carbon_monitor <- function(filepath = "data/CM_EU.csv") {
  url <- "https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu"
  if (!file.exists(filepath)) {
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
    utils::download.file(url, filepath)
  }
  readr::read_csv(filepath, show_col_types = FALSE)
}


#' Fetch the EEA emissions inventory used by transport validation
#'
#' @param filepath Optional local cache path.
#'
#' @return Raw EEA inventory observations.
#' @keywords internal
validation_data_access_get_eea_transport <- function(filepath = NULL) {
  url <- paste0(
    "https://sdi.eea.europa.eu/webdav/datastore/public/eea_t",
    "_national-emissions-reported_p_2024_v01_r00/CSV/UNFCCC_",
    "v27.csv"
  )
  if (is.null(filepath)) filepath <- file.path("data", basename(url))
  if (!file.exists(filepath)) {
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
    utils::download.file(url, filepath)
  }
  readr::read_csv(filepath, show_col_types = FALSE)
}
