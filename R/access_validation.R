# Validation files retain their native bytes before any filtering or aggregation.
.validation_input <- function(url, filepath) {
  use_cache <- getOption("creaco2tracker.artifacts")$use_cache
  if (is.null(use_cache)) use_cache <- TRUE
  cache_file_get_or_refresh(
    filepath = filepath,
    populate_fun = function(path) .raw_http_download(url, list(), path),
    consume_fun = function(path) {
      .pipeline_input_file(path, "validation", paste0("000_raw_", basename(filepath)))
      .pipeline_snapshot(.raw_read_csv(path), "validation",
        paste0("010_decoded_", tools::file_path_sans_ext(basename(filepath))))
    },
    validate_fun = function(path) {
      .pipeline_input_file(path, "validation", paste0("000_raw_", basename(filepath)))
      .raw_read_csv(path)
    },
    use_cache = use_cache
  )
}

#' Fetch the EMBER annual data catalogue used by validation
#' @param filepath Local raw-cache path.
#' @return Decoded EMBER catalogue data.
#' @keywords internal
validation_data_access_get_ember_catalogue <- function(
  filepath = "data/ember_yearly_full_release_long_format.csv"
) {
  .validation_input(paste0("https://storage.googleapis.com/emb-prod-bkt-publicdata/",
    "public-downloads/yearly_full_release_long_format.csv"), filepath)
}

#' Fetch the Carbon Monitor EU dataset used by monthly validation
#' @param filepath Local raw-cache path.
#' @return Decoded Carbon Monitor observations.
#' @keywords internal
validation_data_access_get_carbon_monitor <- function(filepath = "data/CM_EU.csv") {
  .validation_input(
    "https://datas.carbonmonitor.org/API/downloadFullDataset.php?source=carbon_eu", filepath)
}

#' Fetch the EEA emissions inventory used by transport validation
#' @param filepath Optional local raw-cache path.
#' @return Decoded EEA inventory observations.
#' @keywords internal
validation_data_access_get_eea_transport <- function(filepath = NULL) {
  url <- paste0("https://sdi.eea.europa.eu/webdav/datastore/public/eea_t",
    "_national-emissions-reported_p_2024_v01_r00/CSV/UNFCCC_v27.csv")
  if (is.null(filepath)) filepath <- file.path("data", basename(url))
  .validation_input(url, filepath)
}
