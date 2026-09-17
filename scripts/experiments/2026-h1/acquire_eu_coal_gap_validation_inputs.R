#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(devtools)
  library(dplyr)
  library(readr)
})

load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default) {
  position <- match(name, args)
  if (is.na(position) || position == length(args)) default else args[[position + 1]]
}

output <- arg("--output", ".tmp/eu_coal_gap_validation_inputs_2026-09-16")
cutoff <- as.Date(arg("--cutoff", "2026-09-16"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)

sources <- list(
  nrg_cb_sffm = get_eurostat_from_code("nrg_cb_sffm", use_cache = FALSE),
  nrg_cb_sff = get_eurostat_from_code("nrg_cb_sff", use_cache = FALSE),
  iea_conversion = iea.get_conversion_factors(
    iso2 = c(get_eu_iso2s(include_eu = FALSE), "EU"),
    use_cache = FALSE
  )
)

paths <- file.path(output, paste0(names(sources), ".parquet"))
for (index in seq_along(sources)) {
  arrow::write_parquet(sources[[index]], paths[[index]])
}

latest_source_date <- vapply(sources, function(data) {
  date_column <- intersect(c("time", "year"), names(data))[[1]]
  as.character(max(data[[date_column]], na.rm = TRUE))
}, character(1))

manifest <- tibble::tibble(
  source = names(sources),
  path = paths,
  checksum_md5 = unname(tools::md5sum(paths)),
  rows = vapply(sources, nrow, integer(1)),
  source_latest_date = latest_source_date,
  retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  requested_cutoff = as.character(cutoff),
  use_cache = FALSE
)
readr::write_csv(manifest, file.path(output, "input_manifest.csv"))
writeLines(
  c(
    paste0("cutoff=", cutoff),
    "eurostat_monthly=nrg_cb_sffm",
    "eurostat_annual=nrg_cb_sff",
    "conversion=IEA conversion factors",
    "use_cache=false"
  ),
  file.path(output, "acquisition_settings.txt")
)

message("Frozen fresh validation inputs in ", output)
