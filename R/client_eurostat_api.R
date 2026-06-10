get_eurostat_from_code <- function(code, iso2s = NULL, use_cache = TRUE, filters = NULL) {
    normalize_eurostat <- function(x) {
        x %>%
            # Column changed with new EUROSTAT version
            dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD")))
    }

    # Create a digest of iso2s and filters
    create_dir("cache")
    cache_schema_version <- "v4_normalized_time_col"
    digest <- digest::digest(list(cache_schema_version, iso2s, filters))
    filepath <- file.path("cache", glue("eurostat_{code}_{digest}.rds"))

    if (use_cache && file.exists(filepath)) {
        return(normalize_eurostat(readRDS(filepath)))
    }

    effective_filters <- filters
    if (!is.null(iso2s)) {
        effective_filters$geo <- iso2s
    }

    raw <- eurostat::get_eurostat(
        code,
        filters = effective_filters,
        keepFlags = FALSE
    )

    normalized <- normalize_eurostat(raw)
    saveRDS(normalized, filepath)
    normalized
}
