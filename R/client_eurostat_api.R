get_eurostat_from_code <- function(code, iso2s = NULL, use_cache = TRUE, filters = NULL) {
    normalize_eurostat <- function(x) {
        x %>%
            # Column changed with new EUROSTAT version
            dplyr::rename(dplyr::any_of(c(time = "TIME_PERIOD")))
    }

    effective_filters <- filters
    if (!is.null(iso2s)) {
        effective_filters$geo <- iso2s
    }

    cache_parquet_get_or_fetch(
        cache_prefix = glue::glue("eurostat_{code}"),
        cache_key = list(iso2s = iso2s, filters = effective_filters),
        use_cache = use_cache,
        cache_schema_version = "v5_parquet_cache",
        fetch_fun = function() {
            raw <- eurostat::get_eurostat(
                code,
                filters = effective_filters,
                keepFlags = FALSE
            )

            normalize_eurostat(raw)
        }
    )
}
