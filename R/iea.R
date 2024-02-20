
iea.get_balance <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {
  # Generate a hash of the parameters
  param_hash <- digest::digest(c(year_from, year_to, iso2))

  # Create a filename using the hash
  cache_dir <- "cache" # Set the cache directory path
  filepath <- file.path(cache_dir, paste0("ieabalance_", param_hash, ".RDS"))

  # Check if the file exists in cache and use_cache is TRUE
  if (use_cache && file.exists(filepath)) {
    return(readRDS(filepath))
  } else {
    # Fetch the data
    result <- pbapply::pblapply(seq(year_from, year_to), function(year) {
      creahelpers::api.get("api.energyandcleanair.org/energy/iea_balance",
                           year_from = year,
                           year_to = year,
                           country = iso2)
    }) %>%
      bind_rows()

    # Save the fetched data to cache
    dir.create(dirname(filepath))
    saveRDS(result, filepath)

    return(result)
  }
}


iea.get_conversion_factors <- function(year_from = 2000, year_to = 2022, iso2, use_cache = TRUE) {

    # Generate a hash of the parameters
  param_hash <- digest::digest(c(year_from, year_to, iso2))

  # Create a filename using the hash
  cache_dir <- "cache" # Set the cache directory path
  filepath <- file.path(cache_dir, paste0("ieaconversion_", param_hash, ".RDS"))

  # Check if the file exists in cache and use_cache is TRUE
  if (use_cache && file.exists(filepath)) {
    return(readRDS(filepath))
  } else {
    # Fetch the data
    result <- creahelpers::api.get("api.energyandcleanair.org/energy/iea_conversion",
                                   year_from = year_from,
                                   year_to = year_to,
                                   country = iso2)

    # Save the fetched data to cache
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir)
    }
    saveRDS(result, filepath)

    return(result)
  }
}
