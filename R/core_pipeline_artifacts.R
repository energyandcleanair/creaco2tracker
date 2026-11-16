# Stage ordering lives here; no index or manifest is written to the bundle.
PIPELINE_ARTIFACT_STAGES <- c(
  entsog = "10_010_gas_entsog",
  agsi = "10_020_gas_agsi",
  gas_inputs = "10_030_gas_masked_inputs",
  gas_candidates = "10_040_gas_candidates",
  gas_scores = "10_050_gas_selection_scores",
  gas_selected = "10_060_gas_selected",
  gas_factors = "10_070_gas_correction_factors",
  get_gas_demand = "10_990_gas_demand",
  entsoe = "20_010_power_entsoe",
  entsoe_totals = "20_020_power_entsoe_totals",
  entsoe_completed = "20_030_power_entsoe_completed",
  ember = "20_040_power_ember",
  power_inputs = "20_050_power_masked_inputs",
  power_monthly = "20_060_power_monthly",
  power_monthly_ratios = "20_070_power_monthly_ratios",
  power_monthly_extended = "20_080_power_monthly_ratios_extended",
  power_scaled = "20_090_power_monthly_scaled",
  power_fallback = "20_100_power_with_fallback",
  power_yearly_ratios = "20_110_power_yearly_ratios",
  power_yearly_extended = "20_120_power_yearly_ratios_extended",
  power_reconciled = "20_130_power_reconciled",
  get_power_generation = "20_990_power_generation",
  eurostat = "30_010_eurostat",
  eurostat_masked = "30_020_eurostat_masked",
  collect_oil_fill_non_energy_yearly = "30_030_oil_non_energy_yearly",
  collect_oil_fill_gid_obs_yearly = "30_040_oil_observed_yearly",
  collect_oil_fill_non_energy_monthly = "30_050_oil_non_energy_monthly",
  collect_oil_add_transport = "30_060_oil_transport",
  collect_oil_fill_gid_obs_monthly = "30_070_oil_observed_monthly",
  eurostat_collect_oil = "30_075_oil_prepared",
  eurostat_collect_gas = "30_076_gas_prepared",
  eurostat_collect_solid = "30_085_coal_prepared",
  solid_filled = "30_080_coal_gap_filled",
  solid_annual_backed = "30_090_coal_annual_backed",
  coke_resolved = "30_100_coke_resolved",
  coking_resolved = "30_110_coking_resolved",
  process_eurostat_monthly = "30_120_consumption_monthly",
  process_eurostat_yearly = "30_130_consumption_yearly",
  apply_seasonal_adjustment = "30_140_consumption_annual_allocated",
  consumption_combined = "30_150_consumption_combined",
  get_eurostat_cons = "30_990_consumption",
  consumption_prepared = "30_995_consumption_prepared",
  get_eurostat_indprod = "40_990_industrial_production",
  iea = "50_010_iea",
  references = "50_020_reference_factors",
  ncv_cleaned = "50_030_ncv_cleaned",
  ncv_filled = "50_040_ncv_filled",
  consumption_ncv = "50_050_consumption_with_ncv",
  emissions_detailed = "50_060_emissions_detailed",
  get_co2_from_eurostat_cons = "50_990_emissions_unprojected",
  projection_split = "60_010_projection_gas_split",
  projection_elec = "60_020_projection_electricity",
  projection_gas = "60_030_projection_gas",
  projection_eu = "60_040_projection_eu",
  projection_coal = "60_050_projection_coal",
  projection_gaps = "60_060_projection_internal_gaps",
  projection_forecast = "60_070_projection_forecast",
  projection_forecast_gaps = "60_080_projection_forecast_gaps",
  project_until_now = "60_990_emissions_projected",
  regions = "70_010_selected_regions",
  downscale_daily = "70_020_emissions_daily",
  gas_split = "80_010_gas_split",
  fuels = "80_020_fuels_recombined",
  detotalised = "80_030_emissions_detotalised",
  totals = "80_040_emissions_with_totals",
  tail = "80_050_eu_tail_adjusted",
  validation = "90_010_validation_inputs",
  final = "99_000_co2"
)

.pipeline_artifacts_start <- function(folder, use_cache) {
  if (!is.null(folder)) {
    if (!is.character(folder) || length(folder) != 1L || is.na(folder) || !nzchar(folder)) {
      stop("artifacts_folder must be NULL or one non-empty path.", call. = FALSE)
    }
    if (file.exists(folder) && (!dir.exists(folder) ||
      length(list.files(folder, all.files = TRUE, no.. = TRUE)) > 0L)) {
      stop("artifacts_folder must be new or empty.", call. = FALSE)
    }
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    folder <- normalizePath(folder, mustWork = TRUE)
  }
  old <- options(creaco2tracker.artifacts = list(folder = folder, use_cache = use_cache))
  function() options(old)
}

.pipeline_artifact_name <- function(stage, detail = NULL) {
  prefix <- unname(PIPELINE_ARTIFACT_STAGES[[stage]])
  if (is.null(prefix)) stop(paste0("Unknown artifact stage: ", stage), call. = FALSE)
  paste(c(prefix, detail),
    collapse = "_"
  )
}

.pipeline_artifact_write <- function(name, write_fun) {
  folder <- getOption("creaco2tracker.artifacts")$folder
  if (is.null(folder)) {
    return(invisible(NULL))
  }
  path <- file.path(folder, name)
  temporary <- tempfile(pattern = ".artifact-", tmpdir = folder)
  on.exit(unlink(temporary), add = TRUE)
  tryCatch(
    {
      write_fun(temporary)
      # Repeated reads share a filename when unchanged. Preserve distinct versions
      # if a source refreshes during a run instead of overwriting earlier evidence.
      if (file.exists(path)) {
        checksum <- digest::digest(file = temporary)
        extension <- if (grepl("\\.(csv|tsv)\\.gz$", name)) {
          sub("^.*(\\.(csv|tsv)\\.gz)$", "\\1", name)
        } else {
          paste0(".", tools::file_ext(name))
        }
        stem <- substr(name, 1, nchar(name) - nchar(extension))
        occurrence <- 1L
        while (file.exists(path)) {
          if (identical(digest::digest(file = path), checksum)) return(invisible(path))
          occurrence <- occurrence + 1L
          path <- file.path(folder, paste0(stem, sprintf("_%03d", occurrence), extension))
        }
      }
      if (!file.rename(temporary, path)) stop("Cannot move artifact into place.")
    },
    error = function(e) {
      stop(structure(
        list(message = paste0(
          "Cannot write artifact ", name, ": ",
          conditionMessage(e)
        ), call = NULL),
        class = c("pipeline_artifact_error", "error", "condition")
      ))
    }
  )
  invisible(path)
}

.pipeline_snapshot <- function(value, stage, detail = NULL) {
  if (is.null(getOption("creaco2tracker.artifacts")$folder)) {
    return(value)
  }
  name <- .pipeline_artifact_name(stage, detail)
  if (is.data.frame(value)) {
    .pipeline_artifact_write(paste0(name, ".parquet"), function(path) {
      # Capture columns separately from provenance attributes, without modifying value.
      arrow::write_parquet(tibble::as_tibble(value), path)
    })
    extra <- attributes(value)
    extra[c("names", "row.names", "class", "groups")] <- NULL
    for (key in names(extra)) {
      if (is.data.frame(extra[[key]]) || is.list(extra[[key]])) {
        .pipeline_snapshot(extra[[key]], stage, paste(c(detail, key),
          collapse = "_"
        ))
      }
    }
  } else if (is.list(value)) {
    for (key in names(value)) {
      .pipeline_snapshot(value[[key]], stage, paste(c(detail, key),
        collapse = "_"
      ))
    }
  }
  value
}

.pipeline_input_file <- function(path, stage, detail) {
  .pipeline_artifact_write(.pipeline_artifact_name(stage, detail), function(destination) {
    if (!file.copy(path, destination, overwrite = TRUE)) stop("Cannot copy input file.")
  })
  invisible(path)
}

# Human-readable variants are file labels, not request metadata.
.pipeline_variant <- function(parts) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  label <- paste(
    vapply(names(parts), function(key) {
      paste0(key, "-", paste(sort(as.character(parts[[key]])),
        collapse = "-"
      ))
    }, character(1)),
    collapse = "_"
  )
  label <- gsub("[^A-Za-z0-9_-]", "-", label)
  if (nchar(label) > 130L) {
    label <- paste0(substr(label, 1, 100), "_", digest::digest(parts))
  }
  label
}
