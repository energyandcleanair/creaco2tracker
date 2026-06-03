architecture_get_boundary_config <- function() {
  layer_file_patterns <- list(
    client = "^client_[a-z0-9_]+\\.R$",
    access = "^access_[a-z0-9_]+\\.R$",
    workflow = "^workflow_[a-z0-9_]+\\.R$",
    model = "^model_[a-z0-9_]+\\.R$",
    quality = "^quality_[a-z0-9_]+\\.R$",
    viz = "^viz_[a-z0-9_]+\\.R$",
    core = "^core_[a-z0-9_]+\\.R$",
    research = "^research_[a-z0-9_]+\\.R$"
  )

  layer_names <- names(layer_file_patterns)
  r_dir <- testthat::test_path("..", "..", "R")
  all_r_files <- if (dir.exists(r_dir)) {
    basename(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))
  } else {
    character(0)
  }

  list(
    layer_names = layer_names,
    layer_file_patterns = layer_file_patterns,
    data_access_files = sort(all_r_files[grepl(layer_file_patterns$access, all_r_files, perl =
      TRUE)]),
    client_files = sort(all_r_files[grepl(layer_file_patterns$client, all_r_files, perl = TRUE)]),
    front_layer_files = c(
      "model_co2.R",
      "model_demand_components.R",
      "model_corrected_demand.R",
      "model_gas_demand.R",
      "model_power_generation.R",
      "workflow_update.R"
    ),
    forbidden_front_patterns = c(
      "apply_source_data_mask\\s*\\(",
      "creahelpers::api.get\\s*\\(",
      "entsoe\\.get_power_generation\\s*\\(",
      "ember\\.get_power_generation\\s*\\(",
      "collect_oil\\s*\\(",
      "collect_solid\\s*\\(",
      "collect_gas\\s*\\(",
      "get_weather\\s*\\(",
      "agsi\\.get_storage_change\\s*\\(",
      "eurostat::get_eurostat\\s*\\("
    )
  )
}
