#' Get documented coal-coking source conflicts
#'
#' SDES reports continued operation of French coke ovens in 2023 and 2024.
#' These source-derived exceptions are static package data: model code consumes
#' the structured evidence without embedding source URLs.
#'
#' @return A tibble describing affected source observations and their evidence.
#' @keywords internal
get_coal_coking_conflicts <- function() {
  tibble::tibble(
    iso2 = "FR",
    year_from = 2021L,
    year_to = 2024L,
    frequency = "annual",
    conflicting_value = 0,
    source = paste0(
      "https://www.statistiques.developpement-durable.gouv.fr/",
      "media/9074/download?inline="
    ),
    supporting_source = paste0(
      "https://www.statistiques.developpement-durable.gouv.fr/",
      "media/9087/download?inline="
    )
  )
}
