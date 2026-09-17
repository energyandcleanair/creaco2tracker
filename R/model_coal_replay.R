#' Keep unaffected fuel series separate from a masked coal system
#'
#' The fuel category column remains distinct from the selected SIEC code.
#' Allocation provenance follows the same exclusion as the observations.
#' @keywords internal
#' @noRd
coal_replay_background <- function(context, selected_siec, eligible_iso2) {
  result <- context %>% filter(
    siec != .env$selected_siec | !iso2 %in% .env$eligible_iso2
  )
  allocation <- attr(context, "coal_allocation")
  if (!is.null(allocation)) {
    attr(result, "coal_allocation") <- allocation %>% filter(
      siec != .env$selected_siec | !iso2 %in% .env$eligible_iso2
    )
  }
  result
}
