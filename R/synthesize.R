#' Creates a synthetic population based on ipu results
#' 
#' A simple function that takes the \code{weight_tbl} output from
#' \code{\link{ipu}} and randomly samples based on the weight.
#' 
#' @inheritParams ipu
#' @param weight_tbl the \code{data.frame} of the same name output by
#'   \code{\link{ipu}}.
#' @param group_by if provided, the \code{data.frame} will be grouped by this
#'   variable before sampling. If not provided, tidyverse/dplyr groupings will
#'   be respected. If no grouping info is present, samples are drawn from the
#'   entire table.
#' @return A \code{data.frame} with one record for each synthesized member of
#'   the population (e.g. household). A \code{new_id} column is created, but
#'   the previous \code{primary_id} column is maintained to facilitate joining
#'   back to other data sources (e.g. a person attribute table).
#' @export
#' @examples 
#' hh_seed <- dplyr::tibble(
#' id = c(1, 2, 3, 4),
#' siz = c(1, 2, 2, 1),
#' weight = c(1, 1, 1, 1),
#' geo_cluster = c(1, 1, 2, 2)
#' )
#' hh_targets <- list()
#' hh_targets$siz <- dplyr::tibble(
#'   geo_cluster = c(1, 2),
#'   `1` = c(75, 100),
#'   `2` = c(25, 150)
#' )
#' result <- ipu(hh_seed, hh_targets, max_iterations = 5)
#' synthesize(result$weight_tbl, "geo_cluster")

synthesize <- function(weight_tbl, group_by = NULL, primary_id = "id") {
  
  if (!primary_id %in% colnames(weight_tbl)) {
    stop("primary_id not found in weight_tbl") # nocov
  }
  
  if (!is.null(group_by)) {
    if (!group_by %in% colnames(weight_tbl)) {
      stop("group_by not found in weight_tbl") # nocov
    }
    weight_tbl <- weight_tbl %>%
      group_by(!!as.name(group_by))
  }
  
  synthetic_table <- weight_tbl %>%
    sample_n(round(sum(weight), 0), replace = TRUE, weight = weight) %>%
    ungroup() %>%
    select(-weight, -avg_weight, -weight_factor) %>%
    mutate(new_id = seq(1, n())) %>%
    select(new_id, one_of(primary_id), everything())
  
  return(synthetic_table)
}
