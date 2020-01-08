#' Create the ASU example
#' 
#' Sets up the Arizona example IPU problem and is used in multiple places
#' throughout the package (vignettes/tests).
#' @return A list of four variables:
#'   hh_seed, hh_targets, per_seed, and per_targets. These can be used directly
#'   by \code{\link{ipu}}.
#' @export
#' @examples
#' setup_arizona()

setup_arizona <- function() {
  hh_seed <- tibble(
    id = c(1:8),
    hhtype = c(1, 1, 1, 2, 2, 2, 2, 2)
  )
  per_seed <- tibble(
    id = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 8, 8),
    pertype = c(1, 2, 3, 1, 3, 1, 1, 2, 1, 3, 3, 2, 2, 3, 1, 2, 1, 1, 2, 3, 3, 1, 2)
  )
  hh_targets <- list()
  hh_targets$hhtype <- tibble(
    `1` = 35,
    `2` = 65
  )
  per_targets <- list()
  per_targets$pertype <- tibble(
    `1` = 91,
    `2` = 65,
    `3` = 104
  )
  
  result <- list()
  result$hh_seed <- hh_seed
  result$per_seed <- per_seed
  result$hh_targets <- hh_targets
  result$per_targets <- per_targets
  return(result)
}
