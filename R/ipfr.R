#' ipfr: A package to perform iterative proportional fitting
#' 
#' The main function is \code{\link{ipu}}. For a 2D/matrix problem, the 
#' \code{\link{ipu_matrix}} function is easier to use. The resulting
#' \code{weight_tbl} from \code{ipu()} can be fed into \code{\link{synthesize}}
#' to generate a synthetic population
#' 
#' @docType package
#' @importFrom utils type.convert
#' @importFrom stats setNames
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @name ipfr
"_PACKAGE"