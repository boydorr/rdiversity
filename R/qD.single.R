#' Hill number / naive diversity of a single population
#' 
#' Calculates the Hill number (naive diversity) of order \emph{q} of a single
#' population with given relative proportions. 
#'
#' @param proportions \code{vector} of mode \code{numeric}; contains the 
#' relative proportions of different individuals/types in a population.
#' @param q object of class \code{numeric}; contains the order of diversity 
#' measurement.
#' 
#' @return Returns the naive divesity of order \emph{q}.
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' qD.single(pop, 0)
#' 
qD.single <-
function(proportions, q) {
  proportions <- check_partition(proportions)
  1 / power.mean(values = proportions, order = q - 1, weights = proportions)
}
