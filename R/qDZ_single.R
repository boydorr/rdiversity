#' Similarity-sensitive diversity of a single population
#' 
#' Calculates the similarity-sensitive diversity of order \emph{q} of a single  
#' population with given relative proportions.
#'
#' @param proportions \code{vector} of mode \code{numeric}; contains the 
#' relative proportions of different individuals/types in a population.
#' @param q object of class \code{numeric}; contains the order of diversity 
#' measurement.
#' @param Z two-dimensional \code{matrix} of mode \code{numeric}; contains the 
#' pair-wise similarity of individuals/types in a population.
#' @param Zp two-dimensional \code{matrix} of mode \code{numeric}; contains 
#' the ordinariness of individuals/types in population.
#' 
#' @return Returns the similarity-sensitive diversity of order \emph{q}.
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' 
#' # Create similarity matrix
#' Z <- diag(1, length(pop))
#' Z[Z==0] <- 0.4
#' 
#' # Calculate similarity-sensitive diversity of order 0 (species richness)
#' qDZ_single(pop, 0, Z)
#' 
qDZ_single <-
  function(proportions, q,
           Z = diag(nrow(proportions)),
           Zp = Z %*% proportions) {
    proportions <- check_partition(proportions)
    1 / power_mean(values = Zp, order = q - 1, weights = proportions)
  }
