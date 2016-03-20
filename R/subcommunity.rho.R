#' Similarity-sensitive Normalised subcommunity.rho diversity
#' 
#' The inverse of the similarity-sensitive Normalised subcommunity.beta 
#' diversity; Calculates the diversity of a series of columns representing 
#' independent subcommunities counts relative to a total supercommunity (by 
#' default the sum of the subcommunities), for a series of orders, repesented 
#' as a vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param ... additional arguments
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
#' 
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace=T),
#'                         subcommunityB = sample(1:50, 5, replace=T))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # q-parameter 
#' qs <- 0:2
#' 
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' subcommunity.rho(data, qs)
#' 
subcommunity.rho <-
function(populations, qs, ...)
{
  Z = populations@zmatrix
  res <- 1 / subcommunity.beta(populations, qs, ...)

  output <- new('rdiv', res, measure = 'Subcommunity rho',
            tag = bquote('Subcommunity' ~ rho),
            level = 'subcommunity')
  return(output) 
}