#' Similarity-sensitive Normalised subcommunity.alpha
#' 
#' \code{subcommunity.alpha} calculates the diversity of a series of columns 
#' representing independent subcommunity counts, for a series of orders 
#' repesented as a vector of \code{qs}.
#' 
#' @inheritParams inherit_params
#' @return An array of diversities, first dimension representing 
#' sub-communities, and last representing values of \emph{q}
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
#' subcommunity.alpha.bar(data, qs)
#' 
subcommunity.alpha.bar <- 
function(populations, qs) 
{
  Z = populations@zmatrix
  res <- subcommunity.alpha(populations, qs, normalise = T)
  res <- data.frame(res)
  
  output <- new('rdiv', res, measure = 'Subcommunity alpha bar',
            tag = bquote('Subcommunity' ~ bar(alpha)),
            level = 'subcommunity')
  return(output) 
}
