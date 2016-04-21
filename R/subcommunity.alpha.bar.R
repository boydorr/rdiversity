#' Similarity-sensitive Normalised subcommunity.alpha
#' 
#' \code{subcommunity.alpha} calculates the diversity of a series of columns 
#' representing independent subcommunity counts, for a series of orders 
#' repesented as a vector of \code{qs}.
#' 
#' @param populations object of class \code{collection}
#' @param qs \code{vector} of \emph{q} values
#' 
#' @details 
#' 
#' @return An array of diversities, first dimension representing 
#' sub-communities, and last representing values of \emph{q}
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Coerse object into a supercommunity
#' data <- as.supercommunity(population)
#' 
#' # Calculate diversity
#' subcommunity.alpha.bar(data, 0:2)
#' 
subcommunity.alpha.bar <- 
function(populations, qs) 
{
  res <- subcommunity.alpha(populations, qs, normalise = T)
  res <- data.frame(res)
  
  output <- new('rdiv', res, measure = 'Subcommunity alpha bar',
            tag = bquote('Subcommunity' ~ bar(alpha)),
            level = 'subcommunity')
  return(output) 
}
