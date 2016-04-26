#' Similarity-sensitive Normalised subcommunity.gamma diversity
#' 
#' \code{subcommunity.gamma()} is used to calculate the 
#' contribution per individual toward supercommunity diversity.
#' 
#' \code{subcommunity.gamma()} calculates the subcommunity gamma diversity of 
#' a series of columns representing independent subcommunities counts relative 
#' to a total supercommunity (by default the sum of the subcommunities), for 
#' a series of orders, repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' @param ... additional arguments
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
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
#' data <- supercommunity(population)
#' 
#' # Calculate diversity
#' subcommunity.gamma.bar(data, 0:2)
#' 
subcommunity.gamma.bar <-
function(populations, qs, ...)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  res <- subcommunity.gamma(populations, qs, ..., normalise = T)
  
  output <- new('rdiv', res, measure = 'Subcommunity gamma bar',
            tag = bquote('Subcommunity' ~ bar(gamma)),
            level = 'subcommunity')
  return(output) 
}
