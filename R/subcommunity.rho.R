#' Similarity-sensitive Normalised subcommunity.rho diversity
#' 
#' \code{subcommunity.rho()} is used to calculate the redundancy of 
#' subcommunity \emph{j}.
#' 
#' \code{subcommunity.rho()} (the inverse of \code{subcommunity.beta.bar}) 
#' calculates the subcommunity rho diversity of a series of columns  
#' representing independent subcommunities counts relative to a total 
#' supercommunity (by default the sum of the subcommunities), for a series of 
#' orders, repesented as a vector of \code{qs}.
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
#' subcommunity.rho(data, 0:2)
#' 
subcommunity.rho <-
function(populations, qs, ...)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  res <- 1 / subcommunity.beta(populations, qs, ...)

  output <- new('rdiv', res, measure = 'Subcommunity rho',
            tag = bquote('Subcommunity' ~ rho),
            level = 'subcommunity')
  return(output) 
}