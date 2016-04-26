#' Similarity-sensitive Normalised supercommunity.R diversity
#' 
#' \code{supercommunity.R.bar()} is used to calculate the 
#' average representativeness of subcommunities.
#' 
#' \code{supercommunity.R.bar()} calculates the total noramlised 
#' supercommunity rho diversity of a series of columns representing 
#' subcommunity counts, for a series of orders, repesented as a vector of 
#' \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' 
#' @details 
#' 
#' @return An array of diversities, last representing values of \emph{q}
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
#' supercommunity.R.bar(data, 0:2)
#' 
supercommunity.R.bar <-
function(populations, qs)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  res <- supercommunity.R(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity R bar',
            tag = bquote('Supercommunity' ~ bar(italic('R'))),
            level = 'supercommunity')
  return(output) 
}