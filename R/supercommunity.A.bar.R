#' Similarity-sensitive Normalised supercommunity.A diversity
#' 
#' Calculates the total supercommunity alpha diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' 
#' @details \code{subcommunity.A.bar()} is used to calculate the 
#' average similarity-sensitive diversity of subcommunities.
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
#' data <- as.supercommunity(population)
#' 
#' # Calculate diversity
#' supercommunity.A.bar(data, 0:2)
#' 
supercommunity.A.bar <-
function(populations, qs)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  res <- supercommunity.A(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity A bar',
            tag = bquote('Supercommunity' ~ bar(italic('A'))),
            level = 'supercommunity')
  return(output) 
}
