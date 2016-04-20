#' Similarity-sensitive Normalised supercommunity.R diversity
#' 
#' Calculates the total supercommunity.R.bar diversity of a series of 
#' columns representing subcommunity counts, for a series of orders, 
#' repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' @return An array of diversities, last representing values of \emph{q}
#' 
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
#' supercommunity.R.bar(data, 0:2)
#' 
supercommunity.R.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- supercommunity.R(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity R bar',
            tag = bquote('Supercommunity' ~ bar(italic('R'))),
            level = 'supercommunity')
  return(output) 
}