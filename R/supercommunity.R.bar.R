#' Similarity-sensitive Normalised supercommunity.R diversity
#' 
#' Calculates the total supercommunity.R.bar diversity of a series of 
#' columns representing subcommunity counts, for a series of orders, 
#' repesented as a vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return An array of diversities, last representing values of \emph{q}
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
#' supercommunity.R.bar(data, qs)
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