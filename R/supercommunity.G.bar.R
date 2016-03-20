#' Similarity-sensitive Normalised supercommunity.G diversity
#' 
#' Calculates the total supercommunity gamma diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return array of diversities, last representing values of \emph{q}
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
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' supercommunity.G.bar(data, 0:2)
#' 
supercommunity.G.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- supercommunity.G(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity G bar',
            tag = bquote('Supercommunity' ~ bar(italic('G'))),
            level = 'supercommunity')
  return(output) 
}
