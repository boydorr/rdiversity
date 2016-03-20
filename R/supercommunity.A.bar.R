#' Similarity-sensitive Normalised supercommunity.A diversity
#' 
#' Calculates the total supercommunity alpha diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
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
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' supercommunity.A.bar(data, 0:2)
#' 
supercommunity.A.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- supercommunity.A(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity A bar',
            tag = bquote('Supercommunity' ~ bar(italic('A'))),
            level = 'supercommunity')
  return(output) 
}
