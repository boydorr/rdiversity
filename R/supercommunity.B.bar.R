#' Similarity-sensitive Normalised supercommunity.B diversity
#' 
#' Calculates the total supercommunity beta diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return An array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.B}}, \code{\link{subcommunity.beta}}, \code{\link{subcommunity.beta.bar}}
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
#' supercommunity.B.bar(data, qs)
#' 
supercommunity.B.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- supercommunity.B(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity B bar',
            tag = bquote('Supercommunity' ~ bar(italic('B'))),
            level = 'supercommunity')
  return(output) 
}
