#' Similarity-sensitive Normalised subcommunity.beta diversity
#' 
#' Calculates the diversity of a series of columns representing independent
#' subcommunities counts relative to a total supercommunity (by default the 
#' sum of the sub-communities), for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param ... additional arguments
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
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
#' subcommunity.beta.bar(data, 0:2)
#' 
subcommunity.beta.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- subcommunity.beta(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Subcommunity beta bar',
            tag = bquote('Subcommunity' ~ bar(beta)),
            level = 'subcommunity')
  return(output) 
  }