#' Similarity-sensitive Raw subcommunity.rho diversity
#' 
#' The inverse of the similarity-sensitive Raw subcommunity.beta diversity;
#' Calculates the diversity of a series of columns representing independent
#' subcommunities counts relative to a total supercommunity (by default the
#' sum of the subcommunities), for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
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
#' # Coerse object into a supercommunity
#' data <- as.supercommunity(population)
#' 
#' # Calculate diversity
#' subcommunity.rho.bar(data, 0:2)
#' 
subcommunity.rho.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- 1 / subcommunity.beta.bar(populations, qs)

  output <- new('rdiv', res, measure = 'Subcommunity rho bar',
            tag = bquote('Subcommunity' ~ bar(rho)),
            level = 'subcommunity')
  return(output) 
}