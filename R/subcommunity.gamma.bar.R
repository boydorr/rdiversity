#' Similarity-sensitive Normalised subcommunity.gamma diversity
#' 
#' Calculates the diversity of a series of columns representing independent 
#' subcommunities counts relative to a total supercommunity (by default the 
#' sum of the subcommunities), for a series of orders, repesented as a  
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param ... additional arguments
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
#' 
#' @seealso \code{\link{subcommunity.gamma}}, \code{\link{supercommunity.G}}, \code{\link{supercommunity.G.bar}}
#' @export
#' 
subcommunity.gamma.bar <-
function(populations, qs, ...)
{
  Z = populations@zmatrix
  res <- subcommunity.gamma(populations, qs, ..., normalise = T)
  
  output <- new('rdiv', res, measure = 'Subcommunity gamma bar',
            tag = bquote('Subcommunity' ~ bar(gamma)),
            level = 'subcommunity')
  return(output) 
}
