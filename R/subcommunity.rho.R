#' Similarity-sensitive Normalised subcommunity.rho diversity
#' 
#' The inverse of the similarity-sensitive Normalised subcommunity.beta 
#' diversity; Calculates the diversity of a series of columns representing 
#' independent subcommunities counts relative to a total supercommunity (by 
#' default the sum of the subcommunities), for a series of orders, repesented 
#' as a vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param ... additional arguments
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
#' 
#' @seealso \code{\link{subcommunity.rho.bar}}, \code{\link{supercommunity.R}}, \code{\link{supercommunity.R.bar}}
#' @export
#' 
subcommunity.rho <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
  res <- 1 / subcommunity.beta(populations, qs, Z, ...)

  output <- new('rdiv', res, measure = 'Subcommunity rho',
            tag = bquote('Subcommunity' ~ rho),
            level = 'subcommunity')
  return(output) 
}