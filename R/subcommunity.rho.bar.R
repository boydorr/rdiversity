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
#' @seealso \code{\link{subcommunity.rho}}, \code{\link{supercommunity.R}}, \code{\link{supercommunity.R.bar}}
#' @export
#' 
subcommunity.rho.bar <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
  res <- 1 / subcommunity.beta.bar(populations, qs, Z, ...)
  
  res <- structure(res,
            measure = 'Subcommunity rho bar',
            tag = bquote('Subcommunity' ~ bar(rho)),
            level = 'subcommunity')
  return(res) 
}