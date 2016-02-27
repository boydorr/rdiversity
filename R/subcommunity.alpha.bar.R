#' Similarity-sensitive Normalised subcommunity.alpha
#' 
#' \code{subcommunity.alpha} calculates the diversity of a series of columns 
#' representing independent subcommunity counts, for a series of orders, 
#' repesented as a vector of \code{qs}.
#' 
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter \emph{q}
#' @param Z Similarity matrix
#'
#' @return An array of diversities, first dimension representing 
#' sub-communities, and last representing values of \emph{q}
#' @seealso \code{\link{subcommunity.alpha}}, \code{\link{supercommunity.A}}, \code{\link{supercommunity.A.bar}}
#' @export
#' 
subcommunity.alpha.bar <- 
function(populations, qs, Z = diag(nrow(populations))) 
{
  res <- subcommunity.alpha(populations, qs, Z, normalise = T)
  res <- structure(res,
            measure = 'Subcommunity alpha bar',
            tag = bquote('Subcommunity' ~ bar(alpha)),
            level = 'subcommunity')
  return(res) 
}
