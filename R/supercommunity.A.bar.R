#' Similarity-sensitive Normalised supercommunity.A diversity
#' 
#' Calculates the total supercommunity alpha diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return An array of diversities, last representing values of q
#' @seealso \code{\link{supercommunity.A, subcommunity.alpha, subcommunity.alpha.bar}}
#' @export
#' 
supercommunity.A.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.A(populations, qs, Z, normalise = T)
  
  attr(res, 'measure') <- 'Supercommunity alpha bar'
  attr(res, 'tag') <- bquote('Supercommunity' ~ bar(italic(A)))
  attr(res, 'level') <- 'supercommunity'
  return(res) 
}
