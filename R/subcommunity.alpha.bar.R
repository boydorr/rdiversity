#' Similarity-sensitive Normalised subcommunity.alpha
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of qs
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return An array of diversities, first dimension representing 
#' sub-communities, and last representing values of q
#' 
subcommunity.alpha.bar <- 
function(populations, qs, Z = diag(nrow(populations))) 
{
  res <- subcommunity.alpha(populations, qs, Z, normalise = T)

  attr(res, 'name') <- 'Subcommunity alpha bar'
  attr(res, 'tag') <- bquote('Subcommunity' ~ bar(alpha))
  return(res) 
}
