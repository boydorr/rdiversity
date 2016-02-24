#' Similarity-sensitive Normalised subcommunity.rho diversity
#' 
#' The inverse of the similarity-sensitive Normalised subcommunity.beta 
#' diversity; Calculates the diversity of a series of columns representing 
#' independent subcommunities counts relative to a total supercommunity (by 
#' default the sum of the sub-communities), for a series of orders, repesented 
#' as a vector of qs.
#'
#' @param populations Population counts or proportions - single vector or matrix
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param ...
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of q
#' 
subcommunity.rho <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
    1 / subcommunity.beta(populations, qs, Z, ...)
  
  attr(res, 'name') <- 'Subcommunity rho'
  attr(res, 'tag') <- bquote('Subcommunity' ~ rho)
  attr(res, 'type') <- 'subcommunity'
  return(res) 
}