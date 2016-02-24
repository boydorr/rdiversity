#' Similarity-sensitive Normalised subcommunity.beta diversity
#' 
#' Calculates the diversity of a series of columns representing independent
#' sub-communities counts relative to a total supercommunity (by default the 
#' sum of the sub-communities), for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions - single vector or matrix
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param ... 
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of q
#' 
subcommunity.beta.bar <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
  res <- subcommunity.beta(populations, qs, Z, ..., normalise = T)

  attr(res, 'name') <- 'Subcommunity beta bar'
  attr(res, 'tag') <- bquote('Subcommunity' ~ bar(beta))
  return(res) 
  }