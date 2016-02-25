#' Similarity-sensitive Normalised supercommunity.R diversity
#' 
#' Calculates the total supercommunity.R.bar diversity of a series of 
#' columns representing subcommunity counts, for a series of orders, 
#' repesented as a vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return An array of diversities, last representing values of q
#' 
supercommunity.R.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.R(populations, qs, Z, normalise = T)
  
  attr(res, 'measure') <- 'Supercommunity rho bar'
  attr(res, 'tag') <- bquote('Supercommunity' ~ bar(italic(R)))
  attr(res, 'level') <- 'supercommunity'
  return(res) 
}