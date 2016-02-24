#' Similarity-sensitive Normalised supercommunity.G diversity
#' 
#' Calculates the total supercommunity gamma diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param normalise Normalise probability distribution to sum to 1
#'
#' @return array of diversities, last representing values of q
#' 
supercommunity.G.bar <-
function(populations, qs, Z = diag(nrow(populations)))
  supercommunity.G(populations, qs, Z, normalise = T)
