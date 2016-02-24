#' Similarity-sensitive Normalised supercommunity.B diversity
#' 
#' Calculates the total supercommunity beta diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param normalise Normalise probability distribution to sum to 1
#'
#' @return An array of diversities, last representing values of q
#' 
supercommunity.B.bar <-
structure(function(populations, qs, Z = diag(nrow(populations)))
  supercommunity.B(populations, qs, Z, normalise = T), 
  class = "diversity", name = "supercommunity.B.bar")
