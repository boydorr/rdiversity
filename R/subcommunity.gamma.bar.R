#' Similarity-sensitive Normalised subcommunity.gamma diversity
#' 
#' Calculates the diversity of a series of columns representing independent 
#' subcommunities counts relative to a total supercommunity (by default the 
#' sum of the subcommunities), for a series of orders, repesented as a  
#' vector of qs.
#'
#' @param populations Population counts or proportions; single vector or matrix
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param ...
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of q
#' @seealso \code{\link{subcommunity.gamma}}, \code{\link{supercommunity.G}}, \code{\link{supercommunity.G.bar}}
#' @export
#' 
subcommunity.gamma.bar <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
  res <- subcommunity.gamma(populations, qs, Z, ..., normalise = T)
  
  structure(res, 
            measure = 'Subcommunity gamma bar',
            tag = bquote('Subcommunity' ~ bar(gamma)),
            level = 'subcommunity')
  return(res) 
}
