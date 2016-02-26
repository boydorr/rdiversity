#' Similarity-sensitive Normalised supercommunity.G diversity
#' 
#' Calculates the total supercommunity gamma diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return array of diversities, last representing values of q
#' @seealso \code{\link{supercommunity.G}}, \code{\link{subcommunity.gamma}}, \code{\link{subcommunity.gamma.bar}}
#' @export
#' 
supercommunity.G.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.G(populations, qs, Z, normalise = T)
  
  structure(res, 
            measure = 'Supercommunity G bar',
            tag = bquote('Supercommunity' ~ bar(italic('G'))),
            level = 'supercommunity')
  return(res) 
}
