#' Similarity-sensitive Normalised supercommunity.B diversity
#' 
#' Calculates the total supercommunity beta diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of qs.
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return An array of diversities, last representing values of q
#' @seealso \code{\link{supercommunity.B}}, \code{\link{subcommunity.beta}}, \code{\link{subcommunity.beta.bar}}
#' @export
#' 
supercommunity.B.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.B(populations, qs, Z, normalise = T)
  
  structure(res, 
            measure = 'Supercommunity B bar',
            tag = bquote('Supercommunity' ~ bar(italic('B'))),
            level = 'supercommunity')
  return(res) 
}
