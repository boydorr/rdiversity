#' Similarity-sensitive Normalised supercommunity.R diversity
#' 
#' Calculates the total supercommunity.R.bar diversity of a series of 
#' columns representing subcommunity counts, for a series of orders, 
#' repesented as a vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return An array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.R}}, \code{\link{subcommunity.rho}}, \code{\link{subcommunity.rho.bar}}
#' @export
#' 
supercommunity.R.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.R(populations, qs, Z, normalise = T)
  
  res <- structure(res, 
            measure = 'Supercommunity R bar',
            tag = bquote('Supercommunity' ~ bar(italic('R'))),
            level = 'supercommunity')
  return(res) 
}