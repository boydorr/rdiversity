#' Similarity-sensitive Normalised supercommunity.G diversity
#' 
#' Calculates the total supercommunity gamma diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.G}}, \code{\link{subcommunity.gamma}}, \code{\link{subcommunity.gamma.bar}}
#' @export
#' 
supercommunity.G.bar <-
function(populations, qs, Z = diag(nrow(populations)))
{
  res <- supercommunity.G(populations, qs, Z, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity G bar',
            tag = bquote('Supercommunity' ~ bar(italic('G'))),
            level = 'supercommunity')
  return(output) 
}
