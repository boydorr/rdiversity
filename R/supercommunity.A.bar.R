#' Similarity-sensitive Normalised supercommunity.A diversity
#' 
#' Calculates the total supercommunity alpha diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @return An array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.A}}, \code{\link{subcommunity.alpha}}, \code{\link{subcommunity.alpha.bar}}
#' @export
#' 
supercommunity.A.bar <-
function(populations, qs)
{
  Z = populations@zmatrix
  res <- supercommunity.A(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity A bar',
            tag = bquote('Supercommunity' ~ bar(italic('A'))),
            level = 'supercommunity')
  return(output) 
}
