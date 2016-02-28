#' Similarity-sensitive Normalised subcommunity.beta diversity
#' 
#' Calculates the diversity of a series of columns representing independent
#' subcommunities counts relative to a total supercommunity (by default the 
#' sum of the sub-communities), for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param ... 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
#' 
#' @seealso \code{\link{subcommunity.beta}}, \code{\link{supercommunity.B}}, \code{\link{supercommunity.B.bar}}
#' @export
#' 
subcommunity.beta.bar <-
function(populations, qs, Z = diag(nrow(populations)), ...)
{
  res <- subcommunity.beta(populations, qs, Z, ..., normalise = T)

  structure(res, 
            measure = 'Subcommunity beta bar',
            tag = bquote('Subcommunity' ~ bar(beta)),
            level = 'subcommunity')
  return(res) 
  }