#' Similarity-sensitive Raw supercommunity.A diversity
#' 
#' Calculates the total supercommunity alpha diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param normalise Normalise probability distribution to sum to 1
#' @return An array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.A.bar}}, \code{\link{subcommunity.alpha}}, \code{\link{subcommunity.alpha.bar}}
#' @export
#' 
supercommunity.A <-
function(populations, qs, Z = diag(nrow(populations)),
                        normalise = F)
{
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # Turn all columns into proportions if needed
  ds <- subcommunity.alpha(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
  
  structure(res,
            measure = 'Supercommunity A',
            tag = bquote('Supercommunity' ~ italic('A')),
            level = 'supercommunity')
  return(res) 
}