#' Similarity-sensitive Raw supercommunity.G diversity
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
#' @seealso \code{\link{supercommunity.G.bar}}, \code{\link{subcommunity.gamma}}, \code{\link{subcommunity.gamma.bar}}
#' @export
#' 
supercommunity.G <-
function(populations, qs, Z = diag(nrow(populations)),  normalise = F)
{
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  # If it's a dataframe make it a matrix
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # Turn all columns into proportions if needed
  ds <- subcommunity.gamma(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
  
  structure(res, class = 'RDiversity',
            measure = 'Supercommunity G',
            tag = bquote('Supercommunity' ~ italic('G')),
            level = 'supercommunity')
  return(res) 
}