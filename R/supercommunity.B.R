#' Similarity-sensitive Raw supercommunity.B diversity
#' 
#' Calculates the total supercommunity beta diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param normalise Normalise probability distribution to sum to 1
#' @return An array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{supercommunity.B.bar}}, \code{\link{subcommunity.beta}}, \code{\link{subcommunity.beta.bar}}
#' @export
#' 
supercommunity.B <-
function(populations, qs, Z = diag(nrow(populations)), normalise = F)
{
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # Turn all columns into proportions if needed
  ds <- subcommunity.beta(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
  res <- data.frame(res)
  
  output <- new('rdiv', res, measure = 'Supercommunity B',
            tag = bquote('Supercommunity' ~ italic('B')),
            level = 'supercommunity')
  return(output) 
}