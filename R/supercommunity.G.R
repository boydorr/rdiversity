#' Similarity-sensitive Raw supercommunity.G diversity
#' 
#' Calculates the total supercommunity gamma diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param normalise logical operator; T returns normalised probability 
#' distribution summed to 1
#' @return array of diversities, last representing values of \emph{q}
#' 
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace=T),
#'                         subcommunityB = sample(1:50, 5, replace=T))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # q-parameter 
#' qs <- 0:2
#' 
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' supercommunity.G(data, qs)
#' 
supercommunity.G <-
function(populations, qs, normalise = F)
{
  Z = populations@zmatrix
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  # If it's a dataframe make it a matrix
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # Turn all columns into proportions if needed
  ds <- subcommunity.gamma(populations, qs, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
  res <- data.frame(res)

  output <- new('rdiv', res, measure = 'Supercommunity G',
            tag = bquote('Supercommunity' ~ italic('G')),
            level = 'supercommunity')
  return(output) 
}