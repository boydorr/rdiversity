#' Similarity-sensitive Raw supercommunity.B diversity
#' 
#' Calculates the total supercommunity beta diversity of a series of columns
#' representing subcommunity counts, for a series of orders, repesented as a 
#' vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' @param normalise logical operator; T returns normalised probability 
#' distribution summed to 1
#' 
#' @details 
#' 
#' @return An array of diversities, last representing values of \emph{q}
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Coerse object into a supercommunity
#' data <- as.supercommunity(population)
#' 
#' # Calculate diversity
#' supercommunity.B(data, 0:2)
#' 
supercommunity.B <-
function(populations, qs, normalise = F)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  Z = populations@similarity
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # Turn all columns into proportions if needed
  ds <- subcommunity.beta(populations, qs, normalise)
  
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