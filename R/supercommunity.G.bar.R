#' Similarity-sensitive Normalised supercommunity.G diversity
#' 
#' \code{supercommunity.G()} is used to calculate the 
#' supercommunity similarity-sensitive diversity.
#' 
#' \code{supercommunity.G()} calculates the total supercommunity gamma diversity 
#' of a series of columns representing subcommunity counts, for a series of 
#' orders, repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' 
#' @return array of diversities, last representing values of \emph{q}
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
#' data <- supercommunity(population)
#' 
#' # Calculate diversity
#' supercommunity.G.bar(data, 0:2)
#' 
supercommunity.G.bar <-
function(populations, qs)
{
  if(!is.supercommunity(populations))
    stop('populations must be object of class supercommunity.')
  
  res <- supercommunity.G(populations, qs, normalise = T)

  output <- new('rdiv', res, measure = 'Supercommunity G bar',
            tag = bquote('Supercommunity' ~ bar(italic('G'))),
            level = 'supercommunity')
  return(output) 
}
