#' Similarity-sensitive Normalised supercommunity.B diversity
#' 
#' \code{supercommunity.B.bar()} is used to calculate the 
#' effective number of distinct subcommunities.
#' 
#' \code{supercommunity.B.bar()} calculates the total normalised supercommunity 
#' beta diversity of a series of columns representing subcommunity counts, for 
#' a series of orders, repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
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
#' data <- supercommunity(population)
#' 
#' # Calculate diversity
#' supercommunity.B.bar(data, 0:2)
#' 
supercommunity.B.bar <-
  function(populations, qs)
  {
    if(!is.supercommunity(populations))
      stop('populations must be object of class supercommunity.')
    
    res <- supercommunity.B(populations, qs, normalise = T)
    
    output <- new('rdiv', res, measure = 'Supercommunity B bar',
                  tag = bquote('Supercommunity' ~ bar(italic('B'))),
                  level = 'supercommunity')
    output
  }
