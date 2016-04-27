#' Similarity-sensitive Raw subcommunity.rho diversity
#' 
#' \code{subcommunity.rho.bar()} is used to calculate the 
#' representativeness of subcommunity \emph{j}.
#' 
#' \code{subcommunity.rho.bar()} (the inverse of \code{subcommunity.beta}) 
#' calculates the normalised subcommunity rho diversity of a series of columns 
#' representing independent subcommunities counts relative to a total 
#' supercommunity (by default the sum of the subcommunities), for a series of 
#' orders, repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
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
#' subcommunity.rho.bar(data, 0:2)
#' 
subcommunity.rho.bar <-
  function(populations, qs)
  {
    if(!is.supercommunity(populations))
      stop('populations must be object of class supercommunity.')
    
    res <- 1 / subcommunity.beta.bar(populations, qs)
    
    output <- new('rdiv', res, measure = 'Subcommunity rho bar',
                  tag = bquote('Subcommunity' ~ bar(rho)),
                  level = 'subcommunity')
    output
  }