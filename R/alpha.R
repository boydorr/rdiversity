#' Individual diversity term
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' @param super 
#' @return Returns 
#' 
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
#' subcommunity.alpha(data, 0:2)
#' 
alpha <- function(super) {
  results <- 1 / super@ordinariness
  
  new('powermean', results, 
      measure = "alpha")
}

  