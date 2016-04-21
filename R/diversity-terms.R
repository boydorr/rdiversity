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


#' @rdname alpha
alphabar <- function(super) {
  ordinariness.bar <- sapply(1:length(super@subcommunity_weight), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weight[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- 1 / ordinariness.bar
  
  new('powermean', results, 
      measure = "alpha bar")
}


#' @rdname alpha
rho <- function(super) {
  results <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  
  new('powermean', results, 
      measure = "rho")
}


#' @rdname alpha
rhobar <- function(super) {
  ordinariness.bar <- sapply(1:length(super@subcommunity_weight), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weight[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  
  new('powermean', results, 
      measure = "rho bar")
}
#' 
#' 
#' #' @rdname alpha
#' beta <- function(super)
#'   1 / rho(super)
#' 
#' 
#' #' @rdname alpha
#' betabar <- function(super)
#'   1 / rhobar(super)


#' @rdname alpha
gamma <- function(super) {
  results <- 1 / rowSums(super@ordinariness, na.rm = T)
  
  new('powermean', results, 
      measure = "gamma")
}

