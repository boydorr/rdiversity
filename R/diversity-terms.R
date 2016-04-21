#' Individual diversity term
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of \code{qs}.
#'
#' @param super object of class \S4class{supercommunity}.
#' 
#' @details 
#' 
#' @return Returns 
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
  powermean(results, super, "alpha")
}


#' @rdname alpha
#' 
alphabar <- function(super) {
  ordinariness.bar <- sapply(1:length(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- 1 / ordinariness.bar
  powermean(results, super, "alpha bar")
}


#' @rdname alpha
#' 
rho <- function(super) {
  results <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  powermean(results, super, "rho")
}


#' @rdname alpha
#' 
rhobar <- function(super) {
  ordinariness.bar <- sapply(1:length(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, super, "rho bar")
}


#' @rdname alpha
#' 
beta <- function(super) {
  results <- 1 / rho(super)
  relativeentropy(results, super, "beta")
}


#' @rdname alpha
#' 
betabar <- function(super) {
  results <- 1 / rhobar(super)
  relativeentropy(results, super, "beta bar")
}


#' @rdname alpha
#' 
gamma <- function(super) {
  results <- 1 / rowSums(super@ordinariness, na.rm = T)
  powermean(results, super, "gamma")
}







