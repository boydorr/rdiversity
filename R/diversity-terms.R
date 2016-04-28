#' Individual diversity term
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of \code{qs}.
#'
#' @param super object of class \code{supercommunity}.
#' 
#' @details 
#' 
#' @return Returns 
#' @include as.supercommunity.R
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
#' subcommunity.alpha(data, 0:2)
#' 
alpha <- function(super) {
  results <- 1 / super@ordinariness
  powermean(results, super, "alpha")
}


#' @rdname alpha
#' @export
#' 
alphabar <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- 1 / ordinariness.bar
  powermean(results, super, "alpha bar")
}


#' @rdname alpha
#' @export
#' 
rho <- function(super) {
  results <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  powermean(results, super, "rho")
}


#' @rdname alpha
#' @export
#' 
rhobar <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, super, "rho bar")
}


#' @rdname alpha
#' @export
#' 
beta <- function(super) {
  results <- 1 / rho(super)
  relativeentropy(results, super, "beta")
}


#' @rdname alpha
#' @export
#' 
betabar <- function(super) {
  results <- 1 / rhobar(super)
  relativeentropy(results, super, "beta bar")
}


#' @rdname alpha
#' @export
#' 
gamma <- function(super) {
  results <- rowSums(super@ordinariness, na.rm=T)
  results[results==0] <- NaN
  results <- 1 / results
  results <- matrix(rep((results), ncol(super)), ncol=ncol(super))
  colnames(results) <- colnames(super)
  row.names(results) <- row.names(super)
  powermean(results, super, "gamma")
}







