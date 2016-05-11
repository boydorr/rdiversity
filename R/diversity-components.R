#' Low level diversity components: alpha
#' 
#' Calculates the low-level diversity component necessary for calculating alpha
#' diversity.
#'
#' Values generated from \code{alpha()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte raw subcommunity/supercommunity alpha diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate alpha component
#' a <- alpha(super)
#' 
alpha <- function(super) {
  results <- 1 / super@ordinariness
  powermean(results, super, "alpha")
}


#' Low level diversity components: normalised alpha
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised alpha diversity.
#'
#' Values generated from \code{alphabar()} may be input into \code{subdiv()} 
#' and \code{superdiv} to calcualte normalised subcommunity/supercommunity 
#' alpha diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised alpha component
#' a <- alphabar(super)
#' 
alphabar <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- 1 / ordinariness.bar
  powermean(results, super, "alpha bar")
}


#' Low level diversity components: rho
#' 
#' Calculates the low-level diversity component necessary for calculating rho
#' diversity.
#'
#' Values generated from \code{rho()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte raw subcommunity/supercommunity rho diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate rho component
#' a <- rho(super)
#' 
rho <- function(super) {
  results <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  powermean(results, super, "rho")
}


#' Low level diversity components: normalised rho
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised rho diversity.
#'
#' Values generated from \code{rhobar()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte normalised subcommunity/supercommunity rho 
#' diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised rho component
#' a <- rhobar(super)
#' 
rhobar <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  colnames(ordinariness.bar) <- colnames(super)
  results <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, super, "rho bar")
}


#' Low level diversity components: gamma
#' 
#' Calculates the low-level diversity component necessary for calculating beta
#' diversity.
#'
#' Values generated from \code{beta()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte raw subcommunity/supercommunity beta diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate beta component
#' a <- beta(super)
#' 
beta <- function(super) {
  results <- 1 / rho(super)
  relativeentropy(results, super, "beta")
}


#' Low level diversity components
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised beta diversity.
#'
#' Values generated from \code{betabar()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte normalised subcommunity/supercommunity beta 
#' diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised beta component
#' a <- betabar(super)
#' 
betabar <- function(super) {
  results <- 1 / rhobar(super)
  relativeentropy(results, super, "beta bar")
}


#' Low level diversity components
#' 
#' Calculates the low-level diversity component necessary for calculating gamma
#' diversity.
#'
#' Values generated from \code{gamma()} may be input into \code{subdiv()} and
#' \code{superdiv} to calcualte subcommunity/supercommunity gamma diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns 
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate gamma component
#' a <- gamma(super)
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

