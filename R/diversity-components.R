#' Low level diversity components: alpha
#' 
#' Calculates the low-level diversity component necessary for calculating alpha
#' diversity.
#'
#' Values generated from \code{raw.alpha()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate raw subcommunity/supercommunity alpha diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate raw alpha component
#' a <- raw.alpha(super)
#' 
raw.alpha <- function(super) {
  results <- 1 / super@ordinariness
  powermean(results, super, "raw alpha")
}


#' Low level diversity components: normalised alpha
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised alpha diversity.
#'
#' Values generated from \code{normalised.alpha()} may be input into \code{subdiv()} 
#' and \code{superdiv()} to calculate normalised subcommunity/supercommunity 
#' alpha diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised alpha component
#' a <- normalised.alpha(super)
#' 
normalised.alpha <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(super)
  results <- 1 / ordinariness.bar
  powermean(results, super, "normalised alpha")
}


#' Low level diversity components: raw rho
#' 
#' Calculates the low-level diversity component necessary for calculating raw rho
#' diversity.
#'
#' Values generated from \code{raw.rho()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate raw subcommunity/supercommunity rho diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate raw rho component
#' a <- raw.rho(super)
#' 
raw.rho <- function(super) {
  results <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  powermean(results, super, "raw rho")
}


#' Low level diversity components: normalised rho
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised rho diversity.
#'
#' Values generated from \code{normalised.rho()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate normalised subcommunity/supercommunity rho 
#' diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised rho component
#' a <- normalised.rho(super)
#' 
normalised.rho <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(super)
  results <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, super, "normalised rho")
}


#' Low level diversity components: raw beta
#' 
#' Calculates the low-level diversity component necessary for calculating raw beta
#' diversity.
#'
#' Values generated from \code{raw.beta()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate raw subcommunity/supercommunity beta diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{relativeentropy}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate raw beta component
#' a <- raw.beta(super)
#' 
raw.beta <- function(super) {
  rho <- rowSums(super@ordinariness, na.rm = T) / super@ordinariness
  results <- 1 / rho
  relativeentropy(results, super, "raw beta")
}


#' Low level diversity components: normalised beta
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised beta diversity.
#'
#' Values generated from \code{normalised.beta()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate normalised subcommunity/supercommunity beta 
#' diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{relativeentropy}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate normalised beta component
#' a <- normalised.beta(super)
#' 
normalised.beta <- function(super) {
  ordinariness.bar <- sapply(seq_along(super@subcommunity_weights), 
                             function(x) super@ordinariness[,x] /
                               super@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(super)
  normalised.rho <- rowSums(super@ordinariness, na.rm = T) / ordinariness.bar
  results <- 1 / normalised.rho
  relativeentropy(results, super, "normalised beta")
}


#' Low level diversity components: gamma
#' 
#' Calculates the low-level diversity component necessary for calculating gamma
#' diversity.
#'
#' Values generated from \code{raw.gamma()} may be input into \code{subdiv()} and
#' \code{superdiv()} to calculate subcommunity/supercommunity gamma diversity.
#' 
#' @param super object of class \code{supercommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.supercommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate gamma component
#' a <- raw.gamma(super)
#' 
raw.gamma <- function(super) {
  results <- rowSums(super@ordinariness, na.rm=T)
  results[results==0] <- NaN
  results <- 1 / results
  results <- matrix(rep((results), ncol(super@type_abundance)), 
                    ncol=ncol(super@type_abundance))
  colnames(results) <- colnames(super@type_abundance)
  row.names(results) <- row.names(super@type_abundance)
  powermean(results, super, "gamma")
}

