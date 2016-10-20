#' Low level diversity components: alpha
#' 
#' Calculates the low-level diversity component necessary for calculating alpha
#' diversity.
#'
#' Values generated from \code{raw.alpha()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity alpha diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw alpha component
#' a <- raw.alpha(meta)
#' 
raw.alpha <- function(meta) {
  results <- 1 / meta@ordinariness
  powermean(results, meta, "raw alpha")
}


#' Low level diversity components: normalised alpha
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised alpha diversity.
#'
#' Values generated from \code{normalised.alpha()} may be input into \code{subdiv()} 
#' and \code{metadiv()} to calculate normalised subcommunity/metacommunity 
#' alpha diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised alpha component
#' a <- normalised.alpha(meta)
#' 
normalised.alpha <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights), 
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta)
  results <- 1 / ordinariness.bar
  powermean(results, meta, "normalised alpha")
}


#' Low level diversity components: raw rho
#' 
#' Calculates the low-level diversity component necessary for calculating raw rho
#' diversity.
#'
#' Values generated from \code{raw.rho()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity rho diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw rho component
#' a <- raw.rho(meta)
#' 
raw.rho <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm = T) / meta@ordinariness
  powermean(results, meta, "raw rho")
}


#' Low level diversity components: normalised rho
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised rho diversity.
#'
#' Values generated from \code{normalised.rho()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity/metacommunity rho 
#' diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised rho component
#' a <- normalised.rho(meta)
#' 
normalised.rho <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights), 
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta)
  results <- rowSums(meta@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, meta, "normalised rho")
}


#' Low level diversity components: raw beta
#' 
#' Calculates the low-level diversity component necessary for calculating raw beta
#' diversity.
#'
#' Values generated from \code{raw.beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity beta diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{relativeentropy}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw beta component
#' a <- raw.beta(meta)
#' 
raw.beta <- function(meta) {
  rho <- rowSums(meta@ordinariness, na.rm = T) / meta@ordinariness
  results <- 1 / rho
  relativeentropy(results, meta, "raw beta")
}


#' Low level diversity components: normalised beta
#' 
#' Calculates the low-level diversity component necessary for calculating 
#' normalised beta diversity.
#'
#' Values generated from \code{normalised.beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity/metacommunity beta 
#' diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{relativeentropy}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate normalised beta component
#' a <- normalised.beta(meta)
#' 
normalised.beta <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights), 
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta)
  normalised.rho <- rowSums(meta@ordinariness, na.rm = T) / ordinariness.bar
  results <- 1 / normalised.rho
  relativeentropy(results, meta, "normalised beta")
}


#' Low level diversity components: gamma
#' 
#' Calculates the low-level diversity component necessary for calculating gamma
#' diversity.
#'
#' Values generated from \code{raw.gamma()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate subcommunity/metacommunity gamma diversity.
#' 
#' @param meta object of class \code{metacommunity}.
#' 
#' @return Returns an object of class \code{powermean}.
#' @include as.metacommunity.R
#' @export
#' 
#' @examples
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate gamma component
#' a <- raw.gamma(meta)
#' 
raw.gamma <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm=T)
  results[results==0] <- NaN
  results <- 1 / results
  results <- matrix(rep((results), ncol(meta@type_abundance)), 
                    ncol=ncol(meta@type_abundance))
  colnames(results) <- colnames(meta@type_abundance)
  row.names(results) <- row.names(meta@type_abundance)
  powermean(results, meta, "gamma")
}

