#' Raw alpha (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating alpha
#' diversity.
#'
#' Values generated from \code{raw_alpha()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity alpha diversity.
#'
#' @param meta object of class \code{metacommunity}.
#'
#' @return Returns an object of class \code{powermean}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. 
#' arXiv 1404.6520v3:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw alpha component
#' raw_alpha(meta)
#'
raw_alpha <- function(meta) {
  results <- 1 / meta@ordinariness
  powermean(results, meta, "raw alpha")
}


#' Normalised alpha (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating
#' normalised alpha diversity.
#'
#' Values generated from \code{norm_alpha()} may be input into \code{subdiv()}
#' and \code{metadiv()} to calculate normalised subcommunity/metacommunity
#' alpha diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{powermean}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised alpha component
#' norm_alpha(meta)
#'
norm_alpha <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta@type_abundance)
  results <- 1 / ordinariness.bar
  
  powermean(results, meta, "normalised alpha")
}


#' Raw rho (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating raw rho
#' diversity.
#'
#' Values generated from \code{raw_rho()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity rho diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{powermean}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw rho component
#' raw_rho(meta)
#'
raw_rho <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm = T) / meta@ordinariness
  powermean(results, meta, "raw rho")
}


#' Normalised rho (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating
#' normalised rho diversity.
#'
#' Values generated from \code{norm_rho()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity/metacommunity rho
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{powermean}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised rho component
#' norm_rho(meta)
#'
norm_rho <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta@type_abundance)
  results <- rowSums(meta@ordinariness, na.rm = T) / ordinariness.bar
  powermean(results, meta, "normalised rho")
}


#' Raw beta (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating raw beta
#' diversity.
#'
#' Values generated from \code{raw_beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity/metacommunity beta diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{relativeentropy}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw beta component
#' raw_beta(meta)
#'
raw_beta <- function(meta) {
  rho <- rowSums(meta@ordinariness, na.rm = T) / meta@ordinariness
  results <- 1 / rho
  relativeentropy(results, meta, "raw beta")
}


#' Normalised beta (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating
#' normalised beta diversity.
#'
#' Values generated from \code{norm_beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity/metacommunity beta
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{relativeentropy}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised beta component
#' norm_beta(meta)
#'
norm_beta <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[,x] /
                               meta@subcommunity_weights[x])
  if(!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta@type_abundance)
  normalised.rho <- rowSums(meta@ordinariness, na.rm = T) / ordinariness.bar
  results <- 1 / normalised.rho
  relativeentropy(results, meta, "normalised beta")
}


#' Gamma (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating gamma
#' diversity.
#'
#' Values generated from \code{raw_gamma()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate subcommunity/metacommunity gamma diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return Returns an object of class \code{powermean}.
#' @include metacommunity.R
#' @export
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate gamma component
#' raw_gamma(meta)
#'
raw_gamma <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm=T)
  results[results==0] <- NaN
  results <- 1 / results
  results <- matrix(rep((results), ncol(meta@type_abundance)),
                    ncol=ncol(meta@type_abundance))
  colnames(results) <- colnames(meta@type_abundance)
  row.names(results) <- row.names(meta@type_abundance)
  powermean(results, meta, "gamma")
}

