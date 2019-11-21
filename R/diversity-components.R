#' Raw alpha (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating alpha
#' diversity.
#'
#' Values generated from \code{raw_alpha()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity and metacommunity alpha
#' diversity.
#'
#' @param meta object of class \code{metacommunity}
#'
#' @return \code{raw_alpha} returns an object of class \code{powermean}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw alpha component
#' a <- raw_alpha(meta)
#' subdiv(a, 1)
#' metadiv(a, 1)
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
#' and \code{metadiv()} to calculate normalised subcommunity and metacommunity
#' alpha diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{norm_alpha} returns an object of class \code{powermean}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised alpha component
#' a <- norm_alpha(meta)
#' subdiv(a, 1)
#' metadiv(a, 1)
#'
norm_alpha <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[, x] /
                               meta@subcommunity_weights[x])
  if (!is.matrix(ordinariness.bar))
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
#' \code{metadiv()} to calculate raw subcommunity and metacommunity rho
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{raw_rho} returns an object of class \code{powermean}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw rho component
#' r <- raw_rho(meta)
#' subdiv(r, 1)
#' metadiv(r, 1)
#'
raw_rho <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm = TRUE) / meta@ordinariness
  powermean(results, meta, "raw rho")
}


#' Normalised rho (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating
#' normalised rho diversity.
#'
#' Values generated from \code{norm_rho()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity and metacommunity rho
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{norm_rho} returns an object of class \code{powermean}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised rho component
#' r <- norm_rho(meta)
#' subdiv(r, 1)
#' metadiv(r, 1)
#'
norm_rho <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[, x] /
                               meta@subcommunity_weights[x])
  if (!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta@type_abundance)
  results <- rowSums(meta@ordinariness, na.rm = TRUE) / ordinariness.bar
  powermean(results, meta, "normalised rho")
}


#' Raw beta (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating raw beta
#' diversity.
#'
#' Values generated from \code{raw_beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate raw subcommunity and metacommunity beta
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{raw_beta} returns an object of class \code{relativeentropy}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw beta component
#' b <- raw_beta(meta)
#' subdiv(b, 1)
#' metadiv(b, 1)
#'
raw_beta <- function(meta) {
  rho <- rowSums(meta@ordinariness, na.rm = TRUE) / meta@ordinariness
  results <- 1 / rho
  relativeentropy(results, meta, "raw beta")
}


#' Normalised beta (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating
#' normalised beta diversity.
#'
#' Values generated from \code{norm_beta()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate normalised subcommunity and metacommunity beta
#' diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{norm_beta} returns an object of class \code{relativeentropy}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate normalised beta component
#' b <- norm_beta(meta)
#' subdiv(b, 1)
#' metadiv(b, 1)
#'
norm_beta <- function(meta) {
  ordinariness.bar <- sapply(seq_along(meta@subcommunity_weights),
                             function(x) meta@ordinariness[, x] /
                               meta@subcommunity_weights[x])
  if (!is.matrix(ordinariness.bar))
    ordinariness.bar <- as.matrix(t(ordinariness.bar))
  colnames(ordinariness.bar) <- colnames(meta@type_abundance)
  normalised.rho <- rowSums(meta@ordinariness, na.rm = TRUE) / ordinariness.bar
  results <- 1 / normalised.rho
  relativeentropy(results, meta, "normalised beta")
}


#' Gamma (low level diversity component)
#'
#' Calculates the low-level diversity component necessary for calculating gamma
#' diversity.
#'
#' Values generated from \code{raw_gamma()} may be input into \code{subdiv()} and
#' \code{metadiv()} to calculate subcommunity and metacommunity gamma diversity.
#'
#' @inheritParams raw_alpha
#'
#' @return \code{raw_gamma} returns an object of class \code{powermean}
#' @include metacommunity.R subdiv.R metadiv.R
#' @export
#'
#' @references R. Reeve, T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' pop <- cbind.data.frame(A = c(1,1), B = c(2,0), C = c(3,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate gamma component
#' g <- raw_gamma(meta)
#' subdiv(g, 1)
#' metadiv(g, 1)
#'
raw_gamma <- function(meta) {
  results <- rowSums(meta@ordinariness, na.rm = TRUE)
  results[results == 0] <- NaN
  results <- 1 / results
  N <- nrow(meta@type_abundance)

  results <- apply(meta@type_abundance, 2, function(x) {
    tmp <- rep(0, N)
    tmp[which(x != 0)] <- results[which(x != 0)]
    tmp
  })
  row.names(results) <- row.names(meta@type_abundance)
  results <- as.matrix(results)

  powermean(results, meta, "gamma")
}
