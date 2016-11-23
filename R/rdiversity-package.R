#' rdiversity
#'
#' The \code{rdiversity} package calculates the similarity-sensitive diversity
#' of a population and it's partitions.
#'
#' This package calculates, or takes as input, the similarity between
#' types (naive, taxonomic, phenotypic, genetic,
#' phylogenetic, functional, etc.), where a type is defined as the categorical
#' variable that describes how the similarity of individuals is assessed.
#'
#' @name rdiversity-package
#' @aliases rdiversity
#' @docType package
#'
#' @import ggplot2
#' @import methods
#' @import ape
#' @import phangorn
#' @importFrom tidyr nest
#' @importFrom tibble as_data_frame
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom utils head
#'
#' @author
#' Sonia Mitchell <soniamitchell@gmail.com>, Richard Reeve <richard.reeve@glasgow.ac.uk>
#' \cr
#' Maintainer: Sonia Mitchell <soniamitchell@gmail.com>
#'
#'
NULL


# In a later release, change .Deprecated to .Defunct.

raw.supercommunity.alpha <- function(super, qs) {
  .Deprecated("raw.metacommunity.alpha")
  raw.metacommunity.alpha(super, qs)
}

normalised.supercommunity.alpha <- function(super, qs) {
  .Deprecated("normalised.metacommunity.alpha")
  normalised.metacommunity.alpha(super, qs)
}

raw.supercommunity.beta <- function(super, qs) {
  .Deprecated("raw.metacommunity.beta")
  raw.metacommunity.beta(super, qs)
}

normalised.supercommunity.beta <- function(super, qs) {
  .Deprecated("normalised.metacommunity.beta")
  normalised.metacommunity.beta(super, qs)
}

raw.supercommunity.rho <- function(super, qs) {
  .Deprecated("raw.metacommunity.rho")
  raw.metacommunity.rho(super, qs)
}

normalised.supercommunity.rho <- function(super, qs) {
  .Deprecated("normalised.metacommunity.rho")
  normalised.metacommunity.rho(super, qs)
}

supercommunity.gamma <- function(super, qs) {
  .Deprecated("metacommunity.gamma")
  metacommunity.gamma(super, qs)
}

is.supercommunity <- function(x) {
  .Deprecated("is.metacommunity")
  is.metacommunity(x)
}

as.supercommunity <- function(partition, similarity) {
  .Deprecated("as.metacommunity")
  as.metacommunity(partition, similarity)
}

supercommunity <- function(partition, similarity) {
  .Deprecated("metacommunity")
  metacommunity(partition, similarity)
}

superdiv <- function(data, qs) {
  .Deprecated("metadiv")
  metadiv(data, qs)
}
