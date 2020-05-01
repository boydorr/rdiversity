#' rdiversity: diversity measurement in R
#'
#' \code{rdiversity} is an R package based around a framework for measuring and
#' partitioning biodiversity using similarity-sensitive diversity measures. It
#' provides functionality for measuring alpha, beta and gamma diversity of
#' metacommunities (\emph{e.g.} ecosystems) and their constituent subcommunities,
#' where similarity may be defined as taxonomic, phenotypic, genetic, phylogenetic,
#' functional, and so on. It uses the diversity measures described in the arXiv
#' paper, `\emph{How to partition diversity}`.
#'
#' \itemize{
#' \item For more information go to our GitHub page;
#' \url{https://github.com/boydorr/rdiversity}
#' \item Please raise an issue if you find any problems;
#' \url{https://github.com/boydorr/rdiversity/issues}
#' \item This package is cross-validated against our Julia package; \url{https://github.com/richardreeve/Diversity.jl}
#' }
#'
#' @author
#' Sonia Mitchell <sonia.mitchell@glasgow.ac.uk> (maintainer) \cr
#' Richard Reeve <richard.reeve@glasgow.ac.uk>
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. (\url{https://arxiv.org/abs/1404.6520})
#' @name rdiversity-package
#' @aliases rdiversity
#' @docType package
#'
#' @import methods
#' @importFrom binaryLogic as.binary
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom utils head installed.packages
#'
NULL
