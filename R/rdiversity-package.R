#' rdiversity
#'
#' \code{rdiversity} is an R package based around a framework for measuring and
#' partitioning biodiversity using similarity-sensitive diversity measures. It
#' provides functionality for measuring alpha, beta and gamma diversity of
#' metacommunities (\emph{e.g.} ecosystems) and their constituent subcommunities, 
#' where similarity may be defined as taxonomic, phenotypic, genetic, phylogenetic,
#' functional, and so on. It uses the diversity measures described in the arXiv
#' paper, \emph{How to partition diversity}.
#'
#' \itemize{
#' \item For more information go to our GitHub page;
#' \url{https://github.com/boydorr/rdiversity}
#' \item Please raise an issue if you find any problems;
#' \url{https://github.com/boydorr/rdiversity/issues}
#' \item This package is cross-validated against our Julia package; \url{https://github.com/richardreeve/Diversity.jl}
#' }
#'
#' @author Sonia Mitchell <s.mitchell.2@research.gla.ac.uk>, 
#' Richard Reeve <richard.reeve@glasgow.ac.uk>
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. (\url{https://arxiv.org/abs/1404.6520})
#' @name rdiversity-package
#' @aliases rdiversity
#' @docType package
#'
#' @import ggplot2
#' @import ggthemes
#' @import methods
#' @import ape
#' @import phangorn
#' @import plyr
#' @importFrom phytools nodeHeights
#' @importFrom tidyr nest
#' @importFrom tibble as_data_frame
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom utils head
#'
#'
NULL
