#' Cut phylogeny
#' 
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param ps \code{phy_struct()} output.
#' 
#' @return Returns an object of class \code{metacommunity}.
#' @export
#' 
chainsaw <- function(partition, ps) {
  partition <- check_partition(partition)
  structure_matrix <- ps$structure
  hs <- phy_abundance(partition, structure_matrix)
  s <- smatrix(ps)
  z <- zmatrix(partition, s, ps)
  cut_meta <- metacommunity(hs, z)
  # Fill in 'phylogeny' metacommunity slots
  cut_meta@raw_abundance <- partition
  cut_meta@raw_structure <- structure_matrix
  cut_meta@parameters <- ps$parameters
  # Output
  cut_meta
}