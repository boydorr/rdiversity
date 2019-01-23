#' Calculate T_bar
#'
#' Function to calculate T_bar.
#'
#' @param structure_matrix \code{output$structure} of \code{phy_struct()}; each
#' row denotes historic species, columns denote terminal taxa, and elements
#' contain branch lengths.
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#'
tbar <- function(partition, structure_matrix) {
  sum(structure_matrix %*% partition)
}
