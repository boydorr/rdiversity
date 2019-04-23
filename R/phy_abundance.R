#' Relative abundance of historical species
#'
#' Calculates the relative abundance of historical species.
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#' @param structure_matrix \code{output$structure} of \code{phy_struct()}.
#'
#' @export
#'
phy_abundance <- function(partition, structure_matrix) {
  partition <- check_phypartition(colnames(structure_matrix), partition)
  structure_matrix %*% partition
}
