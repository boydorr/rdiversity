#' phy_abundance
#'
#' Calculates the relative abundance of historic species
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param structure_matrix \code{output$structure} of \code{phy_struct()}; each 
#' row denotes historic species, columns denote terminal taxa, and elements 
#' contain branch lengths
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree)
#' structure_matrix <- ps$structure
#' phy_abundance(partition, structure_matrix)
#'
phy_abundance <- function(partition, structure_matrix) {
  # Identify which species are present
  keep <- which(row.names(partition) %in% colnames(structure))
  partition <- partition[keep,, drop=FALSE]
  

  T_bar <- tbar(partition, structure_matrix)
  structure_matrix %*% partition / T_bar
}
