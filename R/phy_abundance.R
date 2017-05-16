#' Calculate abundance of historical species
#'
#' Calculates the relative abundance of historical species.
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param structure_matrix \code{output$structure} of \code{phy_struct()}.
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree, partition)
#' structure_matrix <- ps$structure
#' phy_abundance(partition, structure_matrix)
#'
phy_abundance <- function(partition, structure_matrix) {
  # Identify which species are present
  if (any(row.names(partition) != colnames(structure_matrix)))
    stop("Partition does not match phylogeny.")
  partition <- check_partition(partition)
  
  structure_matrix %*% partition
}
