#' phy_abundance
#'
#' Calculates the relative abundance of historic species
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param ps \code{phy_struct()} output.
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree)
#'
#' phy_abundance(partition, ps)
#'
phy_abundance <- function(partition, ps) {
  structure <- ps$structure
  
  # Identify which species are present
  keep <- which(row.names(partition) %in% colnames(structure))
  partition <- partition[keep,]

  T_bar <- sum(structure %*% partition)
  structure %*% partition / T_bar

}
