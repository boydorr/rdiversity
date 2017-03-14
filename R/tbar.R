#' tbars
#' 
#' Function to calculate T_bar.
#' 
#' @param ps \code{phy_struct()} output.
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @export
#' 
tbar <- function(ps, partition) {
  # Identify which species are here
  structure <- ps$structure
  keep <- which(row.names(partition) %in% colnames(structure))
  partition <- partition[keep,, drop=FALSE]
  
  sum(structure %*% partition)
}