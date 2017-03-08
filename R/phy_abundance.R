#' phy_abundance
#' 
#' Calculates the relative abundance of historic species
#' 
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as subcommunities, and elements containing relative 
#' abundances of types in subcommunities. In the case of phylogenetic 
#' metacommunities, these are the relative abundances of terminal taxa. 
#' @param ps object of class \code{phy_struct}
#' @export
#' 
phy_abundance <- function(partition, ps) {
  
  T_bar <- sum(ps@structure %*% partition)
  res <- ps@structure %*% partition / T_bar
  
}