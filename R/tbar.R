#' Calculate T_bar
#'
#' Function to calculate T_bar.
#'
#' @param structure_matrix \code{output$structure} of \code{phy_struct()}; each 
#' row denotes historic species, columns denote terminal taxa, and elements 
#' contain branch lengths.
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @export
#'
tbar <- function(partition, structure_matrix) {
  sum(structure_matrix %*% partition)
}
