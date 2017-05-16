#' Calculate phylogenetic structure matrix
#'
#' Converts an object into class \code{phylo} into class \code{phy_struct}.
#'
#' @param tree object of class \code{phylo}
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#'
#' @return Returns a \code{list} containing:
#' \tabular{ll}{
#' \code{$structure} \tab - each row denotes historical species, columns denote
#' terminal taxa, and elements contain 'branch lengths / tbar' \cr
#' \code{$parameters} \tab - information associated with each historical species \cr
#' \code{$tree} \tab - object of class \code{phylo} \cr
#' }
#'
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#'
#' res <- phy_struct(tree, partition)
#'
phy_struct <- function(tree, partition) {
  partition <- check_partition(partition)

  # Extract parameters associated with each historic species
  parameters <- hs_parameters(tree)

  # Create structural matrix
  s_matrix <- matrix(0, ncol = length(seq_along(tree$tip.label)),
                     nrow = nrow(parameters))
  row.names(s_matrix) <- parameters$hs_names
  colnames(s_matrix) <- tree$tip.label
  index <- sapply(parameters$tip_label, function(x) which(colnames(s_matrix) %in% x))
  index <- cbind(row = 1:nrow(parameters), col = index)
  s_matrix[index] <- parameters$lengths
  T_bar <-  sum(s_matrix %*% partition)

  s_matrix <- s_matrix / T_bar

  # Output
  list(structure = s_matrix,
       tbar = T_bar,
       parameters = parameters,
       tree = tree)
}


