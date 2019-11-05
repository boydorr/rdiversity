#' Calculate phylogenetic structure matrix
#'
#' Converts an object into class \code{phylo} into class \code{phy_struct}.
#'
#' @param tree object of class \code{phylo}
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#'
#' @return \code{phy_struct()} returns a \code{list} containing:
#' \tabular{ll}{
#' \code{$structure} \tab - each row denotes historical species, columns
#' denote terminal taxa, and elements contain 'branch lengths' \cr
#' \code{$tbar} - the average distance from root to tip for all terminal
#' taxa \cr
#' \code{$parameters} \tab - information associated with each historical
#' species \cr
#' \code{$tree} \tab - object of class \code{phylo} \cr
#' }
#' @export
#'
phy_struct <- function(tree, partition) {
  partition <- check_phypartition(tree$tip.label, partition)

  # Extract parameters associated with each historic species
  parameters <- hs_parameters(tree)

  # Generate structural matrix for entire phylogeny
  s_matrix <- matrix(0, ncol = length(seq_along(tree$tip.label)),
                     nrow = nrow(parameters))
  row.names(s_matrix) <- parameters$hs_names
  colnames(s_matrix) <- tree$tip.label
  index <- vapply(parameters$tip_label, function(x)
    which(colnames(s_matrix) %in% x), numeric(1))
  index <- cbind(row = seq_len(nrow(parameters)), col = index)
  s_matrix[index] <- parameters$lengths
  T_bar <-  sum(s_matrix %*% partition)

  s_matrix <- s_matrix / T_bar

  # Output
  list(structure = s_matrix,
       tbar = T_bar,
       parameters = parameters,
       tree = tree)
}
