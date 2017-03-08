#' z_matrix
#' 
#' Function to 
#' 
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as subcommunities, and elements containing relative 
#' abundances of types in subcommunities. In the case of phylogenetic 
#' metacommunities, these are the relative abundances of terminal taxa. 
#' @param smatrix object
#' @param ps object 
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples
#' tree <- ape::read.tree(text="(A:1,B:2)R:1;")
#' partition <- setNames(c(0.6, 0.4), tree$tip.label)
#' ps <- phy_struct(partition, tree)
#' smatrix <- s_matrix(tree, ps)
#' z_matrix(partition, smatrix, ps)
#' 
z_matrix <- 
  function(partition, smatrix, ps) 
  {
    partition <- check_partition(partition)
    T_bar <- sum(ps@structure %*% partition)
    L_j <- colSums(ps@structure)
    L_j <- L_j[match(ps@parameters$tip_label, colnames(ps@structure))]
    scaling_factor <- T_bar / L_j
    scaling_matrix <- diag(scaling_factor, nrow(ps@structure))
    zmatrix <- smatrix %*% scaling_matrix
    colnames(zmatrix) <- row.names(zmatrix)
    zmatrix
  }

