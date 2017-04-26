#' Similarity matrix
#'
#' Function to calculate a phylogenetic similarity matrix.
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param s \code{smatrix()} output; ultrametric-similarity matrix.
#' @param ps \code{phy_struct()} output.
#'
#' @return Returns an \eqn{hS x hS} matrix; pair-wise similarity of historic 
#' species.
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree, partition)
#' s <- smatrix(ps)
#'
#' zmatrix(partition, s, ps)
#'
zmatrix <-
  function(partition, s, ps)
  {
    partition <- check_partition(partition)
    
    parameters <- ps$parameters
    structure_matrix <- ps$structure
    T_bar <- ps$tbar
    L_j <- colSums(structure_matrix*T_bar)
    L_j <- L_j[match(parameters$tip_label, colnames(structure_matrix))]

    # Identify which species are present
    if (any(row.names(partition) != colnames(structure_matrix)))
      stop("Partition does not match phylogeny.")
    
    scaling_factor <- T_bar / L_j

    scaling_matrix <- diag(scaling_factor, nrow(structure_matrix))
    z <- s %*% scaling_matrix
    colnames(z) <- row.names(z)

    z
  }

