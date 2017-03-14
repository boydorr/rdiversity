#' z_matrix
#'
#' Function to
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param s object
#' @param ps object
#'
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree)
#' s <- smatrix(ps)
#'
#' zmatrix(partition, s, ps)
#'
zmatrix <-
  function(partition, s, ps)
  {
    parameters <- ps$parameters
    structure <- ps$structure
    L_j <- colSums(structure)
    L_j <- L_j[match(parameters$tip_label, colnames(structure))]

    keep <- which(row.names(partition) %in% colnames(structure))
    partition <- partition[keep,]

    T_bar <- sum(structure %*% partition)
    scaling_factor <- T_bar / L_j
    partition <- check_partition(partition)

    scaling_matrix <- diag(scaling_factor, nrow(structure))
    z <- s %*% scaling_matrix
    colnames(z) <- row.names(z)

    z
  }

