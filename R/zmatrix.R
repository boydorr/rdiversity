#' Similarity matrix
#'
#' Function to calculate a phylogenetic similarity matrix.
#'
#' @param partition  two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of terminal taxa
#' @param s \code{smatrix()} output; ultrametric-similarity matrix.
#' @param ps \code{phy_struct()} output.
#'
#' @noRd
#'
#' @return \code{zmatrix()} returns an \eqn{hS x hS} matrix; pair-wise
#' similarity of historic species.
#'
zmatrix <- function(partition, s, ps){
  partition <- check_phypartition(tip_labels = colnames(ps$structure),
                                  partition = partition)

  parameters <- ps$parameters
  structure_matrix <- ps$structure
  T_bar <- ps$tbar
  L_j <- colSums(structure_matrix * T_bar)
  L_j <- L_j[match(parameters$tip_label, colnames(structure_matrix))]

  # Identify which species are present
  if (any(row.names(partition) != colnames(structure_matrix)))
    stop("Partition does not match phylogeny.")

  scaling_factor <- T_bar / L_j

  scaling_matrix <- diag(scaling_factor, nrow(structure_matrix))
  z <- s %*% scaling_matrix
  colnames(z) <- row.names(z)

  new("similarity",
      similarity = z,
      dat_id = "phybranch",
      parameters = list(transform = NA,
                        k = NA,
                        normalise = NA,
                        max_d = NA))
}
