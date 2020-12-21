#' Check partition matrix
#'
#' \code{check_partition()} is used to validate partition matrices.
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#'
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If
#' the partition matrix was valid, this should be identical to that which was
#' input as an argument.
#'
#' @noRd
#'
check_partition <- function(partition) {
  if (is.vector(partition)) partition <- as.matrix(partition)
  if (is.data.frame(partition)) partition <- as.matrix(partition)

  # normalise partition if it does not sum to 1
  if (!isTRUE(all.equal(sum(partition), 1))) {
    partition <- partition / sum(partition)
    message("Metacommunity matrix was normalised to sum to 1.")
  }

  partition
}
