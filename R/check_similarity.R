#' Check similarity matrix
#'
#' \code{check_similarity()} is used to validate similarity matrices.
#'
#' @param similarity two-dimensional \code{matrix} of mode \code{numeric};
#' contains pair-wise similarity between types.
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#'
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If
#' the similarity matrix was valid, this should be identical to that which was
#' input as an argument.
#'
check_similarity <- function(similarity, partition) {
  if (is.data.frame(similarity)) similarity <- as.matrix(similarity)

  if (any(similarity[!is.na(similarity)] < 0))
    stop("similarity matrix elements must take positive values.")

  if (ncol(similarity) != nrow(similarity))
    stop("similarity matrix must be square.")

  if (!missing(partition)) {
    if (nrow(similarity) != nrow(partition))
      stop("similarity and partition matrices must have equal types.")

    if (is.null(row.names(similarity))){
      row.names(similarity) <- row.names(partition)
      colnames(similarity) <- row.names(partition)
    }
  }

  similarity
}
