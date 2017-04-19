#' Check similarity matrix
#'
#' \code{check_similarity()} is used to validate similarity matrices.
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' @param similarity two-dimensinal \code{matrix} of mode \code{numeric};
#' contains pair-wise similarity between types.
#'
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If
#' the similarity matrix was valid, this should be identical to that which was
#' input as an argument.
#' @export
#'
check_similarity <- function(partition, similarity) {
  if(is.data.frame(similarity)) similarity <- as.matrix(similarity)

  if(any(similarity[!is.na(similarity)]<0))
    stop('similarity matrix elements must take positive values.')

  # if(any(similarity[!is.na(similarity)]>1))
    # warning('non-phylogenetic similarity matrix elements should take a value between 0 and 1.')

  if(ncol(similarity)!=nrow(similarity))
    stop('similarity matrix must be square.')

  if(nrow(similarity)!=nrow(partition))
    stop('similarity and partition matrices must have equal types.')

  if(is.null(row.names(similarity))){
    row.names(similarity) <- row.names(partition)
    colnames(similarity) <- row.names(partition)
  }

  similarity
}

