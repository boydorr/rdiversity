#' Check similarity
#' 
#' as
#' 
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains relative abundance of types
#' @param similarity two-dimensinal \code{matrix} of mode \code{numeric}; 
#' contains pair-wise similarity between types
#' 
#' @details 
#' 
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If 
#' the similarity matrix was valid, this should be identical to that which was
#' input as an argument.
#' @export
#' 
#' @examples 
#' 
check_similarity <- function(partition, similarity) {
  if(any(similarity>1) | any(similarity<0)) 
    stop('similarity matrix elements must take a value between 0 and 1.')
  if(is.data.frame(similarity)) similarity <- as.matrix(similarity)
  
  return(similarity)
}
