#' Check partition
#' 
#' sdf
#' 
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains relative abundance of types
#' 
#' @details 
#' 
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If 
#' the partition matrix was valid, this should be identical to that which was
#' input as an argument.
#' @export
#' 
#' @examples 
#' 
check_partition <- function(partition) {
  if(sum(partition) != 1) {
    partition <- partition / sum(partition)
    warning('Population matrix was normalised to sum to 1.')
    if(is.null(row.names(partition))) 
      row.names(partition) <- paste('type', 1:nrow(partition))
    if(is.null(colnames(partition))) 
      colnames(partition) <- paste('subcommunity', 1:ncol(partition))
  }
  return(partition)
}