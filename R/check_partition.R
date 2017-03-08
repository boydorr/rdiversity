#' Check partition
#' 
#' \code{check_partition()} is used to validate partition matrices.
#' 
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as subcommunities, and elements containing relative 
#' abundances of types in subcommunities. In the case of phylogenetic 
#' metacommunities, these are the relative abundances of terminal taxa. 
#' 
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If 
#' the partition matrix was valid, this should be identical to that which was
#' input as an argument.
#' @export
#' 
#' @examples 
#' population <- data.frame(a = 1:2, b = 3:4)
#' population <- population / sum(population)
#' row.names(population) <- NULL
#' new.population <- check_partition(population)
#' new.population
#' 
check_partition <- function(partition) {
  if(is.vector(partition)) partition <- as.matrix(partition)
  if(is.data.frame(partition)) partition <- as.matrix(partition)
  
  # normalise partition if it does not sum to 1
  if(!isTRUE(all.equal(sum(partition),1))) {
    partition <- partition / sum(partition)
    warning ('Population matrix was normalised to sum to 1.')
  }
  
  partition
}
