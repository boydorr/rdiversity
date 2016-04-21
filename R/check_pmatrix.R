check_partition <- function(partition, zmatrix = NA) {
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