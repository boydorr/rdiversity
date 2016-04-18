check.pmatrix <- function(data, zmatrix = NA) {
  pmatrix <- data
  if(sum(pmatrix) != 1) {
    pmatrix <- pmatrix / sum(pmatrix)
    print('Population matrix was normalised to sum to 1.')
    if(is.null(row.names(pmatrix))) 
      row.names(pmatrix) <- paste('type', 1:nrow(pmatrix))
    if(is.null(colnames(pmatrix))) 
      colnames(pmatrix) <- paste('subcommunity', 1:ncol(pmatrix))
  }
  return(pmatrix)
}