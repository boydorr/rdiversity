check.zmatrix <- function(data, zmatrix) {
  if(class(data) == 'RDphylo') {
    if (length(data@hs.name) != nrow(zmatrix))
      stop('Number of historic species in phylogeny must equal number of 
           rows in zmatrix.')
    
  }else if(is.matrix(data)) {
    if(any(zmatrix>1) | any(zmatrix<0)) 
      stop('zmatrix elements must take a value between 0 and 1.')
  }
}