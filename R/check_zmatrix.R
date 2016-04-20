check_zmatrix <- function(data, zmatrix) {
  if(any(zmatrix>1) | any(zmatrix<0)) 
    stop('zmatrix elements must take a value between 0 and 1.')
  return(zmatrix)
}
