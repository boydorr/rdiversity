#' taxid
#' 
#' Generate taxanomic codes for each species by converting species, genus, 
#' family, and subclass into factors
#' 
#' @param lookup \code{data.frame} with colnames corresponding to nested 
#' hierarchical levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' 
taxid <- function(lookup) {
  N <- apply(lookup, 2, function(x) length(unique(x)))
  bits <- round(log(N, 2))
  df <- taxfac(lookup)
  
  output <- lapply(seq_along(bits), function(x) {
    tmp <- df[x,]
    tmp <- lapply(seq_along(tmp), function(y) 
      binaryLogic::as.binary(tmp[y], n = bits[y])) # intToBits(x)
    tmp <- unlist(tmp)  
    tmp <- binaryLogic::as.binary(tmp, logic = TRUE)
    sum(2^(which(rev(unlist(strsplit(as.character(tmp), "")) == 1))-1))
  })
  names(output) <- lookup[,1]
  unlist(output)
}