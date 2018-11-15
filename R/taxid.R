#' taxid
#' 
#' Generate taxanomic codes for each species by converting species, genus, 
#' family, and subclass into factors
#' 
#' @param taxFac sdf
#' 
taxid <- function(taxFac) {
  # taxFac <- taxFac + 1
  species <- row.names(taxFac)
  bits <- apply(taxFac, 2, function(x) ceiling(log(max(x)+1, 2)))

  output <- lapply(seq_along(species), function(x) {
    tmp <- taxFac[x,]
    tmp <- lapply(seq_along(tmp), function(y) 
      binaryLogic::as.binary(tmp[y], n = bits[y])) # intToBits(x)
    tmp <- unlist(tmp)  
    tmp <- binaryLogic::as.binary(tmp, logic = TRUE)
    sum(2^(which(rev(unlist(strsplit(as.character(tmp), "")) == 1))-1))
  })
  names(output) <- row.names(taxFac)
  unlist(output)
}