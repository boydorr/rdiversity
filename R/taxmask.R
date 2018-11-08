#' taxmask
#' 
#' @param lookup lskj
#' 
taxmask <- function(lookup) {
  N <- apply(lookup, 2, function(x) length(unique(x)))
  bits <- round(log(N, 2))
  total <- sum(bits)
  
  output <- lapply(seq_along(bits), function(x) {
    n <- sum(bits[x:length(bits)])
    ones <- rep(TRUE, n)
    zeroes <- rep(FALSE, total-n)
    tmp <- c(zeroes, ones)
    tmp <- binaryLogic::as.binary(tmp, logic = T)
    # as.numeric(as.character(tmp))
  })
  names(output) <- colnames(lookup)
  output
}