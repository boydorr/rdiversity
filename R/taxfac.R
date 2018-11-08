#' taxfac
#' 
#' @param lookup
#' 
taxfac <- function(lookup) {
  for(i in 1:ncol(lookup)) 
    lookup[,i] <- as.numeric(as.factor(lookup[,i]))
  lookup - 1
}