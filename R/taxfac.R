#' taxfac
#' 
#' @param lookup \code{data.frame} with colnames corresponding to nested 
#' hierarchical levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' 
taxfac <- function(lookup) {
  for(i in 1:ncol(lookup)) 
    lookup[,i] <- as.numeric(as.factor(lookup[,i]))
  lookup - 1
}