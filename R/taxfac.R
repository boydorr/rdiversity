#' taxfac
#' 
#' @param lookup \code{data.frame} with colnames corresponding to nested 
#' hierarchical levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' 
taxfac <- function(lookup) {
  output <- lookup
  for(i in 1:ncol(output)) output[,i] <- as.numeric(as.factor(output[,i]))
  row.names(output) <- lookup[,1]
  output - 1
}
