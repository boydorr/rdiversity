#' taxfac
#' 
#' @param lookup \code{data.frame} with colnames corresponding to nested 
#' hierarchical levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' 
#' @examples 
#' \dontrun{
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#' 
#' taxfac(lookup)
#' }
#' 
taxfac <- function(lookup) {
  output <- lookup
  for(i in 1:ncol(output)) output[,i] <- as.numeric(as.factor(output[,i]))
  row.names(output) <- lookup[,1]
  output - 1
}
