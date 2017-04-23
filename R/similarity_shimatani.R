#' Taxonomic similarity matrix
#' 
#' Calculates taxonomic similarity based on Shimatani's index of taxonomic 
#' similarity (see \emph{Details}).
#' 
#' Shimatani's taxonomic similarity index is defined:
#' \tabular{ll}{
#' \code{species.similarity} \tab 1 \cr
#' \code{genus.similarity} \tab 0.75 \cr
#' \code{family.similarity} \tab 0.5 \cr
#' \code{subclass.similarity} \tab 0.25 \cr
#' \code{other.similarity} \tab 0 \cr
#' }
#' @references Shimatani, K. 2001. On the measurement of species diversity 
#' incorporating species differences. Oikos 93:135–147.
#' 
#' @param data \eqn{S * N} \code{matrix}; population counts
#' @param lookup \code{data.frame} with colnames = c('Species', 'Genus', 
#' 'Family', 'Subclass')
#' @return Returns an \eqn{S * S} \code{matrix}; pair-wise taxonomic similarity
#' @export
#' @examples 
#' pop <- sample(1:50, 4)
#' lookup <- data.frame(Subclass=c("Sapindales", "Malvales", "Fabales", 
#'                                   "Fabales"),      
#'                      Family=c("Burseraceae", "Bombacaceae", "Fabaceae", 
#'                               "Fabaceae"), 
#'                      Genus=c("Protium", "Quararibea", "Swartzia", 
#'                              "Swartzia"),       
#'                      Species= c("tenuifolium", "asterolepis",
#'                                     "simplex var.grandiflora",
#'                                     "simplex var.ochnacea"))
#' similarity <- similarity_shimatani(pop, lookup)
#' 
similarity_shimatani <- function(data, lookup) 
{
  # Data and lookup table must have the same number of entries
  stopifnot(nrow(data)==nrow(lookup))
  
  if(is.vector(data))
    data <- as.matrix(data)
  
  # Based on Shimatani's taxonomic similarity indices
  species.similarity <- 1
  genus.similarity <- 0.75
  family.similarity <- 0.5
  subclass.similarity <- 0.25 
  other.similarity <- 0
  
  S <- nrow(data)
  zmatrix <- diag(S)
  for (i in 1:nrow(lookup)) {
    for (j in 1:nrow(lookup)) {
      
      if (lookup$Species[i]==lookup$Species[j]) {
        zmatrix[i,j] <- species.similarity
      }else if(lookup$Genus[i]==lookup$Genus[j]) {
        zmatrix[i,j] <- genus.similarity
      }else if(lookup$Family[i]==lookup$Family[j]) {
        zmatrix[i,j] <- family.similarity
      }else if(lookup$Subclass[i]==lookup$Subclass[j]) {
        zmatrix[i,j] <- subclass.similarity
      }else zmatrix[i,j] <- other.similarity
    }
  }
  row.names(zmatrix) <- lookup$Species
  colnames(zmatrix) <- lookup$Species
  zmatrix
}
