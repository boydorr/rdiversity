#' Calculate Similarity Matrix (Z)
#' 
#' The function \code{calculate.zmatrix} calculates the similarity matrix.
#' 
#' The argument \code{type} takes the following inputs: \cr
#' \tabular{ll}{
#' \code{unique} \tab - assumes all \emph{types} are unique \cr
#' \code{taxonomic} \tab - assumes all \emph{types} are species; calculates a 
#' similarity matrix based on Shimatani's taxonomic similarity \cr
#' }
#' 
#' @param type character string; defining which similarity measure is being 
#' called for. 
#' @param data matrix of count data
#' @param lookup required for calculating \code{taxonomic} similarity; contains 
#' four columns, listing 'Species', 'Genus', 'Family', and 'Subclass'
#' @return matrix of pair-wise similarity indices
#' 
calculate.zmatrix <- function(type, data, lookup = NA) 
{
  if(type=='unique') {
    N <- nrow(data)
    zmatrix <- diag(N)
    row.names <- row.names(data)
    colnames <- row.names(data)
    return(zmatrix)
    
  }else if(type=='taxonomic') {
    if(nrow(data)!=nrow(lookup)) stop('data and lookup do not match up')
    N <- nrow(data)
    
    # Based on Shimatani's taxonomic similarity indices
    species.similarity <- 1
    genus.similarity <- 0.75
    family.similarity <- 0.5
    subclass.similarity <- 0.25 
    other.similarity <- 0
    
    zmatrix <- diag(N)
    for (i in 1:nrow(lookup)) {
      for (j in 1:nrow(lookup)) {
        
        if (lookup$Species[i]==lookup$Species[j]) {
          zmatrix[i,j] <- species.similarity
        } else if (lookup$Genus[i]==lookup$Genus[j]) {
          zmatrix[i,j] <- genus.similarity
        } else if (lookup$Family[i]==lookup$Family[j]) {
          zmatrix[i,j] <- family.similarity
        } else if (lookup$Subclass[i]==lookup$Subclass[j]) {
          zmatrix[i,j] <- subclass.similarity
        } else zmatrix[i,j] <- other.similarity
      }
    }
    row.names(zmatrix) <- row.names(data)
    colnames(zmatrix) <- row.names(data)
    attr(zmatrix, 'type') <- type
    return(zmatrix)
  }
}