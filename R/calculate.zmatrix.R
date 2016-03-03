#' Calculate Similarity Matrix (Z)
#' 
#' The function \code{calculate.zmatrix} calculates the similarity matrix.
#' 
#' The argument \code{similarity} takes the following inputs: \cr
#' \tabular{ll}{
#' \code{unique} \tab - assumes all \emph{types} are unique \cr
#' \code{taxonomic} \tab - assumes all \emph{types} are species; calculates a 
#' similarity matrix based on Shimatani's taxonomic similarity \cr
#' }
#' 
#' @param similarity character string; defining which similarity measure is being 
#' called for. 
#' @param data matrix of count data
#' @param lookup required for calculating \code{taxonomic} similarity; contains 
#' four columns, listing 'Species', 'Genus', 'Family', and 'Subclass'
#' @return matrix of pair-wise similarity indices
#' 
calculate.zmatrix <- function(similarity, data, lookup = NA) 
{
  if(similarity=='unique') {
    unique.zmatrix(data)
  }else if(similarity=='taxonomic') {
    tax.zmatrix(data, lookup)
  }else if(similarity=='phylogenetic') {
    phylo.zmatrix(data)
  }else stop('Similarity not recognised')
}


#' 
#' 
#' 
#' 
#' 
unique.zmatrix <- function() {
  S <- nrow(data)
  zmatrix <- diag(S)
  row.names <- row.names(data)
  colnames <- row.names(data)
  return(zmatrix)
}


#' 
#' 
#' 
#' 
#' 
tax.zmatrix <- function(data, lookup) 
{
  # Test
  if(nrow(data)!=nrow(lookup)) stop('data and lookup do not match up')
  
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
  row.names(zmatrix) <- row.names(data)
  colnames(zmatrix) <- row.names(data)
  attr(zmatrix, 'similarity') <- similarity
  return(zmatrix)
}


