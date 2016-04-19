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
#' @param data \code{matrix} of count data
#' @param lookup required for calculating \code{taxonomic} similarity; contains 
#' four columns, listing 'Species', 'Genus', 'Family', and 'Subclass'
#' @return Returns an \eqn{S * S} matrix; pair-wise similarity of \emph{types}
#' 
#' @include class-supercommunity.R
#' 
calculate.zmatrix <- function(data, similarity = NA, lookup = NA) 
{
  # If object data is class phylo or class rdphylo, calculate phylogenetic 
  # similarity
  if(class(data)=='phylo' | is.rdphylo(data)) {
    phylo.zmatrix(data)
    
  }else if(is.matrix(data)) {
    if(similarity=='unique' | is.na(similarity)) {
      unique.zmatrix(data)
    }else if(similarity=='taxonomic') {
      tax.zmatrix(data, lookup)
    }else {
      stop('Similarity not recognised')
      }
  }else stop('unknown object data format.')
}


#' Naive-type similarity matrix
#' 
#' This function returns a naive-type similarity matrix, whereby \emph{types} 
#' are considered to be completely similar to themselves, but completely 
#' distinct from each other.
#' 
#' @param data \eqn{S x N} \code{matrix}; population counts
#' @return Returns an \eqn{S * S} \code{matrix}; identity matrix where each \emph{type} 
#' is completely distinct
#' 
unique.zmatrix <- function(data) {
  S <- nrow(data)
  zmatrix <- diag(S)
  row.names <- row.names(data)
  colnames <- row.names(data)
  return(zmatrix)
}


#' Taxonomic similarity matrix
#' 
#' This function calculates taxonomic similarity based on Shimatani's 
#' index of taxonomic similarity (See Details).
#' 
#' Shimatani's taxonomic similarity index is defined:
#' 
#' \tabular{ll}{
#' \code{species.similarity} \tab 1 \cr
#' \code{genus.similarity} \tab 0.75 \cr
#' \code{family.similarity} \tab 0.5 \cr
#' \code{subclass.similarity} \tab 0.25 \cr
#' \code{other.similarity} \tab 0 \cr
#' }
#' 
#' @param data \eqn{S * N} \code{matrix}; population counts
#' @param lookup \code{data.frame} with colnames = c('Species', 'Genus', 'Family', 'Subclass')
#' @return Returns an \eqn{S * S} \code{matrix}; pair-wise taxonomic similarity
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
  # attr(zmatrix, 'similarity') <- similarity
  return(zmatrix)
}


