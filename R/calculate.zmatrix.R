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
calculate.zmatrix <- function(type, data) 
{
  if(type='unique') {
    N <- nrow(data)
    zmatrix <- diag(N)
    row.names <- row.names(data)
    colnames <- row.names(data)
    return(zmatrix)
    
  }
}