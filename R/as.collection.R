#' Coerse to a Collection
#' 
#' Functions to check if an object is a \code{collection} or coerce an object 
#' into a collection; an S4 object containing two slots, pmatrix and zmatrix.
#' 
#' @param data Object of class \code{matrix}
#' @param similarity Object of class \code{character}
#' @param zmatrix Object of class \code{matrix}
#' @param lookup Object of class \code{data.frame}
#' @return \code{as.collection()} returns an object of class \code{collection}; an S4 object containing two slots, pmatrix and zmatrix. \cr\cr
#' \code{is.collection()} returns TRUE if its argument is a collection, FALSE otherwise.
#' 
#' @include calculate.zmatrix.R class-collection.R
#' @export
#' 
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Create object of class collection
#' data <- as.collection(population)
#' 
#' print(data)
#' print(data@zmatrix)
#' 
as.collection <- function(data, phylo_abundance = NA, similarity = NA, zmatrix = NA, lookup = NA) {
  
  # Check pmatrix
  if(is.data.frame(data)) data <- as.matrix(data)
  if(sum(data)!=1) data <- data / sum(data)
  if(is.null(row.names(data))) row.names(data) <- paste('type', 1:nrow(data))
  if(is.null(colnames(data))) colnames(data) <- paste('subcommunity', 1:ncol(data))
  
  # If both similarity and zmatrix arguments are provided, return an error
  if(!is.na(similarity) & all(!is.na(zmatrix))) 
    stop('Check arguments. Cannot set both similarity and zmatrix.')
  
  # If neither similarity nor zmatrix arguments are provided assume a 
  # naive-type case; or if data is a phylogeny, calculate phylogenetic 
  # similarity
  zmatrix <- calculate.zmatrix(data)
  
  # If similarity is provided, then calculate the zmatrix
  if(!is.na(similarity)) {
    zmatrix <- calculate.zmatrix(similarity, data, lookup)
  }

  # If the zmatrix is provided, use it
  
  # Coerse object into a collection
  new('collection', data, zmatrix = zmatrix)
}
  

#' @rdname as.collection
#' @param x any R object 
is.collection <-
  function (x) 
  {
    inherits(x, "collection")
  }

