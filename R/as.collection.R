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
as.collection <- function(data, similarity = NA, zmatrix = NA, lookup = NA) {
  # If both similarity and zmatrix arguments are provided, return an error
  if(!is.na(similarity) & all(!is.na(zmatrix))) 
    stop('Check arguments. Cannot set both similarity and zmatrix.')
  
  if(is.data.frame(data)) data <- as.matrix(data)
  if(class(data)=='phylo') new.tree <- as.rdphylo(data)
    
  # If data is class 'rdphylo': if zmatrix is provided check it is valid, 
  # otherwise calculate phylogenetic similarity and abundance of historic 
  # species
  if(class(data)=='rdphylo') {
    pmatrix <- new.tree@hs.abundance
    if(all(is.na(zmatrix))) {
      zmatrix <- calculate.zmatrix(new.tree)
    }else if(is.matrix(zmatrix)) {
      check.zmatrix(data, zmatrix)
    }
    
    # If data is class 'matrix': if zmatrix is provided check it is valid, 
    # if similarity is provided calculate similarity and abundance of types 
    # (naive, taxonomic, phenotypic, etc.), otherwise assume a naive-type
    # case
  }else if(is.matrix(data)) {
    pmatrix <- data
    pmatrix <- check.pmatrix(data)
    if(is.matrix(zmatrix)) {
      check.zmatrix(data, zmatrix)
    }else if(!is.na(similarity)) {
      zmatrix <- calculate.zmatrix(similarity, data, lookup)
    }else if(is.na(similarity) & all(is.na(zmatrix))) 
      zmatrix <- calculate.zmatrix(data)
    
  }else stop('object data is of unknown format.')
  
  # Coerse object into a collection
  new('collection', pmatrix, zmatrix = zmatrix)
}


#' @rdname as.collection
#' @param x any R object 
is.collection <-
  function (x) 
  {
    inherits(x, "collection")
  }


check.pmatrix <- function(data, zmatrix = NA) {
  if(sum(pmatrix) != 1) {
    pmatrix <- pmatrix / sum(pmatrix)
    print('Population matrix was normalised to sum to 1.')
    if(is.null(row.names(pmatrix))) 
      row.names(pmatrix) <- paste('type', 1:nrow(pmatrix))
    if(is.null(colnames(pmatrix))) 
      colnames(pmatrix) <- paste('subcommunity', 1:ncol(pmatrix))
  }
}


check.zmatrix <- function(data, zmatrix) {
  if(class(data) == 'rdphylo') {
    if (length(data@hs.name) != nrow(zmatrix))
      stop('Number of historic species in phylogeny must equal number of 
           rows in zmatrix.')
    
  }else if(is.matrix(data)) {
    if(any(zmatrix>1) | any(zmatrix<0)) 
      stop('zmatrix elements must take a value between 0 and 1.')
  }
}