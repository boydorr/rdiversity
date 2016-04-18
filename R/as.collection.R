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
  
  # 
  if(is.data.frame(data)) data <- as.matrix(data)
  
  # If data is a phylogeny: if zmatrix is provided check it is valid, 
  # otherwise calculate phylogenetic similarity and abundance of historic 
  # species
  if(class(data)=='phylo') {
    if(all(is.na(zmatrix))) {
      new.tree <- as.rdphylo(tree)
      pmatrix <- new.tree@hs.abundance
      zmatrix <- calculate.zmatrix(new.tree)
      
    }else if(is.matrix(zmatrix)) {
      check.phylo.pmatrix(data, zmatrix)
    }
    # If data is class rdphylo
  }else if(is.rdphylo(data)) {
    pmatrix <- data@hs.abundance
    zmatrix <- calculate.zmatrix(data)
    
    # If data is a pmatrix, check it
  }else if(is.matrix(data)) {
    pmatrix <- data
    if(sum(pmatrix) != 1) 
      pmatrix <- pmatrix / sum(pmatrix)
    if(is.null(row.names(pmatrix))) 
      row.names(pmatrix) <- paste('type', 1:nrow(pmatrix))
    if(is.null(colnames(pmatrix))) 
      colnames(pmatrix) <- paste('subcommunity', 1:ncol(pmatrix))
    
    # If the zmatrix is provided, use it
    if(is.matrix(zmatrix)) {
      zmatrix <- zmatrix
      
      # If similarity is provided, then calculate the zmatrix
    }else if(!is.na(similarity)) {
      zmatrix <- calculate.zmatrix(similarity, data, lookup)
      
      # If neither similarity nor zmatrix arguments are provided assume a 
      # naive-type case
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