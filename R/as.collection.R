#' Coerse to a Supercommunity
#' 
#' Functions to check if an object is a \code{supercommunity} or coerce an object 
#' into a \code{supercommunity}.
#' 
#' @param data Object of class \code{matrix}
#' @param similarity Object of class \code{character}
#' @param zmatrix Object of class \code{matrix}
#' @param lookup Object of class \code{data.frame}
#' @return 
#' \code{as.supercommunity()} returns an object of class \code{supercommunity}; 
#' an S4 object containing two slots, pmatrix and zmatrix. \cr\cr
#' \code{is.supercommunity()} returns TRUE if its argument is a supercommunity, 
#' FALSE otherwise.
#' 
#' @include calculate.zmatrix.R class-supercommunity.R check.pmatrix.R check.zmatrix.R
#' @export
#' 
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Create object of class supercommunity
#' data <- as.supercommunity(population)
#' 
#' print(data)
#' print(data@zmatrix)
#' 
as.supercommunity <- function(data, similarity = NA, zmatrix = NA, lookup = NA) {
  # If both similarity and zmatrix arguments are provided, return an error
  if(!is.na(similarity) & all(!is.na(zmatrix))) 
    stop('Check arguments. Cannot set both similarity and zmatrix.')
  
  if(is.data.frame(data)) data <- as.matrix(data)
  if(class(data)=='phylo') new.tree <- as.RDphylo(data)
    
  # If data is class 'RDphylo': if zmatrix is provided check it is valid, 
  # otherwise calculate phylogenetic similarity and abundance of historic 
  # species
  if(class(data)=='RDphylo') {
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
  
  # Coerse object into a supercommunity
  new('supercommunity', pmatrix, zmatrix = zmatrix)
}


#' @rdname as.supercommunity
#' @param x any R object 
is.supercommunity <-
  function (x) 
  {
    inherits(x, "supercommunity")
  }

