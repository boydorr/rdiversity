#' Calculate similarity
#' 
#' Functions to check if an object is a \code{similarity}, or coerse an  
#' object into a \code{similarity}; output of \code{phy2sim}.
#' 
#' @param phylo object of class \code{phylo}
#' @param depth object of class \code{numeric}
#' @export
#' 
#' @return object of class \code{similarity}
#' 
similarity <- function(phylo, depth = 1) {
  new('similarity', 
      phylo = phylo, 
      depth = depth)
}

