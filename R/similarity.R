#' Generate similarity object
#' 
#' Used by \code{phy2branch} to generate a \code{similarity} object.
#' 
#' @param phylo object of class \code{phylo}
#' @param depth object of class \code{numeric}
#' 
#' @return similarity() returns an object of class \code{similarity}
#' 
similarity <- function(phylo, depth = 1) {
  new('similarity', 
      phylo = phylo, 
      depth = depth)
}

