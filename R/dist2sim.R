#' Distance to similarity
#' 
#' Converts any matrix of pairwise distances into pairwise similarities.
#' 
#' Distances are transformed either *linearly* or *exponentially*. That is 
#' \code{1 - k * dist} for non-negative values, or \code{exp(-k * dist)}, 
#' respectively. If \code{normalise} is true, then \code{dist = dist/max_d}.
#' 
#' @param dist two-dimensional \code{matrix} of mode \code{numeric} with rows as 
#' types, columns as types, and elements containing the pairwise distance 
#' between types
#' @param transform can be either "linear", "exponential"
#' @param k scaling parameter
#' @param normalise normalise distances to one; can be either true of false
#' @param max_d object of mode \code{numeric}
#' 
#' @return \code{dist2sim(x)} returns an object of class \code{matrix}.
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(5)
#' dist <- phy2dist(tree)
#' dist2sim(dist, "l")
#' 
#' 
dist2sim <- function(dist, transform, k = 1,
                     normalise = TRUE, max_d = max(dist)) {
  if(normalise) dist <- dist/max_d
  
  if(transform == substr("linear", 1, nchar(transform)))
    return(pmax(1 - k * dist, 0))
  if(transform == substr("exponential", 1, nchar(transform)))
    return(exp(-k * dist))
}