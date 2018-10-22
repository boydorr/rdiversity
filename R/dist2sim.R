#' dist2sim
#' 
#' Converts any matrix of pairwise distances into pairwise similarities.
#' 
#' @param dist two-dimensional \code{matrix} of mode \code{numeric} with rows as 
#' types, columns as types, and elements containing the pairwise distance 
#' between types. 
#' @param transform can be either "l", "e1", or "e2".
#' @param k scaling parameter.
#' @param normalise normalise distances to one; can be either true of false.
#' @param max_d object of mode \code{numeric}; 
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
dist2sim <- function(dist, transform, k = 1, normalise = TRUE, max_d = 1) {
  if(normalise) dist <- dist/max(dist)
  
  if(transform == "l") return(pmax(1-(dist/(max_d*k)), 0))
  if(transform == "e1") return(exp(-(dist*k)))
  if(transform == "e2") return(exp(-(dist/(max(dist)*k))))

}