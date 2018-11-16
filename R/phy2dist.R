#' Phylogenetic pairwise tip distance matrix
#' 
#' Converts any phylo object to a matrix of pairwise tip-to-tip distances.
#' 
#' @param tree object of class \code{phylo}.
#' @return \code{phy2sim(x)} returns a matrix of pairwise distances.
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(5)
#' phy2dist(tree)
#' 
phy2dist <- function(tree) {
  
  dist <- stats::cophenetic(tree)
  
  new("distance", 
      distance = dist,
      datID = "phylodist")
  
}