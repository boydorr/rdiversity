#' Phylogenetic pairwise tip distance matrix
#' 
#' Converts any phylo object to a matrix of pairwise tip-to-tip distances.
#' 
#' @param tree object of class \code{phylo}.
#' @param precompute_dist object of class \code{logical} or \code{numeric}. 
#' When TRUE (by default) a distance matrix is generated and stored in slot 
#' \code{distance}, when FALSE no distance matrix is generated, and when numeric 
#' a distance matrix is generated until the number of species exceeds the 
#' defined value. 
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