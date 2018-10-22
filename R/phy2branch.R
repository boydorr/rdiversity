#' phy2branch
#' 
#' Packages phylo object and depth into an object of class \code{similarity}. 
#' 
#' @param tree object of class \code{phylo}.
#' @param depth proportion of total tree height to be conserved (taken as
#' a proportion from the heighest tip). Describes how much evolutionary history 
#' should be retained, with 0 marking the date of the most recent tip, and 1 
#' (the default) marking the most recent common ancestor. Numbers greater than 
#' 1 extend the root of the tree.
#' @return \code{phy2sim()} returns an object of class \code{similarity}.
#' @export
#' 
phy2branch <- function(tree, depth = 1) {
  similarity(tree, depth)
}