#' ancestral_nodes
#'
#' @param tree object of class \code{phylo}.
#' @param node object of class \code{numeric}.
#'
#' @noRd
#'
ancestral_nodes <- function(tree, node) {
  x <- node
  store <- vector()
  continue <- TRUE
  while(continue) {
    x <- tree$edge[tree$edge[,2] == x][1]
    store <- c(store, x)
    continue <- any(tree$edge[,2] == x)
  }
  store
}
