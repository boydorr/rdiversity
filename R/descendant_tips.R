#' descendant_tips
#'
#' @param tree object of class \code{phylo}.
#' @param node object of class \code{numeric}.
#'
#' @noRd
#'
descendant_tips <- function(tree, node) {
  x <- node
  tips <- seq_len(length(tree$tip.label))

  if(node %in% tips) return(node) else {
    keep <- vector()
    continue <- TRUE

    while(continue) {
      check <- tree$edge[which(tree$edge[, 1] %in% x), 2]
      check
      daughters <- tips[tips %in% check]
      daughters
      keep <- c(keep, daughters)
      keep
      x <- check[!check %in% tips]
      x
      continue <- ifelse(length(x) != 0, TRUE, FALSE)
    }
    return(sort(keep))
  }
}
