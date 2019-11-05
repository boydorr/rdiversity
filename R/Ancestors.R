#' Ancestors
#'
#' Function taken from the phangorn package
#'
Ancestors <- function (x, node, type = c("all", "parent")) {
  parents <- x$edge[, 1]
  child <- x$edge[, 2]
  pvector <- integer(max(x$edge))
  pvector[child] <- parents
  type <- match.arg(type)
  if (type == "parent")
    return(pvector[node])
  anc <- function(pvector, node) {
    res <- numeric(0)
    repeat {
      anc <- pvector[node]
      if (anc == 0)
        break
      res <- c(res, anc)
      node <- anc
    }
    res
  }
  if (!missing(node) && length(node) == 1)
    return(anc(pvector, node))
  else allAncestors(x)[node]
}

