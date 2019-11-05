#' Descendants
#'
#' Function taken from the Phangorn package
#'
Descendants <- function (x, node, type = c("tips", "children", "all")) {
  type <- match.arg(type)
  if (type == "children")
    return(Children(x, node))
  if (type == "tips")
    return(bip(x)[node])
  if (missing(node) || length(node) > 10)
    return(allDescendants(x)[node])
  ch <- allChildren(x)
  isInternal <- logical(max(x$edge))
  isInternal[unique(x$edge[, 1])] <- TRUE
  desc <- function(node, isInternal) {
    if (!isInternal[node])
      return(node)
    res <- NULL
    while (length(node) > 0) {
      tmp <- unlist(ch[node])
      res <- c(res, tmp)
      node <- tmp[isInternal[tmp]]
    }
    res
  }
  if (length(node) > 1)
    return(lapply(node, desc, isInternal))
  desc(node, isInternal)
}
