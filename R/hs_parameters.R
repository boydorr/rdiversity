#' Historical species parameters
#'
#' Internal function, which extracts various parameters associated with
#' historical species.
#'
#' @param tree object of class \code{phylo}.
#'
#' @return Returns parameters associated with each historic species.
#'
hs_parameters <- function(tree) {
  # Perform checks
  if (class(tree) != "phylo") stop("'tree' argument must be class phylo.")

  # If root has a length
  root_ancestor <- 0
  long_root <- ifelse(!is.null(tree$root.edge), TRUE, FALSE)

  # Ancestral species
  anc_nodes <- lapply(seq_along(tree$tip.label), function(x) {
    res <- c(x, ancestral_nodes(tree, x))
    if (long_root) c(res, root_ancestor) else res
  })

  root_node <- length(tree$tip.label) + 1

  if (long_root) {
    tree$edge <- rbind(tree$edge, c(0, root_node))
    tree$edge.length <- c(tree$edge.length, tree$root.edge)
  }

  # Historic species data for each present day species
  parameters <- lapply(seq_along(anc_nodes), function(x) {
    daughters <- anc_nodes[[x]]
    if (long_root) daughters <- c(daughters, 0)
    daughters <- daughters[-length(daughters)]
    res <- lapply(as.list(daughters), function(y)
      tree$edge[tree$edge[, 2] %in% y, ])
    res <- do.call(rbind.data.frame, res)
    res <- cbind.data.frame(tree$tip.label[x], x, res)
    colnames(res) <- c("tip_label", "tip_node", "a_node", "d_node")
    lengths <- vapply(res$d_node, function(x) which(tree$edge[, 2] %in% x),
                      numeric(1))
    lengths <- tree$edge.length[lengths]
    cbind.data.frame(res, lengths)
  })
  parameters <- do.call(rbind.data.frame, parameters)

  # Historic species names
  hs_names <- paste(parameters$tip_node,
                    paste(parameters$a_node,
                          parameters$d_node, sep = "-"), sep = ",")
  cbind.data.frame(hs_names, parameters)
}


#' ancestral_nodes
#'
#' @param tree object of class \code{phylo}.
#' @param node object of class \code{numeric}.
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

