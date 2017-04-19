#' Historical species parameters
#'
#' Extracts various parameters associated with historical species.
#' 
#' @param tree object of class \code{phylo}.
#' 
#' @return Returns parameters associated with each historic species.
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' hs_parameters(tree)
#' 
hs_parameters <- function(tree) {
  # Perform checks
  if(class(tree) != "phylo") stop("'tree' argument must be class phylo.")
  
  # If root has a length
  root_ancestor = 0
  long_root <- ifelse(!is.null(tree$root.edge), TRUE, FALSE)
  
  # Ancestral species
  ancestral_nodes <- lapply(as.list(seq_along(tree$tip.label)), function(x) {
    res <- c(x, phangorn::Ancestors(tree, x, 'all'))
    if(long_root) c(res, root_ancestor) else res
  })
  
  root_node <- ape::Ntip(tree) + 1
  
  if(long_root) {
    tree$edge <- rbind(tree$edge, c(0, root_node))
    tree$edge.length <- c(tree$edge.length, tree$root.edge)
  }
  
  # Historic species data for each present day species
  parameters <- lapply(seq_along(ancestral_nodes), function(x) {
    daughters <- ancestral_nodes[[x]]
    if(long_root) daughters <- c(daughters, 0)
    daughters <- daughters[-length(daughters)]
    res <- lapply(as.list(daughters), function(y) tree$edge[tree$edge[,2] %in% y,])
    res <- do.call(rbind.data.frame, res)
    res <- cbind.data.frame(tree$tip.label[x], x, res)
    colnames(res) <- c("tip_label", "tip_node", "a_node", "d_node")
    lengths <- sapply(res$d_node, function(x) which(tree$edge[,2] %in% x))
    lengths <- tree$edge.length[lengths]
    cbind.data.frame(res, lengths)
  })
  parameters <- do.call(rbind.data.frame, parameters)
  
  # Historic species names
  hs_names <- paste(parameters$tip_node, 
                    paste(parameters$a_node, 
                          parameters$d_node, sep = "-"), sep = ",")
  parameters <- cbind.data.frame(hs_names, parameters)
  tibble::as_data_frame(parameters)
}

