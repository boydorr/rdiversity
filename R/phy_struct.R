#' Convert to phy_struct
#'
#' Converts an object into class \code{phylo} into class \code{phy_struct}.
#' 
#' @param tree object of class \code{phylo}
#' 
#' @return Returns an object of class \code{phy_struct}, which contains: 
#'
#' \tabular{ll}{
#' \code{/@structure} \tab - each row denotes historic species, columns denote 
#' terminal taxa, and elements contain branch lengths \cr
#' \code{/@parameters} \tab - information associated with each historic species \cr
#' }
#'
#' Additional slots (accessed with @, not $) include:
#' 
#' \tabular{ll}{
#' \code{historic.species} \tab - a data_frame containing components associated 
#' with historic species \cr
#' \code{terminal_taxa} \tab - na data_frame containing components associated 
#' with terminal taxa \cr
#' \code{Tbar} \tab - \cr
#' \code{structure} \tab -  \cr
#' }
#'
#' @include class-phy_struct.R
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' 
#' res <- phy_struct(tree)
#' 
phy_struct <- function(tree) {
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
  pds_subset <- lapply(seq_along(ancestral_nodes), function(x) {
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
  pds_subset <- do.call(rbind.data.frame, pds_subset)
  
  # Historic species names
  hs_names <- paste(pds_subset$tip_node, 
                    paste(pds_subset$a_node, 
                          pds_subset$d_node, sep = "-"), sep = ",")
  pds_subset <- cbind.data.frame(hs_names, pds_subset)
  pds_subset <- tibble::as_data_frame(pds_subset)
  
  # Create structural matrix
  s_matrix <- matrix(0, ncol = length(seq_along(tree$tip.label)), 
                     nrow = nrow(pds_subset))
  row.names(s_matrix) <- pds_subset$hs_names
  colnames(s_matrix) <- tree$tip.label
  index <- sapply(pds_subset$tip_label, function(x) which(colnames(s_matrix) %in% x))
  index <- cbind(row = 1:nrow(pds_subset), col = index)
  s_matrix[index] <- pds_subset$lengths
  
  # Output
  new('phy_struct', 
      structure = s_matrix, 
      parameters = pds_subset)
}


#' @rdname phy_struct
#' @param x any R object
#' @export
#'
is.phy_struct <-
  function (x)
  {
    inherits(x, "phy_struct")
  }


#' @rdname phy_struct
#' @param object object of class \code{phy_struct}
#' @export
#'
setMethod(f = "show", signature(object = "phy_struct"),
          definition = function(object){
            all_tips <- sum(colSums(object@structure) > 0)
            Nhs <- sum(rowSums(object@structure) > 0)
            cat('@phylo_struct: Structural matrix describing', all_tips, 'tips and',
                Nhs, 'historic species.\n')
            cat('@parameters: Historic species parameters.')
          })
