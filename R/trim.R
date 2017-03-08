#' Cut phylogeny
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as subcommunities, and elements containing relative 
#' abundances of types in subcommunities. In the case of phylogenetic 
#' metacommunities, these are the relative abundances of terminal taxa. 
#' @param tree object of class \code{phylo}
#' @param interval proportion of total tree height to be conserved (taken as
#' a proportion from the heighest tip). Describes how far back we go in the tree,
#' with 0 marking the date of the most recent tip, and 1 (the default) marking 
#' the most recent common ancestor. Numbers greater than 1 extend the root of 
#' the tree. 
#'
#' @export
#' @return
#' Returns an object of class \code{phy_struct} containing a new structural  
#' matrix ('@structure').and the original phylogenetic parameters 
#' ('@parameters')  
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' 
#' partition <- cbind(a = c(0,1,1,1,0), b = c(1,0,0,0,1))
#' row.names(partition) <- tree$tip.label 
#' partition <- partition / sum(partition)
#' meta <- phy_struct(partition, tree)
#'
#' res <- trim(partition, tree, 0.2)
#' 
trim <- function(partition, tree, interval) {
  if(class(tree) != "phylo") stop("'tree' must be an object of class phylo")
  if(class(interval) == "vector") stop("Only one value may be input as 'interval'")

  long_root <- ifelse(!is.null(tree$root.edge), TRUE, FALSE)
  
  if(interval == 1) {
    # If interval = 1, return original phylogeny 
    ps <- phy_struct(partition, tree)
    trim_struct <- ps@structure
    
  }else if(isTRUE(all.equal(0, interval))) {
    # If interval = 0, remove phylogeny 
    ps <- phy_struct(partition, tree)
    trim_struct <- ps@structure
    trim_struct[] <- 0
    
  }else if(interval > 1){
    # If interval > 1, add root to phylogeny
    partition <- check_partition(partition)
    Ntips <- ape::Ntip(tree)
    d_nodes <- 1:ape::Nnode(tree, internal.only = FALSE)
    d_nodes <- d_nodes[-which(d_nodes %in% (Ntips + 1))]
    
    node_heights <- phytools::nodeHeights(tree)
    index <- sapply(d_nodes, function(x) which(tree$edge[,2] %in% x))
    node_heights <- cbind.data.frame(d_node = d_nodes, 
                                     height = node_heights[index, 2])
    # Remove non-zero rounding errors
    node_heights$height <- sapply(node_heights$height, function(x) 
      ifelse(isTRUE(all.equal(0, x)), 0, x))
    
    tree_height <- max(node_heights[,2])
    cut_height <- tree_height - (tree_height * interval)
    
    rooted_tree <- tree
    rooted_tree$root.edge <- abs(cut_height)
    ps <- phy_struct(partition, rooted_tree)
    
    trim_struct <- ps@structure
    
  }else { 
    # if interval is betweel 0 and 1  
    ps <- phy_struct(partition, tree)
    Ntips <- ncol(ps@structure)
    d_nodes <- 1:ape::Nnode(tree, internal.only = FALSE)
    d_nodes <- if(long_root) d_nodes[-which(d_nodes %in% 0)] else 
      d_nodes[-which(d_nodes %in% (Ntips + 1))]
    
    node_heights <- phytools::nodeHeights(tree)
    index <- sapply(d_nodes, function(x) which(tree$edge[,2] %in% x))
    node_heights <- cbind.data.frame(d_node = d_nodes, 
                                     height = node_heights[index, 2])
    # Remove non-zero rounding errors
    node_heights$height <- sapply(node_heights$height, function(x) 
      ifelse(isTRUE(all.equal(0, x)), 0, x))
    
    tree_height <- max(node_heights[,2])
    cut_height <- tree_height - (tree_height * interval)
    
    # Find branch lengths
    index <- apply(ps@structure, 2, function(x) which(x>0))
    index <- lapply(seq_along(index), function(x) 
      cbind.data.frame(column = x, 
                       start_row = index[[x]][1], 
                       end_row = index[[x]][length(index[[x]])]))
    index <- do.call(rbind.data.frame, index)  
    
    trim_struct <- ps@structure
    for(i in 1:nrow(index)) {
      these_branches <- trim_struct[index$end_row[i]:index$start_row[i],i]
      cut_here <- cut_height
      j = 0
      while(cut_here >= 0) {
        j <- j + 1
        if(j > length(these_branches)) break
        cut_here <- cut_here - these_branches[j]
      }
       
      if(cut_here < 0) {
        these_branches[j] <- these_branches[j] - abs(cut_here)
        if(length(these_branches) > j) 
          these_branches[(j+1):length(these_branches)] <- 0
      }
      trim_struct[index$end_row[i]:index$start_row[i],i] <- these_branches
    }
    
  }
  
  new('phy_struct', 
      structure = trim_struct, 
      parameters = ps@parameters)
}
