#' Function to cut phylogeny from present day species to a specified depth.
#'
#' @param partition proportional abundance of types in the subcommunity as a 
#' fraction of the metacommunity as a whole (in the phylogenetic case, this 
#' corresponds to the proportional abundance of present day species)
#' @param ps \code{phy_struct()} output
#' @param depth proportion of total tree height to be conserved (taken as
#' a proportion from the heighest tip). Describes how far back we go in the tree,
#' with 0 marking the date of the most recent tip, and 1 (the default) marking
#' the most recent common ancestor. Numbers greater than 1 extend the root of
#' the tree
#'
#' @export
#' @return
#' Returns an object of class \code{phy_struct} containing a new structural
#' matrix ('@structure').and the original phylogenetic parameters
#' ('@parameters').
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree, partition)
#'
#' a <- chainsaw(partition, ps, depth = 0.9)
#' b <- chainsaw(partition, ps, depth = 2)
#' z <- chainsaw(partition, ps, depth = 0)
#' m <- chainsaw(partition, ps, depth = 1)
#'
chainsaw <- function(partition, ps, depth) {
  if(!missing(depth))if(length(depth) > 1)
    stop("Only one value may be input as 'depth'")
  
  partition <- check_phypartition(tip_labels = colnames(ps$structure), 
                                  partition = partition)
  
  if(isTRUE(all.equal(1, depth))) {
    # If depth = 1, return original phylogeny
    structure_matrix <- ps$structure
    T_bar <- ps$tbar
    parameters <- ps$parameters
    
  }else if(isTRUE(all.equal(0, depth))) {
    # If depth = 0, remove phylogeny
    old_struct <- ps$structure*ps$tbar
    lineage_heights <- colSums(old_struct)
    tree_height <- max(lineage_heights)
    present_day_species <- sapply(lineage_heights, function(x)
      isTRUE(all.equal(tree_height, x)))
    partition <- partition[present_day_species,]
    cut_meta <- metacommunity(partition)
    return(cut_meta)
    
  }else if(depth > 1) {
    # if depth is greater than 1
    old_struct <- ps$structure*ps$tbar
    tree_height <- max(colSums(old_struct))
    cut_depth <- tree_height - (tree_height * depth)
    
    rooted_tree <- ps$tree
    rooted_tree$root.edge <- abs(cut_depth)
    ps <- phy_struct(rooted_tree, partition)     # Could make this faster
    
    structure_matrix <- ps$structure
    T_bar <- ps$tbar
    parameters <- ps$parameters
    
  }else if(depth > 0 & depth < 1){
    # if depth is between 0 and 1
    old_struct <- ps$structure*ps$tbar
    tree_height <- max(colSums(old_struct))
    cut_depth <- tree_height - (tree_height * depth)
    
    # Extract branch lengths
    index <- lapply(seq_along(colnames(old_struct)),
                    function(x) which(old_struct[,x]>0))
    
    index <- lapply(seq_along(index), function(x)
      cbind.data.frame(sp = x,
                       first_branch = index[[x]][1],
                       last_branch = index[[x]][length(index[[x]])]))
    index <- do.call(rbind.data.frame, index)
    
    # Edit $structure matrix
    structure_matrix <- old_struct
    for(i in 1:nrow(index)) { # for each species
      lineage <- structure_matrix[index$last_branch[i]:index$first_branch[i],
                                  i, drop=FALSE]
      cut_here <- cut_depth
      j = 0
      while(cut_here > 0) {
        j <- j + 1
        cut_here <- cut_here - lineage[j,1]
        if(nrow(lineage) == j) break
      }
      lineage[1:j,1] <- 0
      if(cut_here < 0)
        lineage[j,1] <- abs(cut_here)
      
      structure_matrix[index$last_branch[i]:index$first_branch[i],i] <- lineage
    }
    
    # Remove species that are no longer present
    missing_species <- which(sapply(colSums(structure_matrix),
                                    function(x) isTRUE(all.equal(x, 0))))
    if(!isTRUE(all.equal(length(missing_species), 0)))
      structure_matrix <- structure_matrix[,-missing_species, drop = FALSE]
    
    # Remove historic species that are no longer present
    missing_hs <- which(sapply(rowSums(structure_matrix),
                               function(x) isTRUE(all.equal(x, 0))))
    if(!isTRUE(all.equal(length(missing_hs), 0)))
      structure_matrix <- structure_matrix[-missing_hs,, drop = FALSE]
    
    # Edit $parameters
    parameters <- ps$parameters
    parameters <- parameters[parameters$hs_names %in% row.names(structure_matrix),]
    
    # Remove species that are no longer present
    partition <- partition[which(row.names(partition) %in%
                                   colnames(structure_matrix)),, drop = FALSE]
    
    # If no species are present, there is no metacommunity
    if(isTRUE(all.equal(0, sum(partition)))) return(cut_meta = NA)
    
    partition <- partition / sum(partition)
    
    T_bar <- sum(structure_matrix %*% partition)
    
    # New phy_struct() $structure
    structure_matrix <- structure_matrix / T_bar
  }
  
  # Repackage metacommunity object
  hs <- phy_abundance(partition, structure_matrix)
  ps <- list(structure = structure_matrix,
             tbar = T_bar,
             parameters = parameters,
             tree = ps$tree)
  s <- smatrix(ps)
  z <- zmatrix(partition, s, ps)
  cut_meta <- metacommunity(hs, z)
  
  # Fill in 'phylogeny' metacommunity slots
  cut_meta@raw_abundance <- partition
  cut_meta@raw_structure <- structure_matrix
  cut_meta@parameters <- parameters
  
  # Output
  cut_meta
}
