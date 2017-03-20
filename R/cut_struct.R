#' Cut phylogeny
#'
#' @param ps \code{phy_struct()} output.
#' @param interval proportion of total tree height to be conserved (taken as
#' a proportion from the heighest tip). Describes how far back we go in the tree,
#' with 0 marking the date of the most recent tip, and 1 (the default) marking
#' the most recent common ancestor. Numbers greater than 1 extend the root of
#' the tree.
#' @param depth object length of total tree height to be conserved.
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
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' ps <- phy_struct(tree)
#'
#' a <- cut_struct(ps, interval = 0.4)
#' b <- cut_struct(ps, interval = 2)
#' z <- cut_struct(ps, interval = 0)
#' m <- cut_struct(ps, interval = 1)
#'
cut_struct <- function(ps, interval, depth) {
  if(!missing(interval))if(length(interval) > 1)
    stop("Only one value may be input as 'interval'")
  if(!missing(depth))if(length(depth) > 1)
    stop("Only one value may be input as 'depth'")
  if(!missing(interval) & !missing(depth))
    stop("Either 'interval' or 'depth' may be input, not both!")
  
  if(!missing(depth)) {
    tree_height <- max(colSums(ps$structure))
    interval <- depth / tree_height
  }
  
  if(isTRUE(all.equal(1, interval))) {
    # If interval = 1, return original phylogeny
    trim_struct <- ps$structure
    parameters <- ps$parameters
    
  }else if(isTRUE(all.equal(0, interval))) {
    # If interval = 0, remove phylogeny
    trim_struct <- ps$structure
    trim_struct[] <- 0
    parameters <- ps$parameters
    
  }else if(interval > 1){
    # if interval is greater than 1
    tree_height <- max(colSums(ps$structure))
    cut_depth <- tree_height - (tree_height * interval)
    
    rooted_tree <- ps$tree
    rooted_tree$root.edge <- abs(cut_depth)
    ps <- phy_struct(rooted_tree)
    
    trim_struct <- ps$structure
    parameters <- ps$parameters
    
  }else if(interval > 0 & interval < 1){
    # if interval is betweel 0 and 1
    tree_height <- max(colSums(ps$structure))
    cut_depth <- tree_height - (tree_height * interval)
    
    # Find branch lengths
    index <- apply(ps$structure, 2, function(x) which(x>0))
    index <- lapply(seq_along(index), function(x)
      cbind.data.frame(column = x,
                       start_row = index[[x]][1],
                       end_row = index[[x]][length(index[[x]])]))
    index <- do.call(rbind.data.frame, index)
    
    # Edit $structure matrix
    trim_struct <- ps$structure
    for(i in 1:nrow(index)) {
      these_branches <- trim_struct[index$end_row[i]:index$start_row[i],i]
      cut_here <- cut_depth
      j = 0
      while(cut_here > 0) {
        j <- j + 1
        cut_here <- cut_here - these_branches[[j]]
        if(isTRUE(all.equal(length(these_branches), j))) break
      }
      these_branches[1:j] <- 0
      if(cut_here < 0)
        these_branches[j] <- abs(cut_here)
      
      trim_struct[index$end_row[i]:index$start_row[i],i] <- these_branches
    }
    
    # Remove species that are no longer present
    missing_species <- which(sapply(colSums(trim_struct),
                                    function(x) isTRUE(all.equal(x, 0))))
    if(!isTRUE(all.equal(length(missing_species), 0)))
      trim_struct <- trim_struct[,-missing_species, drop = FALSE]
    
    # Remove historic species that are no longer present
    missing_hs <- which(sapply(rowSums(trim_struct),
                               function(x) isTRUE(all.equal(x, 0))))
    if(!isTRUE(all.equal(length(missing_hs), 0)))
      trim_struct <- trim_struct[-missing_hs,]
    
    # Edit $parameters
    parameters <- ps$parameters
    parameters <- parameters[parameters$hs_names %in% row.names(trim_struct),]
  }
  
  list(structure = trim_struct,
       parameters = parameters,
       tree = ps$tree)
}
