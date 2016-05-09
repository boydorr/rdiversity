#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param tree object of class \code{phylo}
#' @param pds.abundance vector of length {S}; relative abundance of present-day 
#' species
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' Z <- similarity_phylo(tree)
#' 
similarity_phylo <- 
  function(tree, pds.abundance = rep(1/length(tree$tip.label), 
                                     length(tree$tip.label))) 
  {
    if(is.vector(pds.abundance)) pds.abundance <- matrix(pds.abundance)
    
    if(is.rdphylo(tree)) {
      new.tree <- tree
    } else if(class(tree)=='phylo') {
      new.tree <- as.rdphylo(tree, pds.abundance)
    } else 
      stop('tree should be object of class phylo (or rdphylo).')  
    
    # Count historic species
    N.hs <- length(new.tree@parameters$hs.name)
    
    # Define Z-matrix (type = historic species)
    zmatrix <- matrix(NA, nrow = N.hs, ncol = N.hs)
    colnames(zmatrix) <- new.tree@parameters$hs.name 
    row.names(zmatrix) <- new.tree@parameters$hs.name
    
    # Calculate pairwise similarity between historic species
    for (row.index in seq_along(new.tree@parameters$hs.name)) {
      # Historic species 
      ib <- new.tree@parameters$hs.name[row.index]
      daughters <- unlist(new.tree@parameters$branch.descendants[row.index])
      
      zmatrix.row <- vector()
      for (col.index in seq_along(new.tree@parameters$hs.name)) {
        # Historic species (to compare)
        jc <- new.tree@parameters$hs.name[col.index]
        # Present day species descendant
        jc.pds <- new.tree@parameters$pds[col.index]
        # Length of evolutionary history of present day species j
        j.length <- new.tree@parameters$Lj[col.index]
        
        # Similarity between historic species (i,b) and species (j,c)  
        # is non-zero when species j is found within the set of species  
        # descended from branch b
        if (jc.pds %in% daughters) {
          zmatrix.row[jc] <- new.tree@parameters$Tbar[1] / j.length
        } else {
          zmatrix.row[jc] <- 0
        }
      }
      zmatrix[ib,] <- zmatrix.row
    }
    zmatrix
  }

