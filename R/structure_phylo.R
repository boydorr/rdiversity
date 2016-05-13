#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param tree object of class \code{phylo}
#' @param historic.species vector of length {S}; relative abundance of present-day 
#' species
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' new.tree <- rdphylo(tree = tree)
#' new.tree@structure
#' 
structure_phylo <- 
  function(tree, historic.species) 
  {
    # Count historic species
    N.hs <- nrow(historic.species)
    pds <- seq_along(tree$tip.label)
    
    # Define smatrix (type = historic species)
    smatrix <- matrix(NA, nrow = N.hs, ncol = N.hs)
    colnames(smatrix) <- historic.species$hs.name 
    row.names(smatrix) <- historic.species$hs.name
    
    # Calculate pairwise similarity between historic species
    for (row.index in seq_along(historic.species$hs.name)) {
      # Historic species 
      ib <- historic.species$hs.name[row.index]
      daughters <- unlist(historic.species$pds.descendants[row.index])
      
      smatrix.row <- vector()
      for (col.index in seq_along(historic.species$hs.name)) {
        # Historic species (to compare)
        jc <- historic.species$hs.name[col.index]
        jc.pds <- historic.species$tip.node[col.index]
        # Similarity between historic species (i,b) and species (j,c)  
        # is non-zero when species j is found within the set of species  
        # descended from branch b
        if (jc.pds %in% daughters) {
          # zmatrix.row[jc] <- new.tree@parameters$Tbar[1] / j.length
          smatrix.row[jc] <- 1
        } else {
          smatrix.row[jc] <- 0
        }
      }
      smatrix[ib,] <- smatrix.row
    }
    smatrix
  }

