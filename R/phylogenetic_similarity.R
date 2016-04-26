#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param tree object of class \code{phylo}
#' @param pds.abundance vector of length {S}; relative abundance of present-day 
#' species
#' 
#' @details 
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' 
#' tree <- ape::rtree(n = 5)
#' Z <- phylogenetic_similarity(tree)
#' 
#' 
similarity_phylo <- 
  function(tree, pds.abundance = rep(1/length(tree$tip.label), 
                                     length(tree$tip.label))) 
  {
    if(is.vector(pds.abundance)) pds.abundance <- matrix(pds.abundance)
      
    if(class(tree)=='phylo') {
      new.tree <- as.rdphylo(tree)
    } else if(is.rdphylo(tree)) {
      new.tree <- tree
    } else
      stop('tree should be object of class phylo (or rdphylo).')  
    
    # Count historic species
    N.hs <- length(new.tree@hs.name)
    
    # Define Z-matrix (type = historic species)
    zmatrix <- matrix(NA, nrow = N.hs, ncol = N.hs)
    colnames(zmatrix) <- new.tree@hs.name 
    row.names(zmatrix) <- new.tree@hs.name
    
    # Calculate pairwise similarity between historic species
    for (row.index in 1:N.hs) {
      # cat("\r", "Calculating Z matrix: row", row.index, "of", N.hs) 
      # flush.console()
      # Historic species 
      ib <- new.tree@hs.name[row.index]
      # Present day species descendant
      ib.pds <- as.numeric(strsplit(ib,'-')[[1]][2])
      
      zmatrix.row <- vector()
      for (col.index in 1:N.hs) {
        # Historic species (to compare)
        jc <- new.tree@hs.name[col.index]
        # Present day species descendant
        jc.pds <- as.numeric(strsplit(jc,",")[[1]][1])
        # Length of evolutionary history of present day species j
        j.length <- new.tree@Lj[jc.pds]
        
        # Similarity between historic species (i,b) and species (j,c)  
        # is non-zero when species j is found within the set of species  
        # descended from branch b
        if ((ib.pds==jc.pds)) {
          
          zmatrix.row[jc] <- new.tree@Tbar/j.length
        } else {
          zmatrix.row[jc] <- 0
        }
      }
      zmatrix[ib,] <- zmatrix.row
    }
    return(zmatrix)
  }

