#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param tree object of class \code{phylo}
#' @param pds.abundance vector of length {S}; relative abundance of present-day 
#' species
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' 
phylo.zmatrix <- function(tree, 
                          pds.abundance = rep(1/length(tree$tip.label),
                                              length(tree$tip.label))) 
{
  if(class(tree)=='phylo') {
    new.tree <- as.rdphylo(tree)
  }else if(is.rdphylo(tree)){
    new.tree <- tree
  }else 
    stop('Tree should be an object of class phylo or rdphylo.')
  
  Lj <- new.tree@Lj
  Tbar <- new.tree@Tbar
  hs <- new.tree@hs.name
  Nhistoric <- length(hs)
  
  # Define Z-matrix (type = historic species)
  zmatrix <- matrix(NA, nrow = Nhistoric, ncol = Nhistoric)
  colnames(zmatrix) <- new.tree@hs.name; row.names(zmatrix) <- new.tree@hs.name
  
  # Similarity between historic species (i,b) and (j,c)  
  for (row.index in 1:Nhistoric) {
    cat("\r", "Calculating Z matrix: row", row.index, "of", Nhistoric) 
    flush.console()
    # Historic species 
    ib <- hs[row.index]
    # Present day species descendant
    ib.pds <- as.numeric(strsplit(ib,'-')[[1]][2])
    
    zmatrix.row <- vector()
    for (col.index in 1:Nhistoric) {
      # Historic species (to compare)
      jc <- hs[col.index]
      # Present day species descendant
      jc.pds <- as.numeric(strsplit(jc,",")[[1]][1])
      # Length of evolutionary history of present day species j
      j.length <- Lj[jc.pds]
      
      # Similarity between historic species (i,b) and species (j,c)  
      # is non-zero when species j is found within the set of species  
      # descended from branch b
      if ((ib.pds==jc.pds)) {
        
        zmatrix.row[jc] <- Tbar/j.length
      } else {
        zmatrix.row[jc] <- 0
      }
    }
    zmatrix[ib,] <- zmatrix.row
  }
  return(zmatrix)
}


