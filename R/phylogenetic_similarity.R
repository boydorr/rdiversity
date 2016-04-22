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
phylogenetic_similarity <- 
  function(tree, pds.abundance = rep(1/length(tree$tip.label), 
                                     length(tree$tip.label))) 
  {
    if(is.vector(pds.abundance)) pds.abundance <- matrix(pds.abundance)
      
      # Label historic species
      pds.nodes <- 1:length(tree$tip.label)
    hs.names <- unlist(sapply(pds.nodes, function(x) label.hs(tree, x)))
    
    # Check pds.pmatrix
    if(sum(pds.abundance) != 1) pds.abundance <- pds.abundance/sum(pds.abundance)
    if(class(tree)=='phylo') {
      new.tree <- as.rdphylo(tree)
    } else if(is.rdphylo(tree)) {
      new.tree <- tree
    } else
      stop('tree should be object of class phylo (or rdphylo).')  
    
    # Count historic species
    N.hs <- length(hs.names)
    
    # Extract present day species, ancestral and desendant nodes associated with
    # each historic species
    hs.pds <- sapply(hs.names, function(x) as.numeric(strsplit(x,",")[[1]][1]))
    hs.edge <- t(sapply(hs.names, function(x) 
      as.numeric(strsplit(strsplit(x,",")[[1]][2],'-')[[1]])))
    
    # Calculate Lj for each pds; total length of evolutionary change
    Lj <- sapply(pds.nodes, function(x) calc.Lj(tree, x))
    
    # Calculate the mean total evolutionary change over all pds
    Tbar <- apply(pds.abundance, 2, function(x) sum(x*Lj))
    
    # Calculate the length of each historic species
    hs.length <- sapply(1:N.hs, function(x) {
      which.edge <- which(apply(tree$edge, 1, 
                                function(y) all.equal(y, hs.edge[x,]))==T)
      tree$edge[which.edge]
    })
    
    # Calculate the relative abundance of each historic species
    hs.abundance <- apply(pds.abundance, 2, 
                          function(x) (hs.length/Tbar) * x[hs.pds])
    
    # Define Z-matrix (type = historic species)
    zmatrix <- matrix(NA, nrow = N.hs, ncol = N.hs)
    colnames(zmatrix) <- hs.names; row.names(zmatrix) <- hs.names
    
    # Calculate pairwise similarity between historic species
    for (row.index in 1:N.hs) {
      # cat("\r", "Calculating Z matrix: row", row.index, "of", N.hs) 
      # flush.console()
      # Historic species 
      ib <- hs.names[row.index]
      # Present day species descendant
      ib.pds <- as.numeric(strsplit(ib,'-')[[1]][2])
      
      zmatrix.row <- vector()
      for (col.index in 1:N.hs) {
        # Historic species (to compare)
        jc <- hs.names[col.index]
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


#' @rdname phylogenetic_similarity
#' @param node integer corresponding to the node of interest
#' 
#' @details
#' This function constructs unique identifiers for all historic species 
#' ancestral to a given node. Unique identifiers take the form 
#' \emph{(pds, node-tip)}, where \emph{pds} corresponds to the index associated 
#' with the present day species descendant, and node-tip corresponds to the 
#' node index and tip index associated with the historic species itself.   
#' 
#' @return object of class \code{character}
#' 
label.hs <- function(tree, node)
{
  mothers <- phangorn::Ancestors(tree, node, 'all')
  daughters <- c(node, mothers[-length(mothers)])
  hs.name <- sapply(daughters, function(x) {
    branch.name <- paste(tree$edge[which(tree$edge[,2]==x),], collapse='-')
    paste(node,branch.name,sep=',')})
  return(hs.name)
}


#' @rdname phylogenetic_similarity
#' 
#' @details
#' Calculate the total length of evolutionary change of species \emph{j}; may 
#' be in internal or external node corresponding to present-day and historic 
#' species, respectively.
#' 
#' @return object of class \code{numeric}
#' 
calc.Lj <- function(tree, node) 
{
  mothers <- phangorn::Ancestors(tree, node, 'all')
  daughters <- c(node, mothers[-length(mothers)])
  hs.length <- sapply(daughters, function(x) 
    tree$edge.length[which(tree$edge[,2]==x)])
  hs.length <- sum(hs.length)
  return(hs.length)
}

#' 
#' #' @rdname as.rdphylo
#' #' @param x any R object 
#' #' 
#' is.rdphylo <-
#'   function (x)
#'   {
#'     inherits(x, "rdphylo")
#'   }
