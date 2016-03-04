#' 
#' 
#' 
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


#' Historic Species
#' 
#' This function constructs unique identifiers for all historic species 
#' ancestral to a given node. Unique identifiers take the form 
#' \emph{(pds, node-tip)}, where \emph{pds} corresponds to the index associated 
#' with the present day species descendant, and node-tip corresponds to the 
#' node index and tip index associated with the historic species itself.   
#' 
hs.names <- function(tree, node)
{
  mothers <- phangorn::Ancestors(tree, node, 'all')
  daughters <- c(node, mothers[-length(mothers)])
  hs.name <- sapply(daughters, function(x) {
    branch.name <- paste(tree$edge[which(tree$edge[,2]==x),], collapse='-')
    paste(node,branch.name,sep=',')})
  return(hs.name)
}


#' Phylogenetic Similarity Matrix
#' 
#' 
#' 
#' 
phylo.zmatrix <- function(tree, 
                          pds.abundance = rep(1/length(tree$tip.label),
                                              length(tree$tip.label)), 
                          time.interval = NA, 
                          year.last.sample = NA) 
{
  if(class(tree)!='phylo') stop('tree should be object of class phylo.')
  
  # Extract data
  pds.nodes <- 1:length(tree$tip.label)
  
  # Calculate Lj for each pds; total length of evolutionary change
  Lj <- sapply(pds.nodes, function(x) calc.Lj(tree, x))
  
  # Calculate the mean total evolutionary change over all pds
  Tbar <- sum(pds.abundance*Lj)
  
  # How many historic species?
  hs <- unlist(sapply(pds.nodes, function(x) hs.names(tree, x)))
  Ntype <- length(hs)
  
  # Define Z-matrix (type = historic species)
  zmatrix <- matrix(NA, nrow = Ntype, ncol = Ntype)
  colnames(zmatrix) <- hs; row.names(zmatrix) <- hs
  
  # Similarity between historic species (i,b) and (j,c)  
  for (row.index in 1:Ntype) {
    cat("\r", "Calculating Z matrix: row", row.index, "of", Ntype) 
    flush.console()
    # Ancestral species ID
    ib <- hs[row.index]
    # Present day species index
    # pds.ib <- strsplit(ib)
    # Historic species tip 
    hs.tip <- as.numeric(strsplit(ib,"-")[[1]][2])
    # Descendants (pds) of hs.tip
    ib.descendants <- phangorn::Descendants(tree, hs.tip)[[1]]
    
    zmatrix.row <- vector()
    for (col.index in 1:Ntype) {
      # Ancestral species ID
      jc <- hs[col.index]
      # Present day species index
      jc.pds <- as.numeric(strsplit(jc,",")[[1]][1])
      # Length of evolutionary history of present day species j
      j.length <- Lj[jc.pds]
      
      # Similarity between historic species (i,b) and species (j,c)  
      # is non-zero when species j is found within the set of species  
      # descended from branch b
      if (any(ib.descendants %in% jc.pds)) {
        zmatrix.row[jc] <- Tbar/j.length
      } else {
        zmatrix.row[jc] <- 0
      }
    }
    zmatrix[ib,] <- zmatrix.row
  }
  return(zmatrix)
}


