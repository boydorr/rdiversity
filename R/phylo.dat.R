#' Phylogenetic data
#'
#' Extract historic species names, ancestral and descendant nodes, descendant
#' present-day species, and proportional abundance. Output as object
#'
#' @param tree object of class \code{phylo}
#' @param pds.abundance \code{vector} of length \eqn{S}; proportional
#' abundance of present-day species
#'
#' @return S4 generic of class \linkS4class{rdphylo}
#'
as.rdphylo <- function(tree,
                      pds.abundance = rep(1/length(tree$tip.label),
                                          length(tree$tip.label))) {
  pds.nodes <- 1:length(tree$tip.label)
  hs.names <- unlist(sapply(pds.nodes, function(x) label.hs(tree, x)))
  
  N.hs <- length(hs.names)
  cat(N.hs, 'historic species in phylogeny\n')
  hs.pds <- sapply(hs.names, function(x) as.numeric(strsplit(x,",")[[1]][1]))
  hs.edge <- t(sapply(hs.names, function(x) as.numeric(strsplit(strsplit(x,",")[[1]][2],'-')[[1]])))
  
  # Calculate Lj for each pds; total length of evolutionary change
  Lj <- sapply(pds.nodes, function(x) calc.Lj(tree, x))
  
  # Calculate the mean total evolutionary change over all pds
  Tbar <- sum(pds.abundance*Lj)
  
  hs.length <- sapply(1:N.hs, function(x) {
    which.edge <- which(apply(tree$edge, 1, function(y) all.equal(y, hs.edge[x,]))==T)
    tree$edge[which.edge]
  })
  
  hs.abundance <- (hs.length/Tbar) * pds.abundance[hs.pds]

  output <- new('rdphylo', tree,
                hs.name = hs.names,
                hs.pds = hs.pds,
                hs.edge = hs.edge,
                hs.abundance = hs.abundance)
  return(output)
}



#' Historic Species
#' 
#' This function constructs unique identifiers for all historic species 
#' ancestral to a given node. Unique identifiers take the form 
#' \emph{(pds, node-tip)}, where \emph{pds} corresponds to the index associated 
#' with the present day species descendant, and node-tip corresponds to the 
#' node index and tip index associated with the historic species itself.   
#' 
#' @param tree object of class \code{phylo}
#' @param node integer corresponding to the node of interest
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


#' Total evolutionary change
#' 
#' Calculate the total length of evolutionary change of species \emph{j}; may 
#' be in internal or external node corresponding to present-day and historic 
#' species, respectively.
#' 
#' @inheritParams label.hs
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
