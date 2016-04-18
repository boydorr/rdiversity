#' Phylogenetic data
#'
#' Functions to check if an object is of class \code{RDphylo} or coerce an 
#' object into class \code{RDphylo}.
#'
#' @param tree object of class \code{phylo}
#' @param pds.abundance \code{vector} of length \eqn{S}; proportional
#' abundance of present-day species
#'
#' @return S4 generic of class \linkS4class{RDphylo} containing historic 
#' species: names, ancestral and descendant nodes, descendant present day 
#' species, and proportional abundance. 
#' @export
#' 
as.RDphylo <- function(tree,
                      pds.abundance = matrix(rep(1/length(tree$tip.label),
                                          length(tree$tip.label)))) {
  # Label historic species
  pds.nodes <- 1:length(tree$tip.label)
  hs.names <- unlist(sapply(pds.nodes, function(x) label.hs(tree, x)))
  
  # Check pds.pmatrix
  if(sum(pds.abundance) != 1) pds.abundance <- pds.abundance/sum(pds.abundance)
  
  # Count historic species
  N.hs <- length(hs.names)
  cat(N.hs, 'historic species in phylogeny\n')
  
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

  output <- new('RDphylo', tree,
                hs.name = hs.names,
                hs.pds = hs.pds,
                hs.edge = hs.edge,
                hs.length = hs.length,
                hs.abundance = hs.abundance,
                Lj = Lj,
                Tbar = Tbar)
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


#' @rdname as.RDphylo
#' @param x any R object 
#' 
is.RDphylo <-
  function (x)
  {
    inherits(x, "RDphylo")
  }
