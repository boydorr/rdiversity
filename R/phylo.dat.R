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
phylo.dat <- function(tree,
                      pds.abundance = rep(1/length(tree$tip.label),
                                          length(tree$tip.label))) {
  pds.nodes <- 1:length(tree$tip.label)
  
  hs.name <- unlist(sapply(pds.nodes, function(x) hs.names(tree, x)))
  Ntype <- length(hs.name)
  cat(Ntype, 'historic species in phylogeny\n')
  
  hs.pds <- sapply(hs.name, function(x) as.numeric(strsplit(x,",")[[1]][1]))
  hs.edge <- t(sapply(hs.name, function(x) as.numeric(strsplit(strsplit(x,",")[[1]][2],'-')[[1]])))

  # Calculate Lj for each pds; total length of evolutionary change
  Lj <- sapply(pds.nodes, function(x) calc.Lj(tree, x))
  
  # Calculate the mean total evolutionary change over all pds
  Tbar <- sum(pds.abundance*Lj)
  
  hs.length <- sapply(1:Ntype, function(x) {
    cat('\rCalculating abundance of historic species..', round((x/Ntype)*100), "%")
    which.edge <- which(apply(tree$edge, 1, function(y) all.equal(y, hs.edge[x,]))==T)
    tree$edge[which.edge]
  })
  
  hs.abundance <- (hs.length/Tbar) * pds.abundance[hs.pds]

  output <- new('rdphylo', tree,
                hs.name = hs.name,
                hs.pds = hs.pds,
                hs.edge = hs.edge,
                hs.abundance = hs.abundance)
  return(output)
}





