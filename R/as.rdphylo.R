#' Coerse to rdphylo
#' 
#' Functions to check if an object is a \code{rdphylo} or coerce an  
#' object into a \code{rdphylo}.
#' 
#' @param tree object of class \code{phylo}
#' @param pds.abundance \code{vector} of length \eqn{S}; proportional
#' abundance of present-day species
#' 
#' @return S4 generic of class \code{rdphylo} containing historic 
#' species: names, ancestral and descendant nodes, descendant present day 
#' species, and proportional abundance. 
#' @include class-rdphylo.R 
#' 
#' @exportClass rdphylo
#' 
rdphylo <- function(tree,
                    pds.abundance = matrix(rep(1/length(tree$tip.label),
                                               length(tree$tip.label)))) {
  # Label historic species
  pds.nodes <- seq_along(tree$tip.label)
  
  # Extract historic species names  
  hs.names <- unlist(sapply(pds.nodes, function(x) label.hs(tree, x)))
  
  new.tree <- tree
  # If root has a length
  if(!is.null(new.tree$root.edge)) {
    hs.names <- c(hs.names, 
                  paste(seq_along(new.tree$tip.label), 
                        paste(0, length(new.tree$tip.label)+1, sep="-"), sep=","))
    new.tree$edge <- rbind(new.tree$edge, c(0, length(new.tree$tip.label)+1))
    new.tree$edge.length <- c(new.tree$edge.length, new.tree$root.edge)
  }
 
  # Check pds.pmatrix
  if(sum(pds.abundance) != 1) pds.abundance <- pds.abundance/sum(pds.abundance)
  
  # Extract present day species, ancestral and desendant nodes associated with
  # each historic species
  hs.pds <- sapply(hs.names, function(x) as.numeric(strsplit(x,",")[[1]][1]))
  hs.edge <- t(sapply(hs.names, function(x) 
    as.numeric(strsplit(strsplit(x,",")[[1]][2],'-')[[1]])))
  
  # Calculate Lj for each pds; total length of evolutionary change
  Lj <- sapply(pds.nodes, function(x) calc.Lj(new.tree, x))
  
  # Calculate the mean total evolutionary change over all pds
  Tbar <- sum(pds.abundance*Lj)
  
  # Calculate the length of each historic species
  hs.length <- sapply(seq_along(hs.names), function(x) {
    which.edge <- which(apply(new.tree$edge, 1, 
                              function(y) isTRUE(all.equal(y, hs.edge[x,]))))
    new.tree$edge.length[which.edge][1]
  })
  
  # Calculate the relative abundance of each historic species
  hs.abundance <- sapply(seq_along(hs.length),
                        function(x) (hs.length[x]/Tbar) * pds.abundance[hs.pds[x]])
  hs.abundance <- as.matrix(hs.abundance)
  row.names(hs.abundance) <- hs.names
  
  # Extract branch descendants
  internal_nodes <- 1:(max(tree$edge))
  branch_descendants <- lapply(as.list(internal_nodes), function(x) 
    phangorn::Descendants(tree, x, 'all'))
  names(branch_descendants) <- internal_nodes
  
  output <- new('rdphylo', new.tree,
                hs.name = hs.names,
                hs.pds = hs.pds,
                hs.edge = hs.edge,
                hs.length = hs.length,
                hs.abundance = hs.abundance,
                Lj = Lj,
                Tbar = Tbar,
                branch_descendants = branch_descendants)
output
}


#' @rdname rdphylo
#' 
as.rdphylo <- rdphylo


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
hs.name
}


#' Total evolutionary change
#' 
#' Calculate the total length of evolutionary change of species \emph{j}; may 
#' be an internal or external node corresponding to present-day and historic 
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
  res <- sum(hs.length)
  # If root has a length
  if(!is.null(tree$root.edge)) res <- res + tree$root.edge
  res
}


#' @rdname rdphylo
#' @param x any R object 
#' 
is.rdphylo <-
  function (x)
  {
    inherits(x, "rdphylo")
  }


#' @rdname rdphylo
#' @param object object of class \code{rdphylo}
#' 
setMethod(f = "show", signature(object = "rdphylo"),
          definition = function(object){
            cat('Phylogenetic tree with', length(object$tip.label),
                'tips and', object$Nnode,
                'internal nodes (including the root).\n\n')
            
            cat('Tip labels:\n', head(object$tip.label), '\n\n')
            # 
            # if(ape::is.rooted(object)) {
            #   rooted <- 'Rooted'
            # } else rooted <- 'Unrooted'
            
            # cat(rooted, '.')
          } )
