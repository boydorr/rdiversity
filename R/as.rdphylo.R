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
  pds.abundance <- check_partition(pds.abundance)

  pds.nodes <- seq_along(tree$tip.label)
  internal.nodes <- 1:(max(tree$edge))
  root.node <- length(pds.nodes) + 1
    
  # If root has a length
  if(!is.null(tree$root.edge)) {
    long.root = TRUE
    root.ancestor = 0
    all.nodes <- c(internal.nodes, root.ancestor)
  } else {
    long.root = FALSE
    all.nodes <- internal.nodes
  }

  ancestral.nodes <- lapply(as.list(internal.nodes), function(node) {
    res <- c(node, phangorn::Ancestors(tree, node, 'all'))
    if(long.root) res <- c(res, root.ancestor)
    else res
  })
  pds.subset <- ancestral.nodes[pds.nodes]
  
  n.historic <- lapply(pds.subset, function(x) length(x)-1)
    
  # Historic species names
  hs.name <- lapply(pds.subset, function(x) {
    terminal.taxa <- x[1]
    branch.nodes <- sapply(1:(length(x)-1), function(y) {
      paste(terminal.taxa, paste(x[y+1], x[y], sep="-"), sep=",") })
    })
  hs.name <- unlist(hs.name)
  
  # Historic species nodes
  tmp <- lapply(as.list(hs.name), function(x)
    unlist(strsplit(unlist(strsplit(x,"-")),",")))
  tmp <- do.call(rbind, tmp)
  tmp <- as.data.frame(t(apply(tmp, 1, as.numeric)), stringsAsFactors = FALSE)
  colnames(tmp) <- c("pds", "a.node", "d.node")
  parameters <- cbind.data.frame(hs.name, tmp, stringsAsFactors=FALSE)

  # Historic species lengths
  lengths <- cbind.data.frame(tree$edge, tree$edge.length)
  colnames(lengths) <- c("a.node", "d.node", "length")
  if(long.root) 
    lengths <- rbind.data.frame(lengths, c(root.ancestor, root.node, tree$root.edge))
  parameters <- merge(parameters, lengths)
  
  # Total length of evolutionary change
  Lj <- lapply(pds.subset, function(x) {
    daughters <- x[-length(x)]
    hs.length <- sapply(daughters, function(y) 
      lengths$length[match(y, lengths$d.node)])
    sum(hs.length)
  })
  Lj <- unlist(Lj)
  tmp <- cbind(pds = seq_along(pds.subset), Lj = Lj)
  parameters <- merge(parameters, tmp)

  # Relative abundance of terminal taxa for each historic species
  if(ncol(pds.abundance)==1) {
  hs.abundance <- (hs.length/Tbar) * all.pds.abundance
  row.names(hs.abundance) <- hs.names
    all.pds.abundance <- as.matrix(pds.abundance[parameters$pds])
  } else 
    all.pds.abundance <- pds.abundance[parameters$pds,]
  parameters <- cbind(parameters, pds.abundance = all.pds.abundance)
  
  
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




#' Total evolutionary change
#' 
#' Calculate the total length of evolutionary change of species \emph{j}; may 
#' be an internal or external node corresponding to present-day and historic 
#' species, respectively.
#' 




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
