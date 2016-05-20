#' Convert to rdphylo
#' 
#' Converts an object into class \code{phylo} into class \code{rdphylo}.
#' 
#' @param tree object of class \code{phylo}
#' @param pds.abundance \code{vector} of length \eqn{S}; proportional
#' abundance of present-day species
#' 
#' @return Returns an object of class \code{rdphylo}, which extends class 
#' \code{phylo} and therefore inherits components associated  
#' with terminal taxa (see \code{\link{phylo}}): 
#' 
#' \tabular{ll}{
#' \code{edge} \tab - each row denotes the ancestral and descendant nodes of 
#' an edge (branch) of the tree \cr
#' \code{edge.length} \tab - (optional) length of each branch of the tree \cr
#' \code{tip.label} \tab - labels associated with present-day species 
#' (terminal taxa)  \cr
#' \code{Nnode} \tab - number of internal nodes \cr
#' \code{node.label} \tab - (optional) labels associated with each node in 
#' the tree \cr
#' \code{root.edge} \tab - (optional) length of the branch at the root \cr
#' }
#' 
#' An additional \code{parameters} slot (accessed with @, not $) contains 
#' a data_frame containing components associated with historic species:  
#' 
#' \tabular{ll}{
#' \code{pds} \tab - numeric index associated with the present-day species 
#' (or terminal taxa) descended from each historic species  \cr
#' \code{d.node} \tab - numeric index associated with the descending node of 
#' each historic species  \cr
#' \code{a.node} \tab - numeric index associated with the ascending node of 
#' each historic species \cr
#' \code{hs.name} \tab - unique identifier associated with each historic 
#' species \cr
#' \code{length} \tab - evolutionary length of each historic species \cr
#' \code{Lj} \tab - total length of evolutionary change of species j \cr
#' \code{pds.abundance} \tab - relative abundance of present-day species \cr
#' \code{Tbar} \tab - mean total evolutionary change \cr
#' \code{hs.abundance} \tab - relative abundance of historic species \cr
#' \code{branch.descendants} \tab - present-day species descended from each 
#' branch of the tree \cr
#' }
#' 
#' @include class-rdphylo.R 
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' pop <- rep(1/5, 5)
#' rdtree <- rdphylo(pop, tree)
#' 
#' pds.abundance <- data.frame(a = c(0,1,1,1,0), b = c(1,0,0,0,1)) 
#' pds.abundance <- check_partition(pds.abundance)
#' 
#' # Extract components of terminal taxa
#' str(rdtree)
#' rdtree$edge
#' 
#' # Extract components of historic species
#' rdtree@historic.species
#' 
rdphylo <- function(pds.abundance, tree) {
  if(missing(pds.abundance))
    pds.abundance <- matrix(rep(1/length(tree$tip.label), 
                                length(tree$tip.label)))
  pds.abundance <- check_partition(pds.abundance)
  
  tip.nodes <- seq_along(tree$tip.label)
  all.nodes <- 1:(max(tree$edge))
  root.node <- length(tip.nodes) + 1
    
  # If root has a length
  root.ancestor = 0
  if(!is.null(tree$root.edge)) {
    long.root = TRUE
    # all.nodes <- c(all.nodes, root.ancestor)
  } else {
    long.root = FALSE
    # all.nodes <- all.nodes
  }

  ancestral.nodes <- lapply(as.list(all.nodes), function(node) {
    res <- c(node, phangorn::Ancestors(tree, node, 'all'))
    if(long.root) res <- c(res, root.ancestor)
    else res
  })
  if(long.root) ancestral.nodes[[length(ancestral.nodes) + 1]] <- 0
  pds.subset <- ancestral.nodes[tip.nodes]
  
  n.historic <- lapply(pds.subset, function(x) length(x)-1)
    
  # Historic species names
  # Constructs unique identifiers for all historic species ancestral to a 
  # given node. Unique identifiers take the form \emph{(pds, node-tip)},
  # where \emph{pds} corresponds to the index associated with the present
  # day species descendant, and node-tip corresponds to the node index and
  # tip index associated with the historic species itself.   
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
  colnames(tmp) <- c("tip.node", "a.node", "d.node")
  historic.species <- cbind.data.frame(hs.name, tmp, stringsAsFactors=FALSE)

  # Historic species lengths
  lengths <- cbind.data.frame(tree$edge, tree$edge.length)
  colnames(lengths) <- c("a.node", "d.node", "length")
  if(long.root) 
    lengths <- rbind.data.frame(lengths, c(root.ancestor, root.node, 
                                           tree$root.edge))
  historic.species <- merge(historic.species, lengths)
  
  # Total length of evolutionary change of species j  
  # May be an internal or external node corresponding to present-day and  
  # historic species, respectively.
  Lj <- lapply(pds.subset, function(x) {
    daughters <- x[-length(x)]
    hs.length <- sapply(daughters, function(y) 
      lengths$length[match(y, lengths$d.node)])
    sum(hs.length)
  })
  Lj <- unlist(Lj)
  terminal.taxa <- cbind.data.frame(tip.label = tree$tip.label, 
                         tip.node = seq_along(pds.subset), 
                         Lj = Lj)

  # Relative abundance of terminal taxa
  just.abundance <- rowSums(pds.abundance)
  tmp <- cbind(tip.nodes, just.abundance)
  colnames(tmp) <- c("tip.node", "pds.abundance")
  terminal.taxa <- merge(terminal.taxa, tmp)
  
  # Relative abundance of terminal taxa for each historic species
  if(ncol(pds.abundance)==1) {
    all.abundance <- cbind(tip.node = historic.species$tip.node, 
                           pds.abundance = pds.abundance[historic.species$tip.node])
  } else 
    all.abundance <- cbind(tip.node = historic.species$tip.node, 
                           pds.abundance = rowSums(pds.abundance[historic.species$tip.node,]))
  
  all.abundance <- merge(historic.species, all.abundance, by="tip.node")
  
  # Mean total evolutionary change
  Tbar <- sum(pds.abundance * Lj)

  # Relative abundance of historic species
  hs.abundance <- sapply(seq_along(historic.species$hs.name), function(x) {
    row.index <- match(historic.species$tip.node[x], terminal.taxa$tip.node)
    (historic.species$length[x] / Tbar) * terminal.taxa$pds.abundance[row.index]
  })
  hs.abundance <- cbind.data.frame(hs.name = historic.species$hs.name, hs.abundance)  
  historic.species <- merge(historic.species, hs.abundance)
  
  # Extract branch descendants
  # if(!is.null(tree$root.edge)) 
  pds.descendants <- lapply(as.list(all.nodes), function(x) 
    cbind(d.node = x, pds.descendants = phangorn::Descendants(tree, x, 'tips')))
  pds.descendants <- do.call(rbind.data.frame, pds.descendants)
  historic.species <- merge(historic.species, pds.descendants, by = "d.node")

  historic.species <- tibble::as_data_frame(historic.species)
  historic.species <- historic.species[,c("hs.name", "a.node", "d.node", 
                                          "tip.node", "length", "pds.descendants")]
  
  terminal.taxa <- tibble::as_data_frame(terminal.taxa)
  terminal.taxa <- terminal.taxa[,c("tip.label", "tip.node", "Lj", 
                                    "pds.abundance")]
  class(terminal.taxa$tip.label) <- "character"
  
  # Structural component of similarity matrix
  structure <- structure_phylo(tree, historic.species)
  
  new('rdphylo', tree, 
      historic.species = historic.species,
      terminal.taxa = terminal.taxa, 
      Tbar = Tbar,
      structure = structure)
}


#' @rdname rdphylo
#' 
as.rdphylo <- rdphylo


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
