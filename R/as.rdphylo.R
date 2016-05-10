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
#' components associated with historic species:  
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
#' rdtree <- rdphylo(tree)
#' 
#' # Extract components of terminal taxa
#' str(rdtree)
#' rdtree$edge
#' 
#' # Extract components of historic species
#' rdtree@parameters
#' 
rdphylo <- function(tree, pds.abundance = NA) {
  if(all(is.na(pds.abundance))) {
    pds.abundance <- matrix(rep(1/length(tree$tip.label), 
                                length(tree$tip.label)))
    pds.abundance <- pds.abundance/sum(pds.abundance)
  }
  
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
  colnames(tmp) <- c("pds", "a.node", "d.node")
  parameters <- cbind.data.frame(hs.name, tmp, stringsAsFactors=FALSE)

  # Historic species lengths
  lengths <- cbind.data.frame(tree$edge, tree$edge.length)
  colnames(lengths) <- c("a.node", "d.node", "length")
  if(long.root) 
    lengths <- rbind.data.frame(lengths, c(root.ancestor, root.node, 
                                           tree$root.edge))
  parameters <- merge(parameters, lengths)
  
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
  tmp <- cbind(pds = seq_along(pds.subset), Lj = Lj)
  parameters <- merge(parameters, tmp)

  # Relative abundance of terminal taxa for each historic species
  if(ncol(pds.abundance)==1) {
    all.pds.abundance <- as.matrix(pds.abundance[parameters$pds])
  } else 
    all.pds.abundance <- pds.abundance[parameters$pds,]
  parameters <- cbind(parameters, pds.abundance = all.pds.abundance)
  
  # Mean total evolutionary change
  Tbar <- sum(pds.abundance * Lj)
  parameters <- cbind(parameters, Tbar)
  
 
  # Relative abundance of historic species
  hs.abundance <- (parameters$length / Tbar) * parameters$pds.abundance
  parameters <- cbind(parameters, hs.abundance)
  
  # Extract branch descendants
  branch_descendants <- lapply(as.list(internal.nodes), function(x) 
    cbind(d.node = x, pds.descendants = phangorn::Descendants(tree, x, 'all')))
  
  branch_descendants <- do.call(rbind, branch_descendants)
  branch_descendants <- as.data.frame(branch_descendants)
  pds.descendants <- NULL # hack to fix 'no visible binding for global variable'
  branch_descendants <- tidyr::nest(branch_descendants, pds.descendants)
  
  parameters <- merge(parameters, branch_descendants, by = "d.node")
  colnames(parameters)[ncol(parameters)] <- "branch.descendants"

  parameters <- tibble::as_data_frame(parameters)
  new('rdphylo', tree, parameters = parameters)
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
