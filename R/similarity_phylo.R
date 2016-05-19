#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param data object of class \code{rdphylo}
#' @param pds.abundance vector of length {S}; relative abundance of present-day 
#' species
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' pop <- rep(1/5, 5)
#' new.tree <- rdphylo(pop, tree)
#' Z <- similarity_phylo(new.tree)
#' 
similarity_phylo <- 
  function(data, pds.abundance) 
  {
    if(!is.null(data$root.edge)) {
      long.root = TRUE
    } else long.root = FALSE
    
    pds <- seq_along(data$tip.label)
    tip.nodes <- seq_along(data$tip.label)
    all.nodes <- 1:(max(data$edge))
    root.node <- length(tip.nodes) + 1
    root.ancestor <- 0
    
    ancestral.nodes <- lapply(as.list(all.nodes), function(node) {
      res <- c(node, phangorn::Ancestors(data, node, 'all'))
      if(long.root) res <- c(res, root.ancestor)
      else res
    })
    if(long.root) ancestral.nodes[[length(ancestral.nodes) + 1]] <- 0
    pds.subset <- ancestral.nodes[tip.nodes]
    
    # Branch lengths.
    lengths <- cbind.data.frame(data$edge, data$edge.length)
    if(long.root) 
      lengths <- rbind.data.frame(lengths, c(root.ancestor, root.node, 
                                           data$root.edge))
    colnames(lengths) <- c("a.node", "d.node", "branch_length")

    # Total evolutionary lengths.
    Lj <- lapply(pds.subset, function(x) {
      daughters <- x[-length(x)]
      hs.length <- sapply(daughters, function(y)
        lengths$branch_length[match(y, lengths$d.node)])
      sum(hs.length)
    })
    Lj <- cbind.data.frame(pds, unlist(Lj))
    colnames(Lj) <- c("tip.node", "Lj")
    
    # Similarity between historic species (i,b) and species (j,c)  
    # is non-zero when species j is found within the set of species  
    # descended from branch b
    Tbar <- data@Tbar
    hs.Lj <- merge(data@historic.species, Lj)
    
    row.index <- match(row.names(data@structure), hs.Lj$hs.name)
    j.lengths <- hs.Lj$Lj[row.index]
    
    scaling.factor <- Tbar / j.lengths
    scaling.matrix <- diag(scaling.factor, nrow(hs.Lj))
    zmatrix <- data@structure %*% scaling.matrix
    colnames(zmatrix) <- row.names(zmatrix)
    zmatrix
}

