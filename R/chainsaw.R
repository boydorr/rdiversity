#' Cut phylogeny
#' 
#' 
#' @param data object of class \code{rdphylo}
#' @param leaf.abundance proportional abundance of tips
#' @param interval proportion of total tree height to be conserved (taken as 
#' a proportion from the heighest tip)
#' 
#' @return
#' Returns an object of class \code{supercommunity}.
#' 
chainsaw <- function(data, leaf.abundance, interval) {
  if(!is.rdphylo(data))
    stop("data must be class rdphylo.")
  
  # Terminal taxa.
  tip.nodes <- seq_along(data$tip.label)
  all.nodes <- 1:(max(data$edge))
  # if(interval > 1) all.nodes <- c(all.nodes, 0)
  root.node <- length(data$tip.label) + 1
  
  # Abundance of terminal taxa.
  if(missing(leaf.abundance)) {
    leaf.abundance <- matrix(rep(1/length(tip.nodes),length(tip.nodes)))
    row.names(leaf.abundance) <- data$tip.label
  }
  leaf.abundance <- check_partition(leaf.abundance)
  
  if(interval == 1) {
    super <- supercommunity(leaf.abundance, data)
    return(super)
  } 
  
  # Distance from root
  node.height <- ape::node.depth.edgelength(data)
  
  # Height of tree
  max.height <- max(node.height)
  
  cut.depth <- max.height * interval
  
  # Distance from top of tree
  node.depth <- max.height - node.height
  # Remove non-zero rounding errors
  node.depth <- sapply(node.depth, function(x) ifelse(isTRUE(all.equal(0, x)), 0, x))
  node.depth <- cbind.data.frame(all.nodes, node.depth)
  colnames(node.depth) <- c("node", "depth")
  if(interval > 1) {
    root.depth <- abs(cut.depth)
    node.depth <- rbind.data.frame(node.depth, c(0, root.depth))
    
    data$root.edge <- max.height * (interval - 1) 
    data <- rdphylo(leaf.abundance, data)
  }
  
  historic.species <- data@historic.species
  
  # Ancestral nodes
  node.ancestors <-  lapply(as.list(all.nodes), function(x)  
    mothers <- c(x, phangorn::Ancestors(data, x, 'all')))
  # If interval is greater than 1, add the root edge
  if(interval>1) node.ancestors <- lapply(node.ancestors, function(x) c(x,0))
  
  
  # Unharmed branches
  surviving.index <- which(node.depth$depth <= abs(cut.depth))
  surviving.nodes <- node.depth$node[surviving.index]
  unharmed.branches <- surviving.nodes[-na.omit(match(tip.nodes, surviving.nodes))]
  find.unharmed <- historic.species$a.node %in% unharmed.branches
  s.historic <- historic.species[find.unharmed,]
  s.names <- s.historic$hs.name
  
  # Injured branches
  if(interval < 1) {
    injured <- lapply(as.list(surviving.nodes), function(x)  
      mothers <- c(x, phangorn::Ancestors(data, x, 'all')))
    injured <- lapply(injured, function(x) x[2])
    injured <- unique(unlist(injured))
    if(any(injured %in% surviving.nodes))
      injured <- injured[-(stats::na.omit(match(surviving.nodes, injured)))]
    find.injured <- which(historic.species$a.node %in% injured)
    check.injured <- historic.species[find.injured,]
    truly.injured <- which(check.injured$d.node %in% surviving.nodes)
    i.historic <- check.injured[truly.injured,]
    i.names <- i.historic$hs.name 
    
    pruned <- c(s.names, i.names)
    pruned.historic <- rbind.data.frame(s.historic, i.historic)
    
  } else if (interval > 1) {
    pruned <- s.names
    pruned.historic <- s.historic
    
  }
  
  # Pruned historic species
  pruned <- pruned[order(pruned)]
  pruned.structure <- data@structure[pruned, pruned]
  
  # Pruned terminal taxa
  remaining.tips <- surviving.nodes[surviving.nodes %in% tip.nodes]
  
  index.terminal <- data@terminal.taxa$tip.node %in% remaining.tips
  pruned.terminal <- data@terminal.taxa[index.terminal,]
  
  # Total evolutionary lengths - pruned.
  remaining.ancestors <- node.ancestors[remaining.tips]
  names(remaining.ancestors) <- remaining.tips
  
    Lj <- lapply(seq_along(remaining.ancestors), function(x) {
      these.ancestors <- remaining.ancestors[[x]]

      if(interval < 1) {
        length.remaining <- cut.depth
      } else if (interval > 1) {
        length.remaining <- root.depth
      }
      
      # for non-ultrametric trees, subtract the empty space above the tip
      this.pds <- as.numeric(names(remaining.ancestors)[x])
      length.remaining <- length.remaining - node.depth$depth[this.pds]
      
      sum.these <- list()
      for (i in seq_along(these.ancestors)) {
        this.node <- these.ancestors[i]
        find.length <- pruned.historic$d.node %in% this.node
        hs.length <- unique(pruned.historic$length[find.length])
        length.remaining <- length.remaining - hs.length
        sum.these <- c(sum.these, hs.length)
        
        if(isTRUE(all.equal(0,length.remaining))) {
          break
        }else if(length.remaining > 0) {
          next
        }else if (length.remaining < 0) {
          # if(interval < 1) {
          new.length <- hs.length + length.remaining
          row.index <- pruned.historic$d.node %in% this.node
          pruned.historic$length[row.index] <- new.length
          # Edit sum vector
          sum.these[length(sum.these)] <- new.length
          # }
          break
        }}
      sum(unlist(sum.these))
    })
  
  Lj <- cbind.data.frame(remaining.tips, unlist(Lj))
  colnames(Lj) <- c("tip.node", "Lj")
  
  new.leaf.abundance <- leaf.abundance[remaining.tips, , drop = FALSE]
  new.leaf.abundance <- check_partition(new.leaf.abundance)
  
  new.Tbar <- sum(new.leaf.abundance * Lj$Lj)
  
  # Similarity between historic species (i,b) and species (j,c)  
  # is non-zero when species j is found within the set of species  
  # descended from branch b
  hs.Lj <- merge(pruned.historic, Lj)
  hs.Lj <- hs.Lj[order(hs.Lj$hs.name),]
  
  # row.index <- match(row.names(pruned.structure), hs.Lj$hs.name)
  # j.lengths <- hs.Lj$Lj[row.index]
  j.lengths <- hs.Lj$Lj
  
  scaling.factor <- new.Tbar / j.lengths
  scaling.matrix <- diag(scaling.factor, nrow(hs.Lj))
  zmatrix <- pruned.structure %*% scaling.matrix
  colnames(zmatrix) <- row.names(zmatrix)
  
  # Relative abundance of historic species
  hs.abundance <- sapply(seq_along(pruned.historic$hs.name), function(x) {
    row.index <- match(pruned.historic$tip.node[x], pruned.terminal$tip.node)
    (pruned.historic$length[x] / new.Tbar) * pruned.terminal$pds.abundance[row.index]
  })
  
  # Reinstate partitions
  hs.abundance <- cbind.data.frame(pruned.historic, hs.abundance)
  index <- as.list(seq_along(hs.abundance$hs.name))
  type_abundance <- lapply(index, function(x) {
    row.index <- hs.abundance$tip.node[x]
    if(ncol(leaf.abundance) == 1) {
      col.index <- which(leaf.abundance[row.index] > 0)
    } else
      col.index <- which(leaf.abundance[row.index,] > 0)
    vec <- t(matrix(rep(0, ncol(leaf.abundance))))
    vec[,col.index] <- hs.abundance$hs.abundance[x]
    vec
  })
  type_abundance <- do.call(rbind, type_abundance)
  row.names(type_abundance) <- hs.abundance$hs.name
  colnames(type_abundance) <- colnames(leaf.abundance)
  
  pruned.pds <- type_abundance / sum(type_abundance)
  pruned.pds <- pruned.pds[order(row.names(pruned.pds)),,drop=F]

  # Input similarity as matrix, not phylo, so partition must have the same 
  # number of species (i.e. abundance of historic species, not tips)
  supercommunity(pruned.pds, zmatrix)
} 
