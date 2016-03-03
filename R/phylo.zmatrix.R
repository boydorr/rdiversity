

# Initialize packages for phylo.zmatrix()
require(foreach) 
require(doParallel) 
registerDoParallel(cores = 4)
require(phangorn)


#' ** phylo.pmatrix: Calculates P matrix **
#' 
#' Arguments: 
#' ----------
#' tree             - phylogenetic tree of file type *.nexus
#' 
#' Sub-arguments (used by 'chainsaw' function):
#' --------------------------------------------
#' leaf.abundance   - [optional] vector of the relative abundance of 
#'                    present-day species; default is a completely even
#'                    distribution, 1 / N, where N is the number of present 
#'                    day species
#' time.interval    - [optional] integer defining a time interval, or the 
#'                    number of years prior to the most recent sample; default 
#'                    is the complete phylogeny, where no cut-off is set
#' year.last.sample   - [optional] integer defining the time of the most recent 
#'                    sample; default is only possible when time.interval==NA, 
#'                    such that no cut-off is set
#'
#' Outputs:
#' --------
#' pmatrix       - 

phylo.pmatrix <- function(tree, 
                          leaf.abundance=NA, 
                          time.interval=NA, 
                          year.last.sample=NA) 
{
  # Extract data
  data <- chainsaw(tree, leaf.abundance, time.interval, year.last.sample)
  
  # How many historic species?
  S <- length(data$historic.species)
  
  # Calculate p-matrix
  pmatrix <- cbind.data.frame(human=rep(0,S),animal=rep(0,S))
  row.names(pmatrix) <- data$historic.species
  human.index <- grep("^H",data$species)
  pmatrix$human[human.index] <- data$branch.abundance[human.index]
  animal.index <- grep("^V",data$species)
  pmatrix$animal[animal.index] <- data$branch.abundance[animal.index]
  
  return(pmatrix)
}


#' ** phylo.zmatrix: Calculates Z matrix **
#' 
#' Arguments: 
#' ----------
#' tree          - phylogenetic tree of file type *.nexus
#' 
#' Sub-arguments (used by 'chainsaw' function):
#' --------------------------------------------
#' leaf.abundance   - [optional] vector of the relative abundance of 
#'                    present-day species; default is a completely even
#'                    distribution, 1 / N, where N is the number of present 
#'                    day species
#' time.interval    - [optional] integer defining a time interval, or the 
#'                    number of years prior to the most recent sample; default 
#'                    is the complete phylogeny, where no cut-off is set
#' year.last.sample   - [optional] integer defining the time of the most recent 
#'                    sample; default is only possible when time.interval==NA, 
#'                    such that no cut-off is set
#'                    
#' Outputs:
#' --------
#' Z             - 

phylo.zmatrix <- function(tree, 
                          leaf.abundance=NA, 
                          time.interval=NA, 
                          year.last.sample=NA) 
{
  # Extract data
  data <- chainsaw(tree, leaf.abundance, time.interval, year.last.sample)
  
  # Present day species
  pds <- unique(data$pds)
  
  if(is.na(leaf.abundance)) leaf.abundance <- rep(1/length(pds),length(pds))
  
  # Calculate Lj for each present day species (total length of evolutionary change)
  Lj <- NULL
  for (i in 1:length(pds)) {
    this.pds <- pds[i]
    index.pds <- which(data$pds==this.pds)
    Lj[i] <- sum(data$length[index.pds])
  }
  
  # Calculate the mean total evolutionary change across present day species
  Tbar <- sum(leaf.abundance*Lj)
  
  # How many historic species?
  historic.species <- as.character(data$historic.species)
  historic.species <- cbind(historic.species, 
                            pds=sapply(historic.species, function(x) 
                              unlist(strsplit(x,","))[1]))
  S <- nrow(historic.species)
  
  # Calculate Z-matrix for all historic species 
  Z <- data.frame(matrix(NA, nrow = S, ncol = S))
  colnames(Z) <- data$historic.species
  row.names(Z) <- data$historic.species
  for (iteration in 1:S) {
    cat("\r", "Calculating Z matrix: row", iteration, "of", S) 
    flush.console()
    # Ancestral species ID
    historic.ib <- as.character(historic.species[iteration])
    # Present day species index
    species.ib <- as.numeric(unlist(strsplit(historic.ib,","))[1])
    # Ancestral species tip index
    tip.ib <- as.numeric(unlist(strsplit(historic.ib,"-"))[2])
    # Present day species descendants of ancestral species tip
    descendant.ib <- unlist(Descendants(tree,tip.ib))
    
    output <- vector()
    Z[iteration,] <- foreach(row.index=1:S, .combine=c) %dopar% {
      # Ancestral species ID
      historic.jc <- as.character(historic.species[row.index])
      # Present day species index
      species.jc <- as.numeric(unlist(strsplit(historic.jc,","))[1])
      # Length of evolutionary history of present day species j
      length.j <- sum(data$length[data$pds==species.jc])
      
      # Similarity between historic species (i,b) and species (j,c)  
      # is non-zero when species j is found within the set of species  
      # descended from branch b
      if (any(descendant.ib %in% species.jc)) {
        output[historic.jc] <- Tbar/length.j
      } else {
        output[historic.jc] <- 0
      }
    }
  }
  Z <- as.matrix(Z)
}


#' ** chainsaw: Cuts the root from the tree ** 
#' Called by phylo.pmatrix and phylo.zmatrix
#' 
#' Arguments: 
#' ----------
#' tree             - phylogenetic tree of file type *.nexus
#' leaf.abundance   - [optional] vector of the relative abundance of 
#'                    present-day species; default is a completely even
#'                    distribution, 1 / N, where N is the number of present 
#'                    day species
#' time.interval    - [optional] integer defining a time interval, or the 
#'                    number of years prior to the most recent sample; default 
#'                    is the complete phylogeny, where no cut-off is set
#' year.last.sample   - [optional] integer defining the time of the most recent 
#'                    sample; default is only possible when time.interval==NA, 
#'                    such that no cut-off is set
#'                    
#' Outputs:
#' --------
#' pmatrix       - 

chainsaw <- function(tree, 
                     leaf.abundance=NA, 
                     time.interval=NA, 
                     year.last.sample=NA) {
  
  # Initialize packages
  require(phangorn)
  #     require(plyr)
  #     require(reshape2)
  
  # All present day species (tips)
  pds <- tree$tip.label
  
  if(is.na(leaf.abundance)) leaf.abundance <- rep(1/length(pds),length(pds))
  
  # How many present day species (tips)
  num.pds <- Ntip(tree)
  root.node <- num.pds + 1
  
  # All edges (ancestor node, descendant node, and branch length)
  edges <- as.data.frame(cbind(tree$edge,tree$edge.length))
  colnames(edges) <- c("ancestor","descendant","length")
  
  # For each present day species name all branches in the evolutionary 
  # history; ordered from root to tip 
  species.ancestors <- lapply(as.list(1:num.pds), function(x) c(x,Ancestors(tree,x)))
  species.ancestors <- lapply(species.ancestors, function(x) 
    as.data.frame(t(x[length(x):1])))
  branch.names <- lapply(species.ancestors, function(x) {
    descendant.nodes <- which(edges$descendant %in% x)
    this.name <- paste(edges$ancestor[descendant.nodes],
                       edges$descendant[descendant.nodes],sep="-")
    this.name <- paste(x[length(x)],this.name,sep=",")
    as.data.frame(t(this.name))
  })
  branch.names <- rbind.fill(branch.names)
  branch.names <- apply(branch.names,2,as.character)
  row.names(branch.names) <- pds
  
  # For each present day species, find the lengths of each ancestral species 
  # in its evolutionary history
  branch.lengths <- lapply(species.ancestors, function(x) {
    descendant.nodes <- which(edges$descendant %in% x)
    as.data.frame(t(edges$length[descendant.nodes])) 
  })
  branch.lengths <- rbind.fill(branch.lengths)
  row.names(branch.lengths) <- pds
  
  # Simulate strict molecular clock on fixed phylogenies and calculate
  # genetic distance
  #   if (strict.clock) {
  #     
  #   }
  
  # For each present day species, calculate the hypothetical "sampling year" 
  # of each of its ancestral species (descendant node)
  tip.years <- do.call(rbind,strsplit(pds,"_"))[,2]
  tip.years <- as.numeric(tip.years) + 0.5
  branch.years <- lapply(species.ancestors, function(x) {
    which.pds <- as.numeric(x[length(x)])
    descendant.nodes <- which(edges$descendant %in% x)
    all.lengths <- edges$length[descendant.nodes]
    pds.year <- tip.years[which.pds]
    sample.years <- sapply(1:length(all.lengths), function(x) 
      pds.year - sum(all.lengths[x:length(all.lengths)]))
    as.data.frame(t(c(sample.years,pds.year)))
  })
  branch.years <- rbind.fill(branch.years)
  row.names(branch.years) <- pds
  
  # Check that the calculated year at which the root (most recent common
  # ancestor) was sampled is equal for all present day species' evolutionary history
  root.year <- unique(branch.years[,1])
  root.check <- sapply(1:length(root.year), 
                       function(x) all.equal(root.year[1],root.year[x]))
  if(!all(root.check==T)) stop("Root years not aligned. Please check tree")
  
  # Extract root
  branch.years <- branch.years[,-1,drop=F] # fix error arrising from 2-branch tree
  root.year <- mean(root.year)
  
  # Name all present day species descendant from each branch
  branch.descendants <- lapply(species.ancestors, function(x) {
    descendant.nodes <- unlist(x[-1])
    as.data.frame(t(Descendants(tree,descendant.nodes)))
  })
  branch.descendants <- rbind.fill(branch.descendants)
  branch.descendants <- apply(branch.descendants,2,function(x) gsub("NULL", NA, x))
  row.names(branch.descendants) <- pds
  
  # Tidy up
  branch.names <- melt(branch.names,na.rm = T)
  colnames(branch.names) <- c("species","generation","historic.species")
  
  branch.lengths <- cbind.data.frame(row.names(branch.lengths),branch.lengths)
  colnames(branch.lengths)[1] <- "species"
  branch.lengths <- melt(branch.lengths,id.vars="species",na.rm = T)
  colnames(branch.lengths) <- c("species","generation","branch.lengths")
  
  branch.years <- cbind.data.frame(row.names(branch.years),branch.years)
  colnames(branch.years)[1] <- "species"
  branch.years <- melt(branch.years,id.vars="species",na.rm = T)
  colnames(branch.years) <- c("species","generation","sampling.year")
  
  branch.descendants <- melt(branch.descendants,na.rm = T)
  colnames(branch.descendants) <- c("species","generation","branch.decendants")
  
  all.data <- cbind.data.frame(branch.names,
                               length=branch.lengths$branch.lengths,
                               sampling.year=branch.years$sampling.year,
                               branch.descendants=branch.descendants$branch.decendants)
  
  tip.species <- do.call(rbind,strsplit(as.character(all.data$historic.species),","))[,1]
  nodes <- do.call(rbind,strsplit(as.character(all.data$historic.species),","))[,2]
  branch.node <- do.call(rbind,strsplit(nodes,"-"))[,1]       
  branch.tip <- do.call(rbind,strsplit(nodes,"-"))[,2]
  
  all.data <- cbind.data.frame(all.data,
                               pds=tip.species,
                               branch.node,branch.tip)
  
  ############################## Cut the tree ################################
  
  # Remove all ancestral species which were "sampled" prior to the cut.here 
  # and shorten any branches which are cut off
  if (!all(is.na(time.interval))) {
    cut.here <- year.last.sample-time.interval
    if (cut.here <= root.year) {
      # If cut.here is earlier than the "natural root" then extend the root
      chop.data <- all.data
      affected.branches <- chop.data$branch.node==root.node
      stretch.root <- chop.data$length[affected.branches] + (root.year - cut.here)
      chop.data$length[affected.branches] <- stretch.root
    } else if (cut.here > max(all.data$sampling.year)) {
      # If cut.here is later than the last sampled tip
      stop ("cut.here is later than the last sampled tip.")
    } else if (cut.here > root.year & 
               cut.here <= max(all.data$sampling.year)) {
      # If cut.here is sensible
      chop.data <- all.data[all.data$sampling.year >= cut.here,]
      affected.branches <- chop.data$sampling.year < (cut.here + chop.data$length)
      fraction.remaining <- (chop.data$sampling.year[affected.branches] - cut.here) / 
        chop.data$length[affected.branches]
      chopped.branches <- chop.data$length[affected.branches] * fraction.remaining
      chop.data$length[affected.branches] <- chopped.branches
    } else stop ("cut.here is not sensible.")
  } else {
    chop.data <- all.data
  }
  row.names(chop.data) <- NULL
  
  # If any branches are now length 0, remove them
  find.length.zero <- which(chop.data$length==0)
  if(is.integer(find.length.zero) && length(find.length.zero) == 0L) {
    chop.data <- chop.data
  } else chop.data <- chop.data[-find.length.zero,]
  
  ####################### Calculate branch abundance #########################
  
  # Present day species
  pds <- unique(chop.data$pds)
  
  # Calculate Lj for each present day species (total length of evolutionary change)
  Lj <- NULL
  for (i in 1:length(pds)) {
    this.leaf <- pds[i]
    leaf.index <- which(chop.data$pds==this.leaf)
    Lj[i] <- sum(chop.data$length[leaf.index])
  }
  
  # Calculate the mean total evolutionary change across present day species
  Tbar <- sum(leaf.abundance*Lj)
  
  # Proportional abundance of ancestral specices; PI(i,b) = (L(b)/Tbar)*pi
  chop.data <- cbind(chop.data,branch.abundance=NA)
  for (j in 1:length(pds)) {
    this.leaf <- pds[j]
    leaf.index <- which(chop.data$pds==this.leaf)
    branch.abundance <- (chop.data$length[leaf.index]/Tbar) * leaf.abundance[j]
    chop.data$branch.abundance[leaf.index] <- branch.abundance
  }
  return(chop.data)
}


plot.phylogeny <- function(show.labels, show.symbols, offset.size){
  if (show.labels==T & show.symbols==T) {
    # Plot sample labels and tips as symbols 
    plot.phylo(ladderize(pruned.tree),
               tip.color=ifelse(pruned.tree$tip.label %in% human.samples, "red","black"),
               label.offset=offset.size)
    axisPhylo()
    co <- c("red", "blue"); sy <- c(22,24) # pch 21:25
    vec <- rep(1,length(keep.these))
    vec[grep("H",keep.these)] <- 2
    tiplabels(pch=sy[vec],             # symbol
              bg=co[vec],              # fill colour
              cex=1.2,                 # magnification
              adj=offset.size)         # horizonal position of text
    axisPhylo()
    
  } else if (show.labels==T & show.symbols==F) {
    # Plot sample labels only
    plot.phylo(ladderize(pruned.tree),
               tip.color=ifelse(pruned.tree$tip.label %in% human.samples, "red","black"),
               label.offset=offset.size/3)
    axisPhylo()
    
  } else if (show.labels==F & show.symbols==T) {
    # Plot tips as symbols only
    plot.phylo(ladderize(pruned.tree),
               tip.color=ifelse(pruned.tree$tip.label %in% human.samples, "red","black"),
               show.tip.label=F)
    axisPhylo()
    co <- c("red", "blue"); sy <- c(22,24) # pch 21:25
    vec <- rep(1,length(keep.these))
    vec[grep("H",keep.these)] <- 2
    tiplabels(pch=sy[vec],             # symbol
              bg=co[vec],              # fill colour
              cex=1.2,                 # magnification
              adj=offset.size)         # horizonal position of text
    axisPhylo()
  }
} 







#
#
#
#
set.axis <- function(tag) {
  this.measure <- unlist(strsplit(tag,'.',fixed=T))
  if(any(grep('bar',this.measure))) {
    y.axis <- this.measure[2]
    y.axis <- bquote(.(this.measure[1])~ italic(bar(.(this.measure[2]))))
  } else 
    y.axis <- bquote(.(this.measure[1])~ italic(.(this.measure[2])))
  return(y.axis)
}

