#' Phylogenetic population matrix
#' 
#' Proportional abundance of ancestral specices; PI(i,b) = (L(b)/Tbar)*pi
#' 
#' @param tree object of class \code{phylo}
#' @param subcommunities \eqn{hS x 2} matrix; colnames = c('edge', 'subcommunity)
#' @return \eqn{hS x hS} matrix; proportional abundance of historic species
#' 
phylo.pmatrix <- function(tree, subcommunities) {
  
  new.tree <- phylo.dat(tree)
  
  # Calculate proportional abundance of hs
  hs.abundance <- sapply(1:Ntype, function(x) {
    tree$edge
    tree$edge.length
    
    # Define pmatrix (type = historic species)
    zmatrix <- matrix(NA, nrow = length(subcommunities), ncol = Ntype)
    colnames(zmatrix) <- subcommunities; row.names(zmatrix) <- hs
    
    
    # Calculate p-matrix
    pmatrix <- cbind.data.frame(human=rep(0,S),animal=rep(0,S))
    row.names(pmatrix) <- data$historic.species
    human.index <- grep("^H",data$species)
    pmatrix$human[human.index] <- data$branch.abundance[human.index]
    animal.index <- grep("^V",data$species)
    pmatrix$animal[animal.index] <- data$branch.abundance[animal.index]
  })
  
}


