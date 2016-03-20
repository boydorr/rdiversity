#' Phylogenetic population matrix
#'
#' Proportional abundance of ancestral specices; PI(i,b) = (L(b)/Tbar)*pi
#'
#' @param tree object of class \code{phylo}
#' @param lookup \eqn{hS x 2} matrix; colnames = c('edge', 'subcommunity)
#' @return \eqn{hS x hS} matrix; proportional abundance of historic species
#'
phylo.pmatrix <- function(tree, lookup) {

  new.tree <- phylo.dat(tree)

  N.hs <- length(new.tree@hs.name)
  subcommunities <- unique(lookup$subcommunity)
  N.subcommunities <- length(subcommunities)
    
    # Define pmatrix (type = historic species)
    pmatrix <- matrix(NA, nrow = N.hs, ncol = N.subcommunities)
    colnames(pmatrix) <- subcommunities
    row.names(pmatrix) <- new.tree$hs.name


    # Calculate p-matrix
    pmatrix <- cbind.data.frame(human=rep(0,N.hs),animal=rep(0,N.hs))
    row.names(pmatrix) <- data$historic.species
    human.index <- grep("^H",data$species)
    pmatrix$human[human.index] <- data$branch.abundance[human.index]
    animal.index <- grep("^V",data$species)
    pmatrix$animal[animal.index] <- data$branch.abundance[animal.index]

}


