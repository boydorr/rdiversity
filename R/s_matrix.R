#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param tree object of class \code{phylo}
#' @param ps object of class \code{phy_struct}
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' 
s_matrix <- 
  function(tree, ps) 
  {
    # Define s_matrix (type = historic species)
    hs <- ps@parameters$hs_names
    Nhs <- length(hs)
    
    s_matrix <- matrix(0, nrow = Nhs, ncol = Nhs)
    colnames(s_matrix) <- hs
    row.names(s_matrix) <- hs
    
    # Calculate pairwise similarity between historic species
    for (row_index in 1:Nhs) {
      # Historic species 
      ib <- hs[row_index]
      daughters <- phangorn::Descendants(tree, ps@parameters$d_node[row_index])
      daughters <- unlist(daughters)
      # daughters <- unlist(historic_species$Ntips.descendants[row_index])
      
      s_matrix_row <- vector()
      for (col_index in 1:Nhs) {
        # Historic species (to compare)
        jc <- hs[col_index]
        jc_tip <- ps@parameters$tip_node[col_index]
        # jc_Ntips <- historic_species$tip.node[col_index]
        # Similarity between historic species (i,b) and species (j,c)  
        # is non-zero when species j is found within the set of species  
        # descended from branch b
        if (jc_tip %in% daughters) {
          # zmatrix.row[jc] <- new.tree@parameters$Tbar[1] / j.length
          s_matrix_row[col_index] <- 1
        } else {
          s_matrix_row[col_index] <- 0
        }
      }
      s_matrix[row_index,] <- s_matrix_row
    }
    s_matrix
  }

