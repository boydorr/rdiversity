#' Phylogenetic Similarity Matrix
#' 
#' Function to calculate phylogenetic similarity matrix from a phylogeny.
#' 
#' @param ps \code{phy_struct()} output.
#' 
#' @return \eqn{hS x hS} matrix; pair-wise similarity of historic species
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' 
#' ps <- phy_struct(tree)
#' smatrix(ps)
#' 
smatrix <- 
  function(ps) 
  {
    # Define s_matrix (type = historic species)
    hs <- ps$parameters$hs_names
    Nhs <- length(hs)
    
    s_matrix <- matrix(0, nrow = Nhs, ncol = Nhs)
    colnames(s_matrix) <- hs
    row.names(s_matrix) <- hs
    
    # Calculate pairwise similarity between historic species
    for (row_index in 1:Nhs) {
      # Historic species 
      ib <- hs[row_index]
      daughters <- phangorn::Descendants(ps$tree, ps$parameters$d_node[row_index])
      daughters <- unlist(daughters)

      s_matrix_row <- vector()
      for (col_index in 1:Nhs) {
        # Historic species (to compare)
        jc <- hs[col_index]
        jc_tip <- ps$parameters$tip_node[col_index]
        # Similarity between historic species (i,b) and species (j,c)  
        # is non-zero when species j is found within the set of species  
        # descended from branch b
        if (jc_tip %in% daughters) {
          s_matrix_row[col_index] <- 1
        } else {
          s_matrix_row[col_index] <- 0
        }
      }
      s_matrix[row_index,] <- s_matrix_row
    }
    s_matrix
  }

