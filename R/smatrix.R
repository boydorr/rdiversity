#' Phylogenetic similarity matrix (ultrametric)
#' 
#' Function to calculate an ultrametric-similarity matrix.
#' 
#' @param ps \code{phy_struct()} output.
#' 
#' @return Returns an \eqn{hS x hS} matrix; pair-wise similarity of historic 
#' species.
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' 
#' ps <- phy_struct(tree, partition)
#' smatrix(ps)
#' 
smatrix <- 
  function(ps) {
    parameters <- ps$parameters
    hs <- parameters$hs_names

    # Define s_matrix (type = historic species)
    Nhs <- length(hs)

    s_matrix <- matrix(0, nrow = Nhs, ncol = Nhs)
    colnames(s_matrix) <- hs
    row.names(s_matrix) <- hs
    
    # Calculate pairwise similarity between historic species
    for (row_index in 1:Nhs) {
      # Historic species 
      ib <- hs[row_index]
      daughters <- phangorn::Descendants(ps$tree, parameters$d_node[row_index])
      daughters <- unlist(daughters)

      s_matrix_row <- vector()
      for (col_index in 1:Nhs) {
        # Historic species (to compare)
        jc <- hs[col_index]
        jc_tip <- parameters$tip_node[col_index]
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

