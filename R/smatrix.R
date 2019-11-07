#' Phylogenetic similarity matrix (ultrametric)
#'
#' Function to calculate an ultrametric-similarity matrix.
#'
#' @param ps \code{phy_struct()} output.
#'
#' @return Returns an \eqn{hS x hS} matrix; pair-wise ultrametric-similarity of
#' historic species.
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
      daughters <- descendant_tips(ps$tree, parameters$d_node[row_index])

      s_matrix_row <- vector()
      for (col_index in 1:Nhs) {
        # Historic species (to compare)
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
      s_matrix[row_index, ] <- s_matrix_row
    }
    s_matrix
  }


#' descendant_tips
#'
#' @param tree object of class \code{phylo}.
#' @param node object of class \code{numeric}.
#'
descendant_tips <- function(tree, node) {
  x <- node
  tips <- seq_len(length(tree$tip.label))

  if(node %in% tips) return(node) else {
    keep <- vector()
    continue <- TRUE

    while(continue) {
      check <- tree$edge[which(tree$edge[, 1] %in% x), 2]
      check
      daughters <- tips[tips %in% check]
      daughters
      keep <- c(keep, daughters)
      keep
      x <- check[!check %in% tips]
      x
      continue <- ifelse(length(x) != 0, T, F)
    }
    return(sort(keep))
  }
}

