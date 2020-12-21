#' Phylogenetic similarity matrix (ultrametric)
#'
#' Function to calculate an ultrametric-similarity matrix.
#'
#' @param ps \code{phy_struct()} output.
#'
#' @return Returns an \eqn{hS x hS} matrix; pair-wise ultrametric-similarity of
#' historic species.
#'
#' @noRd
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
