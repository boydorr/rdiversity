#' Phylogenetic similarity matrix (ultrametric)
#'
#' Function to calculate an ultrametric-similarity matrix.
#'
#' @param ps \code{phy_struct()} output.
#'
#' @return Returns an \eqn{hS x hS} matrix; pair-wise ultrametric-similarity of
#' historic species.
#'
#' @examples
#' tree <- ape::rtree(10)
#' partition <- matrix(rep(1, 500), nrow = 10)
#' partition <- as.data.frame(partition / sum(partition))
#' rownames(partition) <- tree$tip.label
#' colnames(partition) <- LETTERS[seq_len(ncol(partition))]
#' ps <- phy_struct(tree, partition)
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
      daughters <- phangorn::Descendants(ps$tree, parameters$d_node[row_index])
      daughters <- unlist(daughters)

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


# descendant_tips <- function(tree, node) {
#   x <- node
#
#   store <- vector()
#   continue <- TRUE
#   while(continue) {
#     y <- tree$edge[which(tree$edge[, 1] == x), 2]
#     sapply(y, function(i) tree$edge[which(tree$edge[, 1] == i), 2])
#
#     store <- c(store, x)
#     continue <- any(tree$edge[,2] == x)
#   }
#   store
# }

