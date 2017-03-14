#' Convert to phy_struct
#'
#' Converts an object into class \code{phylo} into class \code{phy_struct}.
#' 
#' @param tree object of class \code{phylo}
#' 
#' @return Returns a \code{list} containing:
#'
#' \tabular{ll}{
#' \code{$structure} \tab - each row denotes historic species, columns denote 
#' terminal taxa, and elements contain branch lengths \cr
#' \code{$parameters} \tab - information associated with each historic species \cr
#' \code{$tree} \tab - object of class \code{phylo} \cr
#' }
#'
#' Additional slots (accessed with @, not $) include:
#' 
#' \tabular{ll}{
#' \code{historic.species} \tab - a data_frame containing components associated 
#' with historic species \cr
#' \code{terminal_taxa} \tab - na data_frame containing components associated 
#' with terminal taxa \cr
#' \code{Tbar} \tab - \cr
#' \code{structure} \tab -  \cr
#' }
#'
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' 
#' res <- phy_struct(tree)
#' 
phy_struct <- function(tree) {
  # Extract parameters associated with each historic species
  parameters <- hs_parameters(tree)
  
  # Create structural matrix
  s_matrix <- matrix(0, ncol = length(seq_along(tree$tip.label)), 
                     nrow = nrow(parameters))
  row.names(s_matrix) <- parameters$hs_names
  colnames(s_matrix) <- tree$tip.label
  index <- sapply(parameters$tip_label, function(x) which(colnames(s_matrix) %in% x))
  index <- cbind(row = 1:nrow(parameters), col = index)
  s_matrix[index] <- parameters$lengths
  
  # Output
  list(structure = s_matrix, 
      parameters = parameters,
      tree = tree)
}

