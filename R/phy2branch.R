#' Phylogenetic similarity
#'
#' Packages all inputs into an object of class \code{similarity}.
#'
#' @param tree object of class \code{phylo}.
#' @param depth proportion of total tree height to be conserved (taken as
#' a proportion from the highest tip). Describes how much evolutionary history
#' should be retained, with 0 marking the date of the most recent tip, and 1
#' (the default) marking the most recent common ancestor. Numbers greater than
#' 1 extend the root of the tree.
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (terminal taxa), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole.
#'
#' @return \code{phy2branch()} returns an object of class \code{similarity}.
#' @export
#'
phy2branch <- function(tree, partition, depth = 1) {
  new("similarity",
      dat_id = "phybranch",
      components = list(tree = tree,
                        partition = partition,
                        tree_depth = depth),
      parameters = list(transform = NA,
                        k = NA,
                        normalise = NA,
                        max_d = NA))
}
