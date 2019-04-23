#' Phylogenetic similarity
#'
#' Packages phylo object and depth into an object of class \code{similarity}.
#'
#' @param tree object of class \code{phylo}.
#' @param depth proportion of total tree height to be conserved (taken as
#' a proportion from the heighest tip). Describes how much evolutionary history
#' should be retained, with 0 marking the date of the most recent tip, and 1
#' (the default) marking the most recent common ancestor. Numbers greater than
#' 1 extend the root of the tree.
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (terminal taxa), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole.
#'
#' @return \code{phy2branch()} returns an object of class \code{distance}.
#' @export
#'
phy2branch <- function(tree, partition, depth = 1) {

  # tidy_tree <- tidytree::as_tibble(tree)
  # tidy_tree <- as.data.frame(tidy_tree)

  new("similarity",
      datID = "phybranch",
      components = list(tree = tree,
                        partition = partition,
                        treeDepth = depth),
      parameters = list(transform = NA,
                        k = NA,
                        normalise = NA,
                        max_d = NA))

}
