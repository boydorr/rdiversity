#' Repartition metacommunity
#' 
#' Randomly reshuffles the relative abundance of types (e.g. species) in a 
#' metacommunity (whilst maintaining the relationship between the relative
#' abundance of a particular species across subcommunities). In the case of a
#' phylogenetic metacommunity, the relative abundance of terminal taxa are 
#' randomly reshuffled and the relative abundance of types (historic species) 
#' are calculated from the resulting partition. 
#' 
#' @param meta object of class \code{metacommunity}.
#' @param ps \code{phy_struct()} output.
#' 
#' @return Returns an object of class \code{metacommunity.}
#' @export
#' 
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' meta <- metacommunity(partition, tree)
#' ps <- phy_struct(tree)
#' 
#' repartition(meta, ps)
#' 
repartition <- function(meta, ps) {
  
  if(isTRUE(all.equal(0, length(meta@raw_structure)))) {
    # Non-phylogenetic metacommunity
    partition <- meta@type_abundance
    new_partition <- partition[sample(seq_along(row.names(partition))),]
    new_partition <- check_partition(new_partition)
    row.names(new_partition) <- row.names(partition)

    new_meta <- metacommunity(new_partition, meta@similarity)

  }else {
    # Phylogenetic metacommunity
    raw_abundance <- meta@raw_abundance
    new_abundance <- raw_abundance[sample(seq_along(row.names(raw_abundance))),]
    new_abundance <- check_partition(new_abundance)
    row.names(new_abundance) <- row.names(raw_abundance)

    hs_abundance <- phy_abundance(new_abundance, ps)
    s <- smatrix(ps)
    z <- zmatrix(new_abundance, s, ps)

    new_meta <- metacommunity(hs_abundance, z)
    new_meta@raw_abundance <- new_abundance
    new_meta@raw_structure <- meta@raw_structure
    new_meta@parameters <- meta@parameters
  }

  new_meta
}