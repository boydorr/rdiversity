#' Repartition metacommunity
#'
#' Randomly reshuffles the relative abundance of types (\emph{e.g}. species) in 
#' a metacommunity (whilst maintaining the relationship between the relative
#' abundance of a particular species across subcommunities). In the case of a
#' phylogenetic metacommunity, the relative abundance of terminal taxa are
#' randomly reshuffled and the relative abundance of types (historical species)
#' are calculated from the resulting partition.
#'
#' @param meta object of class \code{metacommunity}.
#' @param new_partition proportional abundance of \emph{types} in the
#' subcommunity as a fraction of the metacommunity as a whole (in the
#' phylogenetic case, this corresponds to the proportional abundance of
#' terminal taxa). If this argument is missing, all species/tips will be
#' shuffled.
#'
#' @return Returns an object of class \code{metacommunity}.
#' @export
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = sample(5,5), b = sample(5,5))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' meta <- metacommunity(partition, tree)
#' meta@raw_abundance
#'
#' a <- repartition(meta)
#' a@raw_abundance
#'
#' # Non-phylogenetic example
#' meta <- metacommunity(partition)
#' meta@type_abundance
#' a <- repartition(meta)
#' a@type_abundance
#'
repartition <- function(meta, new_partition) {

  if(isTRUE(all.equal(0, length(meta@raw_structure)))) {
    # Non-phylogenetic metacommunity
    partition <- meta@type_abundance
    if(missing(new_partition))
      new_partition <- partition[sample(seq_along(row.names(partition))),,
                                 drop = FALSE]
    new_partition <- check_partition(new_partition)
    # Check
    if(any(dim(partition) != dim(new_partition)))
      stop('Dimensionality has changed during repartition()ing')

    row.names(new_partition) <- row.names(partition)

    new_meta <- metacommunity(new_partition, meta@similarity)

  }else {
    # Phylogenetic metacommunity
    raw_abundance <- meta@raw_abundance
    if(missing(new_partition))
      new_partition <- raw_abundance[sample(seq_along(row.names(raw_abundance))),
                                     , drop = FALSE]
    new_partition <- check_partition(new_partition)
    # Check
    if(any(dim(raw_abundance) != dim(new_partition)))
      stop('Dimensionality has changed during repartition()ing')

    row.names(new_partition) <- row.names(raw_abundance)

    hs_abundance <- phy_abundance(new_partition, meta@raw_structure)

    new_meta <- metacommunity(hs_abundance/sum(hs_abundance),
                              meta@similarity*sum(hs_abundance))
    new_meta@raw_abundance <- new_partition
    new_meta@raw_structure <- meta@raw_structure/sum(hs_abundance)
    new_meta@parameters <- meta@parameters
  }

  new_meta
}
