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
#' @param new_partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of terminal taxa. If this argument
#' is missing, all species / tips will be shuffled
#'
#' @return \code{repartition()} returns an object of class \code{metacommunity}
#' @export
#'
repartition <- function(meta, new_partition) {

  # Non-phylogenetic metacommunity
  if (isTRUE(all.equal(0, length(meta@raw_structure)))) {

    partition <- meta@type_abundance
    if (missing(new_partition))
      new_partition <- partition[sample(seq_along(row.names(partition))), ,
                                 drop = FALSE]
    new_partition <- check_partition(new_partition)
    # Check
    if (any(dim(partition) != dim(new_partition)))
      stop("Dimensionality has changed during repartition()ing")

    row.names(new_partition) <- row.names(partition)

    new_meta <- metacommunity(new_partition, meta@similarity)



    # Phylogenetic metacommunity
  }else {

    raw_abundance <- meta@raw_abundance
    if (missing(new_partition))
      new_partition <- raw_abundance[sample(seq_along(row.names(raw_abundance))),
                                     , drop = FALSE]
    new_partition <- check_partition(new_partition)
    # Check
    if (any(dim(raw_abundance) != dim(new_partition)))
      stop("Dimensionality has changed during repartition()ing")

    row.names(new_partition) <- row.names(raw_abundance)

    hs_abundance <- phy_abundance(new_partition, meta@raw_structure)

    new_similarity <- new("similarity",
                          similarity = meta@similarity * sum(hs_abundance),
                          dat_id = meta@dat_id)

    new_meta <- metacommunity(partition = hs_abundance / sum(hs_abundance),
                              similarity = new_similarity)
    new_meta@raw_abundance <- new_partition
    new_meta@raw_structure <- meta@raw_structure / sum(hs_abundance)
    new_meta@parameters <- meta@parameters
  }

  new_meta
}
