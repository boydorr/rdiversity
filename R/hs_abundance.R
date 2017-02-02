#' hs_abundance
#'
#' Helper function that calculates the relative abundance of historic species
#' for \code{metacommunity()} and \code{chainsaw()}.
#' 
#' @param partition \code{vector} or \code{matrix} containing the
#' relative abundances of individuals or types in their subcommunities. In the
#' case of phylogenetic metacommunities, these are the relative abundances of
#' the tips of the tree (the present day species).
#' @param historic.species f
#' @param terminal.taxa f
#' @param Tbar f
#' @param tag f
#'
hs_abundance <- function(partition, 
                         historic.species, 
                         terminal.taxa, 
                         Tbar, 
                         tag) {
  # Relative abundance of historic species
  if(ncol(partition)==1) {
    hs.abundance <- sapply(seq_along(historic.species$hs.name), function(x) {
      row.index <- match(historic.species$tip.node[x], terminal.taxa$tip.node)
      (historic.species$length[x] / Tbar) * terminal.taxa$pds.abundance[row.index]
    })
    hs.abundance <- cbind.data.frame(hs.name = historic.species$hs.name,
                                     hs.abundance = hs.abundance,
                                     subcommunity = 1)

    historic.species <- merge(historic.species, hs.abundance)
  }else {
    tmp <- lapply(unique(terminal.taxa$subcommunity), function(x) {
      this.sc <- terminal.taxa[terminal.taxa$subcommunity %in% x,]
      sc <- unique(terminal.taxa$subcommunity)[x]
      hs.abundance <- sapply(seq_along(historic.species$hs.name), function(y) {
        row.index <- match(historic.species$tip.node[y], this.sc$tip.node)
        (historic.species$length[y] / Tbar) * this.sc$pds.abundance[row.index]
      })
      hs.abundance <- cbind.data.frame(hs.name = historic.species$hs.name,
                                       hs.abundance = hs.abundance,
                                       subcommunity = sc)
    })
    tmp <- do.call(rbind, tmp)
    historic.species <- merge(historic.species, tmp)
  }

  # Reinstate partitions
  type_abundance <- matrix(0, nrow = length(tag), ncol = ncol(partition))
  row.names(type_abundance) <- tag
  colnames(type_abundance) <- colnames(partition)

  index <- lapply(seq_along(colnames(type_abundance)), function(x) {
    sc <- unique(historic.species$subcommunity)[x]
    this.sc <- historic.species[historic.species$subcommunity %in% sc,]
    cbind(which(this.sc$hs.name %in% tag), x, this.sc$hs.abundance)
  })
  index <- do.call(rbind, index)
  abundance <- index[,3]
  index <- index[,1:2]
  type_abundance[index] <- abundance
  type_abundance
}
