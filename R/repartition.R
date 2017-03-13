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
repartition <- function(meta, tree) {
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
  
  if(isTRUE(all.equal(0, length(meta@structure)))) {
    # Non-phylogenetic metacommunity
    partition <- meta@type_abundance
    new_partition <- partition[sample(seq_along(row.names(partition))),]
    new_partition <- check_partition(new_partition)
    row.names(new_partition) <- row.names(partition)
    
    new_meta <- metacommunity(new_partition, meta@similarity)
    
  }else {
    # Phylogenetic metacommunity
    tip_abundance <- meta@tip_abundance
    new_abundance <- tip_abundance[sample(seq_along(row.names(tip_abundance))),]
    new_abundance <- check_partition(new_abundance)
    row.names(new_abundance) <- row.names(tip_abundance)
    
    ps <- new('phy_struct', 
              structure = meta@structure, 
              parameters = meta@hs_parameters)
    
    hs_abundance <- phy_abundance(new_abundance, ps)
    smatrix <- s_matrix(tree, ps)
    zmatrix <- z_matrix(new_abundance, smatrix, ps)
    
    new_meta <- metacommunity(hs_abundance, zmatrix)
    new_meta@tip_abundance <- new_abundance
    new_meta@structure <- meta@structure
    new_meta@hs_parameters <- meta@hs_parameters
  }
  
  new_meta
}