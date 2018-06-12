#' check_phypartition
#' 
#' \code{check_phypartition()} is used to validate partition matrices for use 
#' with phylogenies.
#' 
#' @param tip_labels vector containing elements of class \code{character}.
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with
#' rows as types, columns as subcommunities, and elements containing relative
#' abundances of types in subcommunities. In the case of phylogenetic
#' metacommunities, these are the relative abundances of terminal taxa.
#' 
#' @return Returns a two-dimensions \code{matrix} of mode \code{numeric}. If 
#' the partition matrix was valid, this should be identical to that which was
#' input as an argument. 
#' 
#' @export
#' 
check_phypartition <- function(tip_labels, partition){
  partition <- check_partition(partition)
  
  # Remove species from partition, if not in phylogeny
  if (any(!row.names(partition) %in% tip_labels)) {
    absent <- row.names(partition)[!row.names(partition) %in% tip_labels]
    if(length(absent)==1)
      warning(paste(absent, "is absent from the phylogeny.")) else
        warning(paste(absent, "are absent from the phylogeny."))
    partition <- partition[-which(row.names(partition) %in% absent),]
    partition <- partition / sum(partition)
  }
  
  # Add species to partition, if in phylogeny
  if (any(!tip_labels %in% row.names(partition))) {
    absent <- tip_labels[!tip_labels %in% row.names(partition)]
    if(length(absent)==1)
      warning(paste(absent, "is absent from the partition.")) else
        warning(paste(absent, "are absent from the partition."))
    missing <- lapply(absent, function(x) matrix(rep(0, ncol(partition)), 
                                                 nrow = 1))
    missing <- do.call(rbind.data.frame, missing)
    row.names(missing) <- absent
    colnames(missing) <- colnames(partition)
    partition <- rbind(partition, missing)
  }
  
  # Reorder partition to match phylogeny
  partition <- partition[tip_labels,]
  partition <- check_partition(partition)
}