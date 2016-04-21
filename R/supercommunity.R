#' Coerse to Supercommunity
#' 
#' Functions to check if an object is a \code{supercommunity} or coerce an  
#' object into a \code{supercommunity}.
#' 
#' @param partition \code{matrix} (usually two-dimensional) of mode numeric; relative 
#' abundance of types
#' @param similarity (optional) two-dimensional \code{matrix} of mode numeric; 
#' pair-wise similarity of types. Default sets similarity to the naive-type 
#' case, where types are completely distinct. 
#' @return returns an object of class \code{supercommunity}; 
#' an S4 object containing five slots, .Data (partition), similarity, 
#' type_abundance, ordinariness, and subcommunity_weights. 
#' 
#' @include class-supercommunity.R check_pmatrix.R check_zmatrix.R
#' @export
#' 
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' 
supercommunity <- function(partition, similarity = NA) {
  if(any(is.na(similarity))) {
    similarity <- diag(1, nrow(partition))
    row.names(similarity) <- row.names(partition)
    colnames(similarity) <- row.names(partition)
  }
  
  if(is.data.frame(partition)) partition <- as.matrix(partition)
  if(is.data.frame(similarity)) similarity <- as.matrix(similarity)
  
  partition <- check_partition(partition)
  similarity <- check_similarity(partition, similarity)
  
  type_abundance <- abundance(partition)
  
  subcommunity_weights <- colSums(type_abundance) / sum(type_abundance)
  
  type_weights <- sapply(1:ncol(type_abundance), function(x)
    (type_abundance[,x]/colSums(type_abundance)[x]))
  
  Zp.j <- similarity %*% type_abundance
  
  # Now mark all of the species that have nothing similar as NaNs
  # because diversity of an empty group is undefined
  Zp.j[Zp.j==0] <- NaN
  
  new('supercommunity', partition, 
      similarity = similarity, 
      type_abundance = type_abundance, 
      ordinariness = Zp.j,
      subcommunity_weights = subcommunity_weights,
      type_weights = type_weights)
}


#' @rdname supercommunity
#' @param x any R object 
#' @return 
#' returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' 
is.supercommunity <- function (x) 
  inherits(x, "supercommunity")
