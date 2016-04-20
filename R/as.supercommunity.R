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
#' subcommunity_weights, type_abundance, and ordinariness. 
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
supercommunity <- function(pmatrix, zmatrix = NA) {
  if(any(is.na(zmatrix))) zmatrix <- diag(1, nrow(pmatrix))
  
  if(is.data.frame(pmatrix)) pmatrix <- as.matrix(pmatrix)
  if(is.data.frame(zmatrix)) zmatix <- as.matrix(zmatrix)
  
  pmatrix <- check_pmatrix(pmatrix)
  zmatrix <- check_zmatrix(pmatrix, zmatrix)
  
  type_abundance <- abundance(pmatrix)
  
  weight <- colSums(data) / sum(data)

  Zp.j <- data@similarity %*% data@type_abundance
  
  # Now mark all of the species that have nothing similar as NaNs
  # because diversity of an empty group is undefined
  Zp.j[Zp.j==0] <- NaN

  new('supercommunity', pmatrix, 
      similarity = zmatrix, 
      subcommunity_weight = weight, 
      type_abundance = type_abundance, 
      ordinariness = Zp.j)
}


#' @rdname as.supercommunity
#' @param x any R object 
#' @return 
#' returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' 
is.supercommunity <- function (x) 
  inherits(x, "supercommunity")
