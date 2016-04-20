#' Coerse to a Supercommunity
#' 
#' Functions to check if an object is a \code{supercommunity} or coerce an object 
#' into a \code{supercommunity}.
#' 
#' @param pmatrix two-dimensional \code{matrix} of mode numeric; relative 
#' abundance of types
#' @param zmatrix (optional) two-dimensional \code{matrix} of mode numeric; pair-wise 
#' similarity of types
#' @return returns an object of class \code{supercommunity}; 
#' an S4 object containing two slots, pmatrix and zmatrix. 
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
  
  # if (is.vector(object)) object <- array(object, dim=c(length(object), 1))
  # if (is.data.frame(object)) object <- as.matrix(object)
  
  pmatrix <- check_pmatrix(pmatrix)
  zmatrix <- check_zmatrix(pmatrix, zmatrix)
  
  type_abundance <- abundance(pmatrix)
  
  weight <- colSums(data) / sum(data)

  Zp.j <- data@similarity %*% data@type_abundance
}


#' @rdname as.supercommunity
#' @param x any R object 
#' @return 
#' returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' 
is.supercommunity <-
  function (x) 
  {
    inherits(x, "supercommunity")
  }

