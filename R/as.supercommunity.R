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
  if(any(is.na(zmatrix))) zmatrix <- diag(1, nrow(pmatrix))
  
  new('supercommunity', pmatrix, zmatrix = zmatrix)
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

