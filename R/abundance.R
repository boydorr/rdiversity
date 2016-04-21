#' Get abundances
#'
#' S4 generic function \code{abundance()}.
#'
#' @param data d
#' 
#' @details 
#' 
#' @return two-dimensional \code{matrix} of mode \code{numeric}; 
#' @export
#' 
#' @examples 
#' 
abundance <- function(data) {

  type_abundance = data@.Data 
  
  return(type_abundance)
}