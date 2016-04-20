#' @name abundance
#'
#' @title Get abundance
#'
#' @description S4 generic function \code{abundance()}.
#'
#' @param pop two-dimensional \code{matrix} of mode \code{numeric}
#' @param ... additional parameters
#' 
#' @return two-dimensional \code{matrix} of mode \code{numeric}; 
#' 
abundance <- function(data) {

  type_abundance = data@.Data 
  
  return(type_abundance)
}