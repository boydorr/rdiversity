#' @name abundance
#'
#' @title Get abundance
#'
#' @description S4 generic function \code{abundance()}.
#'
#' @param data 
#' 
#' @return two-dimensional \code{matrix} of mode \code{numeric}; 
#' 
abundance <- function(data) {

  type_abundance = data@.Data 
  
  return(type_abundance)
}