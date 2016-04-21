#' powermean-class
#' 
#' @description Container for 
#' 
#' @field .Data 
#' @field measure 
#' @field type_abundance
#' @field ordinariness
#' @field type_weights
#' @export
#' 
setClass("powermean", contains = "matrix",
         slots = c(.Data = "matrix", 
                   measure = "character",
                   type_abundance = "matrix",
                   ordinariness = "matrix",
                   type_weights = "matrix"))


#' @rdname powermean
#' @param x any R object 
#' @return \code{is.powermean(x)} returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' @export 
#' 
is.powermean <- function (x) inherits(x, "powermean")


#' @rdname powermean
#' @param object object of class \linkS4class{powermean}
#' @usage show(object) - prints \code{powermean} object
#' 
setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
             print(object@.Data)
            } )
