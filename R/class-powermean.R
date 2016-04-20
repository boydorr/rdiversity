#' Class 'powermean'
#' 
#' Container for 
#' 
#' @name powermean
#' @field .Data 
#' @field measure 
#' @param object object of class \linkS4class{powermean}
#' @export
#' 
setClass("powermean", contains = "matrix",
         slots = c(.Data = "matrix", 
                   measure = "character"))


#' @describeIn powermean Prints power mean
#' @rdname powermean
#' @param x any R object 
#' @return \code{is.powermean(x)} returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' @export 
#' 
is.powermean <- function (x) inherits(x, "powermean")


setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
            cat(object@measure, '\n\n')
            print(object) } )


