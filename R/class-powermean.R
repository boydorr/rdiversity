#' Class 'powermean'
#' 
#' Container for 
#' 
#' @field .Data 
#' @field measure 
#' @param object object of class \linkS4class{powermean}
#' @export
#' 
setClass("powermean", contains = "matrix",
         slots = c(.Data = "matrix", 
                   measure = "character"))


#' @describeIn powermean Prints power mean
setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
            cat(object@measure, '\n\n')
            print(object) } )


