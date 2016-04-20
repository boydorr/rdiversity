#' Class 'relativeentropy'
#' 
#' Container for 
#' 
#' @field .Data 
#' @field measure 
#' @param object object of class \linkS4class{relativeentropy}
#' @export
#' 
setClass("relativeentropy", contains = "matrix",
         slots = c(.Data = "matrix", measure = "character"))


#' @describeIn relativeentropy Prints relative entropy
setMethod(f = "show", signature = "relativeentropy", 
          definition = function(object) {
            cat(object@measure, '\n\n')
            print(object) } )

