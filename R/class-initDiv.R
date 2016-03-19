#' Class 'initDiv'
#' 
#' Define S4 class \linkS4class{initDiv}.
#' 
#' @slot measure object of class \code{matrix}; 
#' @param object object of class \linkS4class{initDiv}
#' @export
#' 
initDiv <- setClass("initDiv",
                 contains = "matrix",
                 slots = c(.Data = "matrix",
                           zmatrix = "matrix"))


#' @describeIn initDiv Prints pmatrix
setMethod(f = "show", signature= "initDiv", 
          definition = function(object) {
            print(object@.Data)
          } )

