#' Class 'initDiv'
#' 
#' Define S4 class \linkS4class{initDiv}.
#' 
#' @slot .Data object of class \code{matrix}; proportional abundance 
#' of \emph{types}
#' @slot zmatrix object of class \code{matrix}; pairwise similarity 
#' between \emph{types}
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

