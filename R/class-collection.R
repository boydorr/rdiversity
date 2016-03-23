#' Class to contain proportional abundance and similarity matrices
#' 
#' Container for proportional abundance and similarity matrices. 
#' 
#' @slot .Data object of class \code{matrix}; proportional abundance 
#' of \emph{types}
#' @slot zmatrix object of class \code{matrix}; pairwise similarity 
#' between \emph{types}
#' @param object object of class \linkS4class{collection}
#' 
#' @export
#' 
collection <- setClass("collection",
                 contains = "matrix",
                 slots = c(.Data = "matrix",
                           zmatrix = "matrix"))


#' @describeIn collection Prints pmatrix
setMethod(f = "show", signature= "collection", 
          definition = function(object) {
            print(object@.Data)
          } )

