#' Class 'collection'
#' 
#' Container for proportional abundance and similarity matrices. 
#' 
#' @field .Data \code{matrix} of mode \code{numeric}; proportional abundance 
#' of \emph{types}
#' @field zmatrix \code{matrix} of mode \code{numeric}; pairwise similarity 
#' between \emph{types}
#' @param object object of class \linkS4class{collection}
#' 
#' @export
#' 
collection <- setClass("collection",
                 contains = "matrix",
                 slots = c(.Data = "matrix",
                           zmatrix = "matrix"))


#' @describeIn collection prints pmatrix
setMethod(f = "show", signature= "collection", 
          definition = function(object) {
            print(head(object@.Data))
          } )

