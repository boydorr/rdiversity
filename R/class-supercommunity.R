#' Class 'supercommunity'
#' 
#' Container for proportional abundance and similarity matrices. 
#' 
#' @field .Data \code{matrix} of mode \code{numeric}; proportional abundance 
#' of \emph{types}
#' @field zmatrix \code{matrix} of mode \code{numeric}; pairwise similarity 
#' between \emph{types}
#' @param object object of class \linkS4class{supercommunity}
#' 
#' @export
#' 
setClass("supercommunity", contains = "matrix",
         slots = c(.Data = "matrix", 
                   similarity = "matrix",
                   subcommunity_weight = "vector",
                   type_abundance = "matrix",
                   ordinariness = "matrix"))


#' @describeIn supercommunity prints pmatrix
setMethod(f = "show", signature= "supercommunity", 
          definition = function(object) {
            print(head(object@.Data))
          } )

