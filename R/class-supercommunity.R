#' Class 'supercommunity'
#' 
#' Container for proportional abundance and similarity matrices. 
#' 
#' @field .Data \code{matrix} of mode \code{numeric}; proportional abundance 
#' of \emph{types}
#' @field similarity \code{matrix} of mode \code{numeric}; pairwise similarity 
#' between \emph{types}
#' @field type_abundance
#' @field ordinariness
#' @field subcommunity_weights
#' @field type_weights
#' 
#' @export
#' 
setClass("supercommunity", contains = "matrix",
         slots = c(.Data = "matrix", 
                   similarity = "matrix",
                   type_abundance = "matrix",
                   ordinariness = "matrix",
                   subcommunity_weights = "vector", 
                   type_weights = "matrix"))


#' @describeIn supercommunity prints pmatrix
#' @param object object of class \linkS4class{supercommunity}
#' 
setMethod(f = "show", signature= "supercommunity", 
          definition = function(object) {
            print(head(object@.Data)) } )
