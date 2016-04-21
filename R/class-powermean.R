#' powermean-class
#' 
#' Container for 
#' 
#' @field .Data 
#' @field measure 
#' @field type_abundance
#' @field ordinariness
#' @field type_weights
#' 
#' 
#' @export
#' 
setClass("powermean", contains = "matrix",
         slots = c(.Data = "matrix", 
                   measure = "character",
                   type_abundance = "matrix",
                   ordinariness = "matrix",
                   subcommunity_weights = "vector",
                   type_weights = "matrix"))


#' @describeIn supercommunity prints powermean
#' @param object object of class \linkS4class{powermean}
#' 
setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
             print(object@.Data)
            } )

