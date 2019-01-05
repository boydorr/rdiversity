#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat('Object of class `distance`, containing either:\n (1) a distance matrix; or\n (2) all of the data required to calculate a distance matrix.')
          } )


#' Generate distance object
#'
#' @param distance distance matrix
#' @param datID 
#' @param components sdf
#'
#' @return \code{distance()} returns an object of class \code{distance}.
#'
setGeneric(name = "distance",
           def = function(distance, datID, components) {
             standardGeneric("distance")
           } )


#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", datID = "character", components = "list"),
          definition = function(distance, datID, components) {
            new('distance',
                distance = distance,
                datID = datID,
                components = components)
          } )
          

#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", datID = "character", components = "missing"),
          definition = function(distance, datID, components) {
            new('distance',
                distance = distance,
                datID = datID)
          } )