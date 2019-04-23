#' Generate distance object
#'
#' Container for class \code{distance}.
#'
#' @param distance distance matrix
#' @param datID object of class \code{character} denoting the type of diversity
#' being calculated. This can be "naive", "genetic", "taxonomic", and so on
#'
#' @return \code{distance()} returns an object of class \code{distance}.
#' @name distance
#' @rdname distance-methods
#' @exportMethod distance
#'
setGeneric(name = "distance",
           def = function(distance, datID) {
             standardGeneric("distance")
           } )


#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", datID = "character"),
          definition = function(distance, datID) {
            new('distance',
                distance = distance,
                datID = datID)
          } )


#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", datID = "missing"),
          definition = function(distance, datID) {
            new('distance',
                distance = distance,
                datID = "UserGenerated")
          } )


#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat('Object of class `distance`, containing either:\n (1) a distance matrix; or\n (2) all of the data required to calculate a distance matrix.')
          } )

