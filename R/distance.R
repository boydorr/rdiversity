#' Generate distance object
#'
#' Container for class \code{distance}.
#'
#' @param distance distance matrix
#' @param dat_id object of class \code{character} denoting the type of diversity
#' being calculated. This can be "naive", "genetic", "taxonomic", and so on
#'
#' @return \code{distance()} returns an object of class \code{distance}.
#' @name distance
#' @rdname distance-methods
#' @exportMethod distance
#'
setGeneric(name = "distance",
           def = function(distance, dat_id) {
             standardGeneric("distance")
           } )


#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", dat_id = "character"),
          definition = function(distance, dat_id) {
            new("distance",
                distance = distance,
                dat_id = dat_id)
          } )


#' @rdname distance-methods
#' @aliases distance
#'
setMethod(f = "distance",
          signature(distance = "matrix", dat_id = "missing"),
          definition = function(distance, dat_id) {
            new("distance",
                distance = distance,
                dat_id = "UserGenerated")
          } )


#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat("Object of class `distance`, containing either:\n (1) a distance matrix; or\n (2) all of the data required to calculate a distance matrix.")
          } )
