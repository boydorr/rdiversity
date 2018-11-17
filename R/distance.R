#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat('Object of class `distance`, containing either:\n (1) a distance matrix; or\n (2) all of the data required to calculate a distance matrix.')
          } )
