#' @rdname similarity-class
#' @param object object of class \code{similarity}
#'
setMethod(f = "show", signature = "similarity",
          definition = function(object) {
            cat('Object of class `similarity`, containing either:\n (1) a similarity matrix; or\n (2) all of the data required to calculate a similarity matrix.')
          } )
