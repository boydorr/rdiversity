#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat('Object of class distance, containing:\n')
            cat('@distance: Matrix of pairwise distances (', 
                ncol(object@distance), 'types )\n')
            cat('@datID:', object@datID, '\n')
            
            if(!isTRUE(all.equal(0, length(object@taxID))))
            cat('@taxDistance: Vector of ...\n')
            cat('@taxID: Vector of ...\n')
            cat('@taxMask: ListVector of ...\n')
            cat('@taxBits: ListVector of ...\n')
          } )
