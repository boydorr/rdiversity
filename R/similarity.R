#' @rdname similarity-class
#' @param object object of class \code{similarity}
#'
setMethod(f = "show", signature = "similarity",
          definition = function(object) {
            cat('Object of class similarity, containing:\n')
            cat('@similarity: Matrix of pairwise similarities (', 
                ncol(object@similarity), 'types\n')
            cat('@datID:', object@datID, '\n')
            
            if(!isTRUE(all.equal(0, length(object@taxID))))
              cat('@taxSimilarity: Vector of ...\n')
            cat('@taxID: Vector of ...\n')
            cat('@taxMask: Vector of ...\n')
            cat('@taxBits: ListVector of ...\n')
            cat('@parameters: ListVector of ...\n')
          } )
