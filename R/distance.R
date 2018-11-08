#' Generate distance object
#' 
#' distance
#' 
#' @param distance lksdjf
#' @param divID lskdj
#' @param values lskdj
#' @param taxID lskdj
#' @param taxMask lskdj
#' 
#' @return distance() returns an object of class \code{distance}
#' 
distance <- function(distance, divID, values, taxID, taxMask) {
  new('distance', 
      distance = distance, 
      divID = divID,
      values = values,
      taxID = taxID, 
      taxMask = taxMask)
}



#' @rdname distance-class
#' @param object object of class \code{distance}
#'
setMethod(f = "show", signature = "distance",
          definition = function(object) {
            cat('Object of class distance, containing:\n')
            cat('@distance: Matrix of pairwise distances (', 
                ncol(object@distance), 'types\n')
            cat('@divID: Identifier describing the type of data being analysed\n')
            
            if(!isTRUE(all.equal(0, length(object@taxID))))
            cat('@values: Vector of ...\n')
            cat('@taxID: Vector of ...\n')
            cat('@taxMask: ListVector of ...\n')
            
          } )
