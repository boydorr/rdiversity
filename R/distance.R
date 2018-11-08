#' Generate distance object
#' 
#' distance
#' 
#' @param depth object of class \code{numeric}
#' 
#' @return distance() returns an object of class \code{distance}
#' 
distance <- function(distance, what) {
  new('distance', 
      distance = distance, 
      what = what)
}

