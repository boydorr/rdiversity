#' distance-class
#' 
#' Container for class \code{distance}.
#' 
#' @name distance-class
#' @rdname distance-class
#' @exportClass distance
#' 
#' @field distance object of class \code{matrix}.
#' @field datID object of class \code{character}.
#' @field components object of class \code{list}.
#' 
setClass("distance", slots = c(distance = "matrix",
                               datID = "character",
                               components = "list"))


