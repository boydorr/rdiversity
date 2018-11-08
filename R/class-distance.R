#' distance-class
#' 
#' Container for class \code{distance}.
#' 
#' @name distance-class
#' @rdname distance-class
#' @exportClass distance
#' 
#' @field distance object of class \code{matrix}.
#' @field divID object of class \code{character}.
#' @field values object of class \code{numeric}.
#' @field taxID object of class \code{numeric}.
#' @field taxMask object of class \code{list}.
#' 
setClass("distance", slots = c(distance = "matrix",
                               divID = "character",
                               values = "numeric",
                               taxID = "numeric",
                               taxMask = "list"))


