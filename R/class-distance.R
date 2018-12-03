#' distance-class
#' 
#' Container for class \code{distance}.
#' 
#' @name distance-class
#' @rdname distance-class
#' @exportClass distance
#' 
#' @field similarity object of class \code{matrix}.
#' @field datID object of class \code{character}.
#' @field ordinariness object of class \code{character}.
#' @field taxDistance object of class \code{numeric}.
#' @field taxID object of class \code{numeric}.
#' @field taxMask object of class \code{list}.
#' @field taxBits object of class \code{numeric}.
#' @field tree object of class \code{data.frame}.
#' @field treeDepth object of class \code{numeric}.
#' 
setClass("distance", slots = c(distance = "matrix",
                               datID = "character",
                               ordinariness = "character",
                               taxDistance = "numeric",
                               taxID = "numeric", 
                               taxMask = "list",
                               taxBits = "numeric",
                               tree = "data.frame", 
                               treeDepth = "numeric"))


