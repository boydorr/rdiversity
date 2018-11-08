#' distance-class
#' 
#' Container for class \code{distance}.
#' 
#' @name distance-class
#' @rdname distance-class
#' @exportClass distance
#' 
#' @field phylo object of class \code{phylo}.
#' @field depth object of class \code{numeric}.
#' 
setClass("distance", slots = c(distance = "matrix",
                               divID = "character",
                               taxID = "numeric",
                               taxmask = "list"))


