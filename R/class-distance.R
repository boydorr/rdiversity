setOldClass("phylo")
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
#' @field taxDistance object of class \code{numeric}.
#' @field taxID object of class \code{numeric}.
#' @field taxMask object of class \code{list}.
#' @field taxBits object of class \code{numeric}.
#' @field phylo object of class \code{phylo}.
#' @field phyDepth object of class \code{numeric}.
#' 
setClass("distance", slots = c(distance = "matrix",
                               datID = "character",
                               taxDistance = "numeric",
                               taxID = "numeric", 
                               taxMask = "list",
                               taxBits = "numeric",
                               phylo = "phylo", 
                               phyDepth = "numeric"))


