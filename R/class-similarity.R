#' similarity-class
#' 
#' Container for class \code{similarity}.
#' 
#' @name similarity-class
#' @rdname similarity-class
#' @exportClass similarity
#' 
#' @field similarity object of class \code{matrix}.
#' @field datID object of class \code{character}.
#' @field taxSimilarity object of class \code{numeric}.
#' @field taxID object of class \code{numeric}.
#' @field taxMask object of class \code{list}.
#' @field taxBits object of class \code{numeric}.
#' @field parameters object of class \code{list}.
#' 
setClass("similarity", slots = c(similarity = "matrix",
                                 datID = "character",
                                 taxSimilarity = "numeric",
                                 taxID = "numeric", 
                                 taxMask = "list",
                                 taxBits = "numeric",
                                 parameters = "list"))