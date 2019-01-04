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
#' @field components object of class \code{list}.
#' @field parameters object of class \code{list}.
#' 
setClass("similarity", slots = c(similarity = "matrix",
                                 datID = "character",
                                 components = "list",
                                 parameters = "list"))