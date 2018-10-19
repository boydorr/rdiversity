setOldClass("phylo")
#' similarity-class
#' 
#' Container for class \code{similarity}.
#' 
#' @name similarity-class
#' @rdname similarity-class
#' @exportClass similarity
#' 
#' @field phylo object of class \code{phylo}.
#' @field depth object of class \code{numeric}.
#' 
setClass("similarity", slots = c(phylo = "phylo",
                                 depth = "numeric"))


