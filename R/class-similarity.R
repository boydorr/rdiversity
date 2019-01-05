#' similarity-class
#' 
#' Container for class \code{similarity}.
#' 
#' @name similarity-class
#' @rdname similarity-class
#' @exportClass similarity
#' 
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric} 
#' with rows as types, columns as types, and elements containing the pairwise
#' similarity of types
#' @field datID object of class \code{character} describing the class of 
#' distance / similarity being used, e.g. "naive", "taxonomic", and so on
#' @field components list containining the components necessary to calculate
#' similarity. This list is empty when \code{precompute_dist = TRUE} when
#' calculating distance. When a pairwise distance matrix is too large and  
#' \code{precompute_dist = FALSE}, this list contains all the information 
#' required to calculate pairwise distance between types
#' @field parameters list containining parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()} 
#' arguments)
#' 
setClass("similarity", slots = c(similarity = "matrix",
                                 datID = "character",
                                 components = "list",
                                 parameters = "list"))