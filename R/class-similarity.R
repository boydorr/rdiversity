#' similarity-class
#'
#' Container for class \code{similarity}.
#'
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as types, and elements containing the pairwise
#' similarity of types
#' @field dat_id object of class \code{character} describing the class of
#' distance / similarity being used, e.g. "naive", "taxonomic", and so on
#' @field components list containing the components necessary to calculate
#' similarity. This list is empty when \code{precompute_dist = TRUE} when
#' calculating distance. When a pairwise distance matrix is too large and
#' \code{precompute_dist = FALSE}, this list contains all the information
#' required to calculate pairwise distance between types
#' @field parameters list containing parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()}
#' arguments)
#'
#' @name similarity-class
#' @rdname similarity-class
#' @exportClass similarity
#'
setClass("similarity", slots = c(similarity = "matrix",
                                 dat_id = "character",
                                 components = "list",
                                 parameters = "list"))
