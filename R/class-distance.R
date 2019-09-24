#' distance-class
#'
#' Container for class \code{distance}.
#'
#' @field distance two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as types, and elements containing the pairwise
#' distance of types
#' @field dat_id object of class \code{character} describing the class of
#' distance / similarity being used, e.g. "naive", "taxonomic", and so on
#' @field components list containing the components necessary to calculate
#' similarity. This list is empty when \code{precompute_dist = TRUE} when
#' calculating distance. When a pairwise distance matrix is too large and
#' \code{precompute_dist = FALSE}, this list contains all the information
#' required to calculate pairwise distance between types
#'
#' @name distance-class
#' @rdname distance-class
#' @exportClass distance
#'
setClass("distance",
         slots = c(distance = "matrix",
                   dat_id = "character",
                   components = "list"))
