#' powermean-class
#'
#' Container for class \code{powermean}.
#'
#' @field results \code{data.frame} containing rdiversity output
#' @field measure object of class \code{character} naming the diversity
#' measure being calculated
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as subcommunities, and elements containing the
#' ordinariness of types within subcommunities
#' @field subcommunity_weights \code{vector} of mode \code{numeric} containing
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric},
#' with rows as types, columns as subcommunities, and elements containing
#' weights of types within a subcommunity
#' @field dat_id object of class \code{character} describing the class of
#' distance / similarity being used, e.g. "naive", "taxonomic", and so on
#' @field similarity_components list containing the components necessary to
#' calculate similarity. This list is empty when \code{precompute_dist = TRUE}
#' when calculating distance. When a pairwise distance matrix is too large and
#' \code{precompute_dist = FALSE}, this list contains all the information
#' required to calculate pairwise distance between types
#' @field similarity_parameters list containing parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()}
#' arguments)
#'
#' @name powermean-class
#' @rdname powermean-class
#' @exportClass powermean
#'
setClass("powermean", slots = c(results = "matrix",
                                measure = "character",
                                type_abundance = "matrix",
                                ordinariness = "matrix",
                                subcommunity_weights = "vector",
                                type_weights = "matrix",
                                dat_id = "character",
                                similarity_components = "list",
                                similarity_parameters = "list"))
