setOldClass("phylo")
#' metacommunity-class
#'
#' Container for class \code{metacommunity}.
#'
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as types, and elements containing the pairwise
#' similarity of types
#' @field similarity_components list containing the components necessary to
#' calculate similarity. This list is empty when \code{precompute_dist = TRUE}
#' when calculating distance. When a pairwise distance matrix is too large and
#' \code{precompute_dist = FALSE}, this list contains all the information
#' required to calculate pairwise distance between types
#' @field similarity_parameters list containing parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()}
#' arguments)
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
#' @field raw_abundance [Phylogenetic] two-dimensional \code{matrix} of mode
#' \code{numeric} with rows as types, columns as subcommunities, and elements
#' containing the relative abundance of present day species
#' @field raw_structure [Phylogenetic] two-dimensional \code{matrix} of mode
#' \code{numeric} with rows as historical species, columns as present day
#' species, and elements containing historical species lengths within lineages
#' @field parameters [Phylogenetic] \code{data.frame} containing parameters
#' associated with each historic species in the phylogeny
#'
#' @name metacommunity-class
#' @rdname metacommunity-class
#' @exportClass metacommunity
#'
setClass("metacommunity",
         slots = c(type_abundance = "matrix",
                   similarity = "matrix",
                   similarity_components = "list",
                   similarity_parameters = "list",
                   ordinariness = "matrix",
                   subcommunity_weights = "numeric",
                   type_weights = "matrix",
                   dat_id = "character",
                   raw_abundance = "matrix",
                   raw_structure = "matrix",
                   parameters = "data.frame"))
