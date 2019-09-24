#' Calculate relative entropy
#'
#' Functions to coerce an object into a \code{relativeentropy}
#' (\code{raw_beta()} and/or \code{norm_beta()}).
#'
#' @param results \code{data.frame} containing rdiversity outputs associated
#' with \code{raw_beta()} and/or \code{norm_beta()}
#' @param meta object of class \code{metacommunity} containing the proportional
#' abundance of types, pair-wise similarity, and other associated variables
#' @param tag object of class \code{character} naming the diversity measure
#' being calculated
#'
#' @field results \code{data.frame} containing rdiversity outputs associated
#' with \code{raw_beta()} and/or \code{norm_beta()}
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
#' @return object of class \code{relativeentropy}
#' @include class-relativeentropy.R
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate raw subcommunity beta diversity
#' a <- raw_beta(meta)
#' class(a)
#'
relativeentropy <- function(results, meta, tag) {
  new("relativeentropy",
      results = results,
      measure = tag,
      type_abundance = meta@type_abundance,
      ordinariness = meta@ordinariness,
      subcommunity_weights = meta@subcommunity_weights,
      type_weights = meta@type_weights,
      dat_id = meta@dat_id,
      similarity_components = meta@similarity_components,
      similarity_parameters = meta@similarity_parameters)
}



#' @rdname relativeentropy
#' @param object object of class \code{relativeentropy}
#'
setMethod(f = "show", signature = "relativeentropy",
          definition = function(object) {
            cat("Object of class relativeentropy, containing:\n")
            cat("@results: inddiv() results\n")
            cat("@measure: measure\n")
            cat("@type_abundance: Matrix of relative abundances (",
                ncol(object@type_abundance), "subcommunities,",
                nrow(object@type_abundance), "types )\n")
            cat("@ordinariness: Matrix of type ordinariness\n")
            cat("@subcommunity_weights: Vector of subcommunity weights\n")
            cat("@type_weights: Vector of type weights\n")
          } )
