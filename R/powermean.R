#' Calculate power mean
#'
#' Functions to coerce an object into a \code{powermean} (\code{raw_alpha()},
#' \code{norm_alpha()}, \code{raw_rho()}, \code{norm_rho()}, and/or
#' \code{raw_gamma()}).
#'
#' @param results \code{data.frame} containing rdiversity outputs associated
#' with \code{norm_alpha()}, \code{raw_alpha()}, \code{raw_rho()},
#' \code{norm_rho()}, and/or \code{raw_gamma()}
#' @param meta object of class \code{metacommunity} containing the proportional
#' abundance of types, pair-wise similarity, and other associated variables
#' @param tag object of class \code{character} naming the diversity measure
#' being calculated
#'
#' @field results \code{data.frame} containing rdiversity outputs associated
#' with \code{norm_alpha()}, \code{raw_alpha()}, \code{raw_rho()},
#' \code{norm_rho()}, and/or \code{raw_gamma()}
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
#' @return \code{powermean(x)} returns an object of class \code{powermean}.
#' @include class-powermean.R
#'
#' @noRd
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate subcommunity raw alpha diversity (takes the powermean)
#' a <- raw_alpha(meta)
#' class(a)
#'
powermean <- function(results, meta, tag) {
  new("powermean",
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



#' @rdname powermean
#' @param object object of class \code{powermean}
#' @return \code{print(x)} prints an object object of class \code{powermean}
#'
#' @noRd
#'
setMethod(f = "show", signature = "powermean",
          definition = function(object) {
            cat("Object of class powermean.")
          } )
