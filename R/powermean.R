#' Calculate power mean
#'
#' Functions to check if an object is a \code{powermean}, or coerse an object 
#' into a \code{powermean}; for \code{raw_alpha()}, \code{norm_alpha()}, 
#' \code{raw_rho()}, \code{norm_rho()}, or  \code{raw_gamma()}. 
#' 
#' @param tag measure
#' @param results \code{matrix} of mode \code{numeric}; contains values 
#' calculated from diversity-term functions \code{norm_alpha()}, 
#' \code{raw_alpha()}, \code{raw_rho()}, \code{norm_rho()}, and 
#' \code{raw_gamma()}
#' @param meta object of class \code{metacommunity}; contains proportional
#' abundance of types, pair-wise similarity, and other associated variables.
#'
#' @return \code{powermean(x)} returns an object of class \code{powermean}.
#' @include class-powermean.R
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
  new('powermean',
      results = results,
      measure = tag,
      type_abundance = meta@type_abundance,
      ordinariness = meta@ordinariness,
      subcommunity_weights = meta@subcommunity_weights,
      type_weights = meta@type_weights,
      datID = meta@datID,
      similarity_components = meta@similarity_components,
      similarity_parameters = meta@similarity_parameters)
}



#' @rdname powermean
#' @param object object of class \code{powermean}
#' @return \code{print(x)} prints an object object of class \code{powermean}
#'
setMethod(f = "show", signature = "powermean",
          definition = function(object) {
            cat('Object of class powermean.')
          } )
