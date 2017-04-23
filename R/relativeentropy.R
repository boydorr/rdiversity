#' Calculate relative entropy
#' 
#' Functions to check if an object is a \code{relativeentropy}, or coerse an  
#' object into a \code{relativeentropy}; for \code{raw_beta()} or 
#' \code{norm_beta()}. 
#' 
#' @param tag measure
#' @param results \code{matrix} of mode \code{numeric}; contains values 
#' calculated from diversity-term functions \code{raw_beta()} and 
#' \code{norm_beta()}
#' @param meta object of class \code{metacommunity}
#' 
#' @return object of class \code{relativeentropy}
#' @include class-relativeentropy.R 
#' @export
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
  new('relativeentropy', 
      results = results, 
      measure = tag,
      type_abundance = meta@type_abundance,
      ordinariness = meta@ordinariness,
      subcommunity_weights = meta@subcommunity_weights,
      type_weights = meta@type_weights)
}


#' @rdname relativeentropy
#' @param x any R object 
#' @return \code{is.relativeentropy(x)} returns TRUE if its argument is a 
#' relativeentropy, FALSE otherwise.
#' @export 
#' 
is.relativeentropy <- function (x) 
  inherits(x, "relativeentropy")


#' @rdname relativeentropy
#' @param object object of class \code{relativeentropy}
#' 
setMethod(f = "show", signature = "relativeentropy", 
          definition = function(object) {
            cat('Object of class relativeentropy, containing:\n')
            cat('@results: inddiv() results\n')
            cat('@measure: measure\n')
            cat('@type_abundance: Matrix of relative abundances (', 
                ncol(object@type_abundance), 'subcommunities,',
                nrow(object@type_abundance), 'types )\n')
            cat('@ordinariness: Matrix of type ordinariness\n')
            cat('@subcommunity_weights: Vector of subcommunity weights\n')
            cat('@type_weights: Vector of type weights\n')
          } )

