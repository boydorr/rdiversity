#' Coerce to relativeentropy
#' 
#' Functions to check if an object is a \code{relativeentropy} or coerce an  
#' object into a \code{relativeentropy}.
#' 
#' @param results two-dimensional \code{matrix} of mode \code{numeric}; 
#' @param meta object of class \code{metacommunity}; 
#' @param tag object of class \code{character}; 
#' 
#' @return object of class \code{relativeentropy}
#' @include class-relativeentropy.R 
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' meta <- metacommunity(pop)
#' 
#' # Calculate raw subcommunity beta diversity
#' a <- raw_beta(meta)
#' class(a)
#' 
relativeentropy <- function(results, meta, tag) {
  new('relativeentropy', 
      results, 
      measure = tag,
      type_abundance = meta@type_abundance,
      ordinariness = meta@ordinariness,
      subcommunity_weights = meta@subcommunity_weights,
      type_weights = meta@type_weights)
}


#' @rdname relativeentropy
#'
as.relativeentropy <- relativeentropy


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
            print(object@.Data)
          } )

