#' Coerce to relativeentropy
#' 
#' Functions to check if an object is a \code{relativeentropy} or coerce an  
#' object into a \code{relativeentropy}.
#' 
#' @param results two-dimensional \code{matrix} of mode \code{numeric}; 
#' @param super object of class \code{supercommunity}; 
#' @param tag object of class \code{character}; 
#' 
#' @return object of class \code{relativeentropy}
#' @include class-relativeentropy.R 
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate raw subcommunity beta diversity
#' a <- raw.beta(super)
#' class(a)
#' 
relativeentropy <- function(results, super, tag) {
  new('relativeentropy', 
      results, 
      measure = tag,
      type_abundance = super@type_abundance,
      ordinariness = super@ordinariness,
      subcommunity_weights = super@subcommunity_weights,
      type_weights = super@type_weights)
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

