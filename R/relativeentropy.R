#' Calculate relative entropy
#' 
#' Functions to check if an object is a \code{relativeentropy} or calculate 
#' the relative entropy of diversity components: \code{raw_beta()} or 
#' \code{norm_beta()}. 
#' 
#' @param results \code{matrix} of mode \code{numeric}; contains values 
#' calculated from diversity-term functions \code{raw_beta()} and 
#' \code{norm_beta()}
#' @param meta object of class \code{metacommunity}
#' @param tag object of class \code{character}
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
      results, 
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
            print(object@.Data)
          } )

