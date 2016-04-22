#' Coerse to powermean
#' 
#' Functions to check if an object is a \code{powermean} or coerce an  
#' object into a \code{powermean}.
#' 
#' @param results two-dimensional \code{matrix} of mode \code{numeric}; 
#' @param super object of class \code{supercommunity}; 
#' @param tag object of class \code{character}; 
#' 
#' @details 
#' 
#' @return object of class \code{powermean}
#' @include class-powermean.R 
#' @export
#' 
#' @examples 
#' 
powermean <- function(results, super, tag) {
  new('powermean', 
      results, 
      measure = tag,
      type_abundance = super@type_abundance,
      ordinariness = super@ordinariness,
      subcommunity_weights = super@subcommunity_weights,
      type_weights = super@type_weights)
}


#' @rdname powermean
#'
as.powermean <- powermean


#' @rdname powermean
#' @param x any R object 
#' @return \code{is.powermean(x)} returns TRUE if its argument is a 
#' powermean, FALSE otherwise.
#' @export 
#' 
is.powermean <- function (x) inherits(x, "powermean")


#' @rdname powermean
#' @param object object of class \code{powermean}
#' 
setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
            print(object@.Data)
          } )