#' Coerce to powermean
#' 
#' Functions to check if an object is a \code{powermean} or coerce an  
#' object into a \code{powermean}.
#' 
#' \code{powermean} is a class of object output from calculating 
#' \code{alpha}, \code{alphabar}, \code{rho}, 
#' \code{rhobar}, and \code{gamma} diversity terms; 
#' \code{beta} and \code{betabar} are output as 
#' \code{relativeentropy} objects. 
#' 
#' To calculate the subcommunity or supercommunity diversity, an object of
#' class \code{powermean} is input to \code{subdiv()} or \code{superdiv()}, 
#' respectively.
#' 
#' @param results two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains values calculated from diversity-term functions, e.g. 
#' \code{alphabar()}, \code{beta}, etc.
#' @param super object of class \code{supercommunity}; contains proportional 
#' abundance of types, pair-wise similarity, and other associated variables.
#' @param tag object of class \code{character}; contains an identifier 
#' associated with the calculated diversity-term, e.g. "alphabar", "beta", etc.
#' 
#' @return \code{powermean(x)} returns an object of class \code{powermean}.
#' @include class-powermean.R 
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity alpha diversity (takes the powermean)
#' a <- alpha(super)
#' class(a)
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
#' @return \code{as.powermean(x)} returns an object of class \code{powermean}.
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
#' @return \code{print(x)} prints an object object of class \code{powermean}
#' 
setMethod(f = "show", signature = "powermean", 
          definition = function(object) {
            print(object@.Data)
          } )
