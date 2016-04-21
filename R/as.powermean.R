#' powermean
#' 
#' sdf
#' 
#' @param results a
#' @param super s
#' @param tag s 
#' 
#' @details 
#' 
#' @return 
#' @include class-powermean.R 
#' @export
#' 
#' @examples 
#' 
powermean <- function(results, super, tag) {
  new('powermean', 
      results, 
      measure = "alpha",
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
#' supercommunity, FALSE otherwise.
#' @export 
#' 
is.powermean <- function (x) inherits(x, "powermean")
