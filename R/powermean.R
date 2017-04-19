#' Calculate power mean
#'
#' Functions to check if an object is a \code{powermean} or calculate 
#' the power mean of diversity components: \code{raw_alpha()}, 
#' \code{norm_alpha()}, \code{raw_rho()}, \code{norm_rho()}, or 
#' \code{raw_gamma()}. 
#' 
#' @param results \code{matrix} of mode \code{numeric}; contains values 
#' calculated from diversity-term functions \code{norm_alpha()}, 
#' \code{raw_alpha()}, \code{raw_rho()}, \code{norm_rho()}, and 
#' \code{raw_gamma()}
#' @param meta object of class \code{metacommunity}; contains proportional
#' abundance of types, pair-wise similarity, and other associated variables.
#' @param tag object of class \code{character}; contains an identifier
#' associated with the calculated diversity-term, e.g.
#' "norm_alpha", "raw_beta", etc.
#'
#' @return \code{powermean(x)} returns an object of class \code{powermean}.
#' @include class-powermean.R
#' @export
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
  output <- reshape2::melt(results)
  output <- cbind.data.frame(measure = tag, 
                             q = NA,  
                             type_level = "type", 
                             type_name = output$Var1, 
                             partition_level = "type",
                             partition_name = output$Var2,
                             diversity = output$value, 
                             stringsAsFactors = FALSE)
  new('powermean',
      output = output,
      results = results,
      type_abundance = meta@type_abundance,
      ordinariness = meta@ordinariness,
      subcommunity_weights = meta@subcommunity_weights,
      type_weights = meta@type_weights)
}


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
            print(object@output)
          })
