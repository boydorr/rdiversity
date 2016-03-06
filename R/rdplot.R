
#' rdplot()
#'
#' S4 generic function \code{rdplot()}.
#'
#' @param data object of class \code{rdiv} returns a diversity profile; object
#' of class \linkS4class{rdphylo} returns phylogenetic tree
#' @param ... additional parameters
#' @return object of class \linkS4class{rdiv} returns a diversity profile; object
#' of class \linkS4class{rdphylo} returns phylogenetic tree
#' @rdname rdplot
#'
setGeneric(name = "rdplot",
           def = function(data, ...) {
             standardGeneric("rdplot")
           } )


#' @describeIn rdplot
setMethod(f = "rdplot", signature = "rdiv", definition = function(data)
{
  plot.this <- cbind(stack(data), row.names(data), stringsAsFactors=F)
  colnames(plot.this) <- c('diversity', 'q', 'subcommunity')
  plot.this$q <- as.numeric(gsub('q', '', plot.this$q))

  g <- ggplot2::ggplot() + ggplot2::theme_classic()
  g <- g + ggplot2::geom_line(ggplot2::aes_string(x = 'q',
                                                  y = 'diversity',
                                                  group = 'subcommunity',
                                                  colour = 'subcommunity'), data = plot.this)
  g <- g + ggplot2::labs(x = bquote(italic('q')), y = data@tag)

  if(style=='big') {
    g <- g + ggplot2::theme(text = ggplot2::element_text(size=20))
  }
  return(g)
} )


#' @describeIn rdplot
setMethod(f = "rdplot", signature = "rdphylo",
          definition = function(data){
            ape::plot.phylo(data)
          } )




