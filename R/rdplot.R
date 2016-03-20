#' rdplot()
#'
#' S4 generic function \code{rdplot()}.
#'
#' @param data object of class \code{rdiv} returns a diversity profile; object
#' of class \linkS4class{rdphylo} returns phylogenetic tree
#' @param ... additional parameters
#' @return object of class \linkS4class{rdiv} returns a diversity profile; object
#' of class \linkS4class{rdphylo} returns phylogenetic tree
#' @include class-RDiversity.R
#' 
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' output <- diversity(subcommunity.alpha.bar, data, 0:2)
#' 
#' rdplot(output)
#' 
setGeneric(name = "rdplot",
           def = function(data, ...) {
             standardGeneric("rdplot")
           } )


setMethod(f = "rdplot", signature = "rdiv", definition = function(data, style='normal')
{
  plot.this <- cbind(stack(data), row.names(data), stringsAsFactors=F)
  colnames(plot.this) <- c('diversity', 'q', 'subcommunity')
  plot.this$q <- as.numeric(gsub('q', '', plot.this$q))

  g <- ggplot2::ggplot(data = plot.this, 
                       ggplot2::aes_string(x = 'q',
                                           y = 'diversity',
                                           group = 'subcommunity',
                                           colour = 'subcommunity'))
  g <- g + ggplot2::theme_classic()
  g <- g + ggplot2::labs(x = bquote(italic('q')), y = data@tag)

  if(style=='big') {
    g <- g + ggplot2::theme(text = ggplot2::element_text(size=20))
    g <- g + ggplot2::geom_line(size=2)
  } else {
    g <- g + ggplot2::geom_line()
  }
  return(g)
} )

# 
# setMethod(f = "rdplot", signature = "rdphylo",
#           definition = function(data){
#             ape::plot.phylo(data)
#           } )




