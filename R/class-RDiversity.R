#' Class 'rdiv'
#' 
#' Define S4 class \code{rdiv}.
#' 
#' 
#' 
#' 
#' 
rdiv <- setClass("rdiv",
                 contains = 'data.frame',
                 slots = c(measure = "character",
                           tag = "formula",
                           level = "character"))


#' 
#'  
#'   
#'    
#'      
is.rdiv <-
  function (x) 
  {
    inherits(x, "rdiv")
  }


#' 
#' 
#' 
#' 
#' 
#' 
setMethod(f = "show", signature(object = "rdiv"), 
          definition = function(object){
            cat(object@measure, '\n\n')
            print(object)} )

setGeneric(name = "plot", 
           valueClass = "gg",
           def = function(results, ...) {
             standardGeneric("plot")

#' 
#' 
#' Declare new method
#' 
#' 
#' 
#' 
           })

setMethod(f = "plot", signature = "rdiv", definition = function(results, style='normal') 

#' 
#' 
#' 
#' 
#' 
#' 
#' 
{
    plot.this <- cbind(stack(results), row.names(results), stringsAsFactors=F)
    colnames(plot.this) <- c('diversity', 'q', 'subcommunity')
    plot.this$q <- as.numeric(gsub('q', '', plot.this$q))
    
    g <- ggplot2::ggplot() + ggplot2::theme_classic() 
    g <- g + ggplot2::geom_line(ggplot2::aes_string(x = 'q', 
                           y = 'diversity', 
                           group = 'subcommunity',
                           colour = 'subcommunity'), data = plot.this) 
    g <- g + ggplot2::labs(x = bquote(italic('q')), y = results@tag)
    
    if(style=='big') {
      g <- g + ggplot2::theme(text = ggplot2::element_text(size=20))
    }
    g
})

