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


#' 
#' 
#' Declare new method
#' 
#' 
#' 
#' 
setGeneric(name = "rdplot", 
           def = function(data, ...) {
             standardGeneric("rdplot")
           })


#' 
#' 
#' 
#' 
#' 
#' 
#' 
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
})

