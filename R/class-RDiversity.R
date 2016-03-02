# Define S4 Class
rdiv <- setClass("rdiv",
                 contains = 'data.frame',
                 slots = c(measure = "character",
                           tag = "formula",
                           level = "character"))


# # Constructor function
# rdiv <- function(data, measure, tag, level) 
#   new("rdiv", 
#       data, 
#       measure = measure,
#       tag = tag,
#       level = level)

  
# 
is.rdiv <-
  function (x) 
  {
    inherits(x, "rdiv")
  }


setMethod(f = "show", signature(object = "rdiv"), 
          definition = function(object){
            cat(object@measure, '\n\n')
            print(object)} )

# Initialize generic function
setGeneric(name = "plot", 
           valueClass = "gg",
           def = function(results, ...) {
             standardGeneric("plot")
           })

# Define methods for generic 
setMethod(f = "plot", signature = "rdiv", definition = function(results, style='normal') 
{
  # if(is.element('ggplot2', installed.packages()[,1])) {
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
    
  # } else {
  #   plot.this <- t(data.frame(results))
  #   g <- matplot(plot.this, type='l', col = 1:nrow(results),
  #                xlab = bquote(italic('q')), 
  #                ylab = results@tag)
    # legend('right', legend = row.names(results), col = 1:nrow(results))
  
})

