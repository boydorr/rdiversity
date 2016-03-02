# Define S4 Class
rdiv <- setClass("rdiv",
                 contains = 'data.frame',
                 slots = c(measure = "character",
                           tag = "formula",
                           level = "character"))


# Constructor function
rdiv <- function(measure, tag, level) 
  new("RDiversity", 
      measure = measure,
      tag = tag,
      level = level)


# 
is.RDiversity <-
  function (x) 
  {
    inherits(x, "RDiversity")
  }

#show() is the S4 analogy of print() in S3

setGeneric("plot.diversity", 
           # valueClass = "gg", 
           function(results) {
             standardGeneric("plot.diversity")
           })

setMethod("plot.diversity", "rdiv", function(results) 
{
  if(is.element('ggplot2', installed.packages()[,1])) {
    plot.this <- cbind(stack(results), row.names(results), stringsAsFactors=F)
    colnames(plot.this) <- c('diversity', 'q', 'subcommunity')
    plot.this$q <- as.numeric(gsub('q', '', plot.this$q))
    
    g <- ggplot2::ggplot() + theme_classic() 
    g <- g + geom_line(aes(x = q, 
                           y = diversity, 
                           group = subcommunity,
                           colour = subcommunity), data = plot.this) 
    g <- g + labs(x = bquote(italic('q')), y = results@tag)
    g
  } else {
    plot.this <- t(data.frame(results))
    g <- matplot(plot.this, type='l', col = 1:nrow(results),
                 xlab = bquote(italic('q')), 
                 ylab = results@tag)
    # legend('right', legend = row.names(results), col = 1:nrow(results))
  }
  
})

