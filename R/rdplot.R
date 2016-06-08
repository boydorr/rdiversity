#' rdplot()
#'
#' Simple function to plot the diversity profile of a single diversity measure
#' 
#' @param data output
#' @param plot.title object of class \code{character}
#' 
#' @export
#'
rdplot <- function(data, plot.title) {
  community <- unique(data$community)
  measure <- unique(data$measure)
  partitions <- unique(data$partition)

  if(length(measure)!=1 | length(community)!=1)
    stop('This function will only plot the results from a single diversity measure.')
  
  # title
  if(missing(plot.title)) 
    plot.title = "Diversity profile"
  
  # y-axis
  com <- strsplit(community, " ")[[1]]
  com <- paste(toupper(substring(com, 1,1)), substring(com, 2),
        sep="", collapse=" ")
  
  if(grepl("bar", measure)) {
    m <- as.symbol(gsub(" bar","", measure))
    m <- bquote(bar(.(m)))
    y.title <- bquote(.(com) ~ .(m))
  }else{
    m <- as.symbol(measure)
    y.title <- bquote(.(com) ~ .(m))
  }
  
  # Qualitative color schemes by Paul Tol
  # https://personal.sron.nl/~pault/colourschemes.pdf
  #
  paul.tol <- list(tol1qualitative=c("#4477AA"),
                   tol2qualitative=c("#4477AA", "#CC6677"),
                   tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677"),
                   tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
                   tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
                   tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677",
                                     "#AA4499"),
                   tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", 
                                     "#CC6677","#AA4499"),
                   tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                                     "#DDCC77", "#CC6677","#AA4499"),
                   tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                                     "#DDCC77", "#CC6677", "#882255", "#AA4499"),
                   tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                                      "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
                   tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", 
                                      "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", 
                                      "#AA4499"),
                   tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", 
                                      "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", 
                                      "#882255", "#AA4499"))
  
  
  # plot
  ggplot2::ggplot(data, aes_string(x="q", y="diversity")) + theme_bw() +
    ggplot2::geom_line(aes_string(group = "partition", colour = "partition"), size = 1) + 
    ggplot2::labs(title=plot.title, 
         x = bquote(italic("q")), 
         y = y.title,
         colour = "Partitions") + 
    ggplot2::theme(axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1)) + 
    ggplot2::scale_color_manual(values = paul.tol[[length(partitions)]]) 
  
}