#' plot_single
#' 
#' @param res object of class \code{diversity}; output of functions
#' \code{subdiv()}, \code{metadiv()}, or any of the specific subcommunity- or 
#' metacommunity-level diversity functions.
#' 
plot_single <- function(res) {
  qs <- unique(res$q)
  what <- what(res)
  res <- asS3(res)

  # Extract partition names
  if(what=="subdiv") {
    partitions <- unique(res$partition_name)
    N <- length(partitions)
    if(N<=12) cols <- ggthemes::ptol_pal()(N)
    if(N>12) cols <- scales::hue_pal()(N) 
    res <- get_label(res, qs)
  }else if(what=="metadiv") {
    partitions <- "Metacommunity"
    res$partition_name <- as.character(res$partition_name)
    res$partition_name[res$partition_level=="metacommunity"] <- "Metacommunity"
    res <- get_label(res, qs)
    cols <- "black"
  }else if(what=="both") {
    partitions <- unique(res$partition_name)
    partitions <- as.character(partitions)
    res$partition_name <- as.character(res$partition_name)
    res$partition_name[res$partition_level=="metacommunity"] <- "Metacommunity"
    partitions[nchar(partitions)==0] <- "Metacommunity"
    N <- length(partitions)-1
    if(N<=12) cols <- c(ggthemes::ptol_pal()(N), "black") 
    if(N>12) cols <- c(scales::hue_pal()(N), "black") 
  }
  names(cols) <- partitions 
  res$partition_name <- factor(res$partition_name, levels = partitions)
  
  
  ggplot2::ggplot(res) + ggplot2::theme_bw() +
    ggplot2::geom_bar(ggplot2::aes_string(x = "partition_name",
                                          y = "diversity",
                                          # group = "partition_name",
                                          fill = "partition_name"),
                      colour = "black",
                      stat = "identity",
                      position = "dodge",
                      size = 0.5) +
    ggplot2::facet_wrap(~measure, labeller = ggplot2::label_parsed) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::labs(x = bquote("Partition"), y = "Diversity",
                  fill = "Type") +
    ggplot2::theme(aspect.ratio = 1,
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_rect(colour = "black",
                                                            size = 0.4,
                                                            fill = "gray90"),
                   axis.line = ggplot2::element_blank(),
                   legend.position = "none")
}