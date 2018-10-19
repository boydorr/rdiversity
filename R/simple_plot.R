#' Plot diversity
#'
#' Simple function to plot diversity profiles.
#'
#' @param res object of class \code{diversity}; output of functions
#' \code{subdiv()}, \code{metadiv()}, or any of the specific subcommunity- or 
#' metacommunity-level diversity functions.
#' 
#' @examples
#' # Define metacommunity
#' pop1 <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop1) <- paste0("sp", 1:2)
#' pop1 <- pop1/sum(pop1)
#' meta1 <- metacommunity(pop1)
#' qs <- 0:2
#'
#' # Plot subcommunity beta diversity
#' b <- raw_beta(meta1)
#' sc <- subdiv(b, qs)
#' plot(sc)
#'
#' # Plot metacommunity beta diversity
#' mc <- metadiv(b, qs)
#' plot(mc)
#'
#' # Plot subcommunity and metacommunity beta diversity
#' res <- diversity(list(sc, mc))
#' plot(res)
#' 
#' # Plot all subcommunity diversity measures
#' all_sc <- subdiv(meta1, qs)
#' plot(all_sc)
#' 
#' # Plot all metacommunity diversity measures
#' all_mc <- metadiv(meta1, qs)
#' plot(all_mc)
#' 
#' # Plot all diversity measures
#' all_res <- diversity(list(all_sc, all_mc))
#' plot(all_res)
#' 
#' # Try a single population
#' pop2 <- c(1,3,4)
#' pop2 <- pop2/sum(pop2)
#' meta2 <- metacommunity(pop2)
#' sc <- sub_gamma(meta2, qs)
#' plot(sc)
#' mc <- meta_gamma(meta2, qs)
#' plot(mc)
#' 
#' # Try large number of subcommunities
#' pop3 <- matrix(sample(1000), ncol = 100)
#' row.names(pop3) <- paste0("sp", 1:10)
#' pop3 <- pop3/sum(pop3)
#' meta3 <- metacommunity(pop3)
#' sc <- sub_gamma(meta3, qs)
#' plot(sc)
#'
simple_plot <- function(res) {
  res <- asS3(res)
  what <- what(res)
  qs <- unique(res$q)
  
  # Extract partition names
  if(what=="subdiv") {
    partitions <- unique(res$partition_name)
    N <- length(partitions)
    if(N<=12) cols <- ggthemes::ptol_pal()(N)
    if(N>12) cols <- scales::hue_pal()(N) 
    res <- get_label(res)
  }else if(what=="metadiv") {
    partitions <- "Metacommunity"
    res$partition_name <- as.character(res$partition_name)
    res$partition_name[res$partition_level=="metacommunity"] <- "Metacommunity"
    res <- get_label(res)
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
  
  g <- ggplot2::ggplot(res) + ggplot2::theme_bw() +
    ggplot2::geom_line(ggplot2::aes_string(x = "q",
                                           y = "diversity",
                                           group = "partition_name",
                                           colour = "partition_name"),
                        size = 0.5) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(x = bquote(italic("q")), y = "Diversity",
                  colour = "Partition") +
    ggplot2::theme(aspect.ratio = 1,
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA),
                   strip.background = ggplot2::element_rect(colour = "black",
                                                            size = 0.4,
                                                            fill = "gray90"),
                   axis.line = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill="transparent"),
                   legend.key = ggplot2::element_blank())
  
  if(what!="both") 
    g <- g + ggplot2::facet_wrap(~measure, labeller = ggplot2::label_parsed) 
  if(what=="both") 
    g <- g + ggplot2::facet_wrap(~measure) 
  
  g
}

