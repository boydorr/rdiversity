#' plot_inddiv
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
#' qs <- c(seq(0,1,.1),2:10, seq(20,100,10),Inf)
#'
#' # Plot metacommunity beta diversity
#' b <- raw_beta(meta1)
#' ind <- inddiv(b, qs)
#' plot(ind)
#' 
#' g <- raw_gamma(meta1)
#' ind <- inddiv(g, qs)
#' plot(ind)
#' 
#' all <- inddiv(meta1, qs)
#' plot(all)
#' 
plot_inddiv <- function(res) {
  res <- asS3(res)
  qs <- unique(res$q)
  res <- res[,-2]
  if(length(qs)>1) res <- res[-which(duplicated(res)),]
  
  type <- unique(res$type_name)
  N <- length(type)
  if(N<=12) cols <- ggthemes::ptol_pal()(N)
  if(N>12) cols <- scales::hue_pal()(N) 
  names(cols) <- type
  res <- get_label(res)
  res$type_name <- factor(res$type_name)
  res$partition_name <- factor(res$partition_name)
  
  ggplot2::ggplot(res) + ggplot2::theme_bw() +
    ggplot2::geom_bar(ggplot2::aes_string(x = "partition_name",
                                          y = "diversity",
                                          group = "type_name",
                                          fill = "type_name"),
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
                   legend.background = ggplot2::element_rect(fill="transparent"),
                   legend.key = ggplot2::element_blank())
}