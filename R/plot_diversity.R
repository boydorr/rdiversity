#' Plot diversity
#'
#' Simple function to plot diversity profiles.
#'
#' @param data output
#'
#' @export
#'
#' @examples
#' # Define metacommunity
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate subcommunity beta diversity (takes the relative entropy)
#' b <- raw_beta(meta)
#' res <- subdiv(b, 0:2)
#' plot_diversity(res)
#'
#' # Calculate all measures of subcommunity diversity
#' res <- subdiv(meta, 0:2)
#' plot_diversity(res)
#'
#' # Try a single population
#' pop <- c(1,3,4)
#' meta <- metacommunity(pop)
#' res <- meta_gamma(meta, 0:2)
#' plot_diversity(res)
#'
plot_diversity <- function(data) {
  community <- unique(data$partition_level)
  measure <- unique(data$measure)
  data$partition_name <- as.factor(data$partition_name)

  community <- gsub("subcommunity", "Subcommunity", community)
  community <- gsub("metacommunity", "Metacommunity", community)
  
  if(isTRUE(all.equal(1, length(measure))))
    tag <- rdiversity::get_title(community, measure, TRUE) else
      tag <- paste(community, "diversity")

  g <- ggplot2::ggplot(data, ggplot2::aes_string(x="q", y="diversity")) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(ggplot2::aes_string(group = "partition_name",
                                           colour = "partition_name")) +
    ggplot2::labs(x = bquote(italic("q")), y = tag, colour = "Partitions") +
    ggthemes::scale_color_ptol()

  if(community %in% "Metacommunity") 
    g <- g + theme(legend.position="none")
  
  if(length(measure)>1) {
    x = ggplot2::label_bquote(.(measure))
    g <- g + ggplot2::facet_wrap(~measure, labeller = x)
  }

  g
}

