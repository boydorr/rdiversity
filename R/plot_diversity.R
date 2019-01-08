#' Plot diversity
#'
#' Simple function to plot diversity profiles.
#'
#' @param res object of class \code{diversity}; output of functions
#' \code{metadiv()}, \code{subdiv()}, \code{inddiv} or any of the specific 
#' subcommunity- or metacommunity-level diversity functions.
#' 
#' @examples
#' \dontrun{
#' # Define metacommunity
#' pop1 <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop1) <- paste0("sp", 1:2)
#' pop1 <- pop1/sum(pop1)
#' meta1 <- metacommunity(pop1)
#' qs <- c(seq(0,1,.1),2:10, seq(20,100,10),Inf)
#'
#' # Plot metacommunity beta diversity
#' b <- raw_beta(meta1)
#' mc <- metadiv(b, qs)
#' plot(mc)
#' 
#' # Plot subcommunity beta diversity
#' sc <- subdiv(b, qs)
#' plot(sc)
#'
#' # Plot beta component
#' ind <- inddiv(b, qs)
#' plot(ind)
#'
#' # Plot subcommunity and metacommunity beta diversity
#' res <- rdiv(list(sc, mc))
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
#' all_res <- rdiv(list(all_sc, all_mc))
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
#' }
#' 
plot_diversity <- function(res) {
  res <- asS3(res)
  what <- what(res)
  qs <- unique(res$q)
  
  # Extract partition names
  if(what=="subdiv") {
    partitions <- unique(res$partition_name)
    res$partition_name <- factor(res$partition_name)
    N <- length(partitions)
    if(N<=12) cols <- ggthemes::ptol_pal()(N)
    if(N>12) cols <- scales::hue_pal()(N) 
    res <- get_label(res)
  }else if(what=="metadiv") {
    partitions <- "Metacommunity"
    res$partition_name <- as.character(res$partition_name)
    res$partition_name[res$partition_level=="metacommunity"] <- "Metacommunity"
    res$partition_name <- factor(res$partition_name, levels = partitions)
    res <- get_label(res)
    cols <- "black"
  }else if(what=="both") {
    partitions <- unique(res$partition_name)
    partitions <- as.character(partitions)
    res$partition_name <- as.character(res$partition_name)
    res$partition_name[res$partition_level=="metacommunity"] <- "Metacommunity"
    partitions[nchar(partitions)==0] <- "Metacommunity"
    res$partition_name <- factor(res$partition_name, levels = partitions)
    N <- length(partitions)-1
    if(N<=12) cols <- c(ggthemes::ptol_pal()(N), "black")
    if(N>12) cols <- c(scales::hue_pal()(N), "black") 
  }
  names(cols) <- partitions
  
  res <- res[,-c(3:4)]
  res <- cbind.data.frame(res, log_q = log(res$q))
  
  # Generate x-axis labels
  xlabs <- unique(res$q)
  xlabs <- xlabs[-which(xlabs %in% 0)]
  xlabs <- xlabs[-which(xlabs %in% Inf)]
  xbreaks <- log(xlabs)
  xlabs[which(!xlabs %in% c(1, 2))] <- ""
  
  # Extract q = 0 and q = Inf
  all_inf <- res[res$q %in% Inf,]
  all_neginf <- res[res$q %in% 0,]
  all_points <- rbind.data.frame(all_inf, all_neginf)
  
  # Extract all the other qs
  all_lines <- res[-which(res$q %in% Inf),]
  all_lines <- all_lines[-which(all_lines$q %in% 0),]
  
  # Plot q = 0 and q = Inf as points
  q0 <- min(all_lines$log_q) - 0.5*diff(pretty(xbreaks))[1]
  all_points$log_q[all_points$log_q %in% -Inf] <- q0
  xbreaks <- c(q0, xbreaks)
  xlabs <- c(0, xlabs)
  qInf <- max(all_lines$log_q) + 0.5*diff(pretty(xbreaks))[1]
  all_points$log_q[all_points$log_q %in% Inf] <- qInf
  xbreaks <- c(xbreaks, qInf)
  
  xlabs <- c(xlabs, expression(infinity))
  ylabs <- pretty(c(0, all_points$diversity))
  
  tmp0 <- all_points[all_points$q %in% 0,]
  row.names(tmp0) <- NULL
  smallest <- ifelse(any(qs %in% 0), min(qs[-which(qs==0)]), min(qs))
  tmp1 <- all_lines[all_lines$q %in% smallest,]
  row.names(tmp1) <- NULL
  dotted1 <- rbind.data.frame(tmp0, tmp1)
  
  tmp100 <- all_lines[all_lines$q %in% 100,]
  row.names(tmp100) <- NULL
  tmpInf <- all_points[all_points$q %in% Inf,]
  row.names(tmpInf) <- NULL
  dotted2 <- rbind.data.frame(tmp100, tmpInf)
  
  g <- ggplot2::ggplot(all_lines) + ggplot2::theme_bw() +
    ggplot2::geom_line(ggplot2::aes_string(x = "log_q",
                                           y = "diversity",
                                           group = "partition_name",
                                           colour = "partition_name"),
                       all_lines, size = 0.5) +
    ggplot2::geom_line(ggplot2::aes_string(x = "log_q",
                                           y = "diversity",
                                           group = "partition_name",
                                           colour = "partition_name"),
                       dotted1, size = 0.5, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes_string(x = "log_q",
                                           y = "diversity",
                                           group = "partition_name",
                                           colour = "partition_name"),
                       dotted2, size = 0.5, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = c(log(1), log(2), unique(all_points$log_q)),
                        linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes_string(x = "log_q",
                                            y = "diversity",
                                            group = "partition_name",
                                            colour = "partition_name"),
                        all_points, alpha = 0.7, size = 0.5) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = xbreaks, labels = xlabs) +
    ggplot2::scale_y_continuous(breaks = ylabs) +
    ggplot2::labs(x = bquote(italic("q")), y = "Diversity",
                  colour = "Partition") +
    ggplot2::expand_limits(y = c(0, max(ylabs))) +
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
  
  if(what!="both") 
    g <- g + ggplot2::facet_wrap(~measure, labeller = ggplot2::label_parsed) 
  if(what=="both") 
    g <- g + ggplot2::facet_wrap(~measure) 
  
  g
}

