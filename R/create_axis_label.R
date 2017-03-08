#' Create axis label
#' 
#' @param results object of class \code{data.frame} or \code{data_frame}, output 
#' from \code{subdiv()} or \code{metadiv}
#' @param symbol (optional) by default, output is e.g. "Metacommunity x"; if 
#' symbol is set to \code{TRUE}, output will be given as "x
#' @export
#' 
create_axis_label <- function(results, symbol = F) {
  # Identify which diversity measure is being calculated
  community.type <- unique(results$community)
  measure <- unique(results$measure)
  
  # Check
  if(length(community.type) != 1)
    stop("Results must not contain any more than one measure of diversity.")
  if(length(measure) != 1)
    stop("Results must not contain any more than one measure of diversity.")
  
  if(is.factor(community.type))
    community.type <- as.character(community.type)
  if(is.factor(measure))
    measure <- as.character(measure)
  
  if(community.type %in% "subcommunity") {
    if(grepl(" ", measure)) {
      tag <- unlist(strsplit(measure, " "))[2]
      } else tag <- measure
    
    tag <- as.symbol(tag)
    
    if(grepl("normalised", measure)) {
      tag <- bquote(bar(.(tag)))
    }else {
      tag <- bquote(.(tag))
    }
  }else if(community.type %in% "metacommunity") {
    if(grepl(" ", measure)) {
      type <- unlist(strsplit(measure, " "))[2]
    } else type <- measure
    
    if(type %in% 'alpha') tag <- "A"
    if(type %in% 'beta') tag <- 'B'
    if(type %in% 'rho') tag <- 'R'
    if(type %in% 'gamma') tag <- 'G'

    if(grepl("normalised", measure)) {
      tag <- bquote(bar(italic(.(tag))))
    }else {
      tag <- bquote(italic(.(tag)))
    }
    }
  community.type <- gsub("subcommunity", "Subcommunity", community.type)
  community.type <- gsub("metacommunity", "Metacommunity", community.type)
  
  if(!isTRUE(symbol)) tag <- bquote(.(community.type) ~ .(tag))

  tag
}
    
