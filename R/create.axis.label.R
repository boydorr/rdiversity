#' Create axis label
#' 
#' 
#' 
#' 
create.axis.label <- function(results, symbol = F) {
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
  }else if(community.type %in% "supercommunity") {
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
 
  community.type <- gsub("s", "S", community.type)
  
  if(!isTRUE(symbol)) tag <- bquote(.(community.type) ~ .(tag))

  tag
}
    
