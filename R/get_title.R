#' Get title
#' 
#' @param community_type object of class \code{character}
#' @param measure object of class \code{character}
#' @param symbol (optional) by default, output is e.g. "Metacommunity x"; if 
#' symbol is set to \code{TRUE}, output will be given as "x
#' @export
#' 
#' @examples 
#' get_title("subcommunity", "normalised alpha")
#' get_title("metacommunity", "normalised alpha")
#' get_title("subcommunity", "normalised alpha", TRUE)
#' get_title("metacommunity", "normalised alpha", TRUE)
#' 
get_title <- function(community_type, measure, symbol = FALSE) {
  # Check
  if(length(measure) != 1)
    stop("Results must not contain any more than one measure.")
  if(length(community_type) != 1)
    stop("Results must not contain any more than one partition_level.")
  
  # Create tag
  if(community_type %in% "subcommunity" | community_type %in% "Subcommunity") {
    if(isTRUE(symbol)) {
      tag <- ifelse(grepl(" ", measure), unlist(strsplit(measure, " "))[2], measure) 
      tag <- as.symbol(tag)
      tag <- if(grepl("normalised", measure)) bquote(bar(.(tag))) else 
        bquote(.(tag))
    }else tag <- measure
    
  }else if(community_type %in% "metacommunity" | community_type %in% "Metacommunity") {
    if(isTRUE(symbol)) {
      tag <- ifelse(grepl(" ", measure), unlist(strsplit(measure, " "))[2], measure)
      if(tag %in% 'alpha') tag <- "A"
      if(tag %in% 'beta') tag <- 'B'
      if(tag %in% 'rho') tag <- 'R'
      if(tag %in% 'gamma') tag <- 'G'
      tag <- if(grepl("normalised", measure)) 
        bquote(bar(italic(.(tag)))) else bquote(italic(.(tag))) 
    }else tag <- measure
  }
  
  community_type <- gsub("subcommunity", "Subcommunity", community_type)
  community_type <- gsub("metacommunity", "Metacommunity", community_type)
  bquote(.(community_type) ~ .(tag))
}

