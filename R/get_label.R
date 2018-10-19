#' get_label
#'
#' @param res object of class \code{diversity}; output of functions
#' \code{subdiv()}, \code{metadiv()}, or any of the specific subcommunity- or 
#' metacommunity-level diversity functions.
#' @param qs parameter of conservatism; only required for \code{plot_single()}.
#'
get_label <- function(res, qs = "q") {
  type <- unique(res$type_level)
  partition <- unique(res$partition_level)
  measure <- unique(res$measure)
  
  tag <- measure
  if(any(tag %in% 'gamma'))
    tag[tag=='gamma'] <- 'raw gamma'
  tag <- data.frame(do.call(rbind, strsplit(tag, split = " ")))
  tag[,2] <- substring(tag[,2], 1, 1)
  
  # Subcommunity-level diversity
  if(type=="types" & partition=="subcommunity") {
    tag <- apply(tag, 1, function(x)
      if(x[1] == "normalised") {
        paste0("bar(symbol(",x[2],"))")
      }else {
        paste0("symbol(",x[2],")")
      })
    tag <- paste0("{}^italic(",qs,")*",tag,"[italic(j)]*{}^bold(Z)")
  }
  
  # Individual-level diversity
  if(type=="type") {
    tag <- apply(tag, 1, function(x)
      if(x[1] == "normalised") {
        paste0("bar(italic(",x[2],"))")
      }else {
        paste0("italic(",x[2],")")
      })
    tag <- paste0("italic(",tag,"[ij])")
  }
  
  if(type=="types" & partition=="metacommunity") {
    tag[,2] <- toupper(tag[,2])
    tag <- apply(tag, 1, function(x)
      if(x[1] == "normalised") {
        paste0("{}^italic(",qs,")*bar(italic(",x[2],"))")
      }else {
        paste0("{}^italic(",qs,")*italic(",x[2],")")
      })
    tag <- paste0(tag,"^bold(Z)")
  }
  
  res$measure <- factor(res$measure, levels = measure, labels = tag)
  res
}
