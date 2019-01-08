#' get_label
#'
#' @param res object of class \code{diversity}; output of functions
#' \code{subdiv()}, \code{metadiv()}, or any of the specific subcommunity- or 
#' metacommunity-level diversity functions.
#' @param qs parameter of conservatism; only required for \code{plot_single()}.
#'
get_label <- function(res, qs = "q") {
  normalised <- symbol <- bracket <- Z <- label <- NULL
  tmp <- res 
  type <- unique(tmp$type_level)
  partition <- unique(tmp$partition_level)
  measure <- unique(tmp$measure)
  similarity <- unique(tmp$datID)
  # qs <- unique(tmp$q)
    
  tag <- measure
  tmp <- dplyr::mutate(tmp,
                normalised = ifelse(grepl("normalised", measure), "bar(", ""),
                bracket = ifelse(grepl("normalised", measure), ")", ""))
  tmp <- dplyr:: mutate(tmp, Z = "Z")
  if(any(similarity == "naive")) tmp$Z[tmp$datID == "naive"] <- "I"
  if(any(similarity == "taxonomic")) tmp$Z[tmp$datID == "taxonomic"] <- "tax"
  if(any(similarity == "phybranch")) tmp$Z[tmp$datID == "phybranch"] <- "tree"
  if(any(similarity == "phydist" && res$transformation == "linear"))
    tmp$Z[tmp$datID == "phydist" && res$transformation == "linear"] <- "PPD[l]"
  if(any(similarity == "phydist" && res$transformation == "exponential"))
    tmp$Z[tmp$datID == "phydist" && res$transformation == "exponential"] <- "PPD[e]"
  tmp <- dplyr::mutate(tmp, symbol = substring(
    gsub("raw ", "", gsub("normalised ", "", measure)),1,1))
  if(type=="types" & partition=="metacommunity") 
    
  # Subcommunity-level diversity
  if(type=="types" & partition=="subcommunity") 
    tmp <- dplyr::mutate(tmp, label = paste0(
      "{}^italic(q)*", normalised, "symbol(", symbol, ")", bracket,
      "[italic(j)]*{}^bold(", Z, ")"))
  
  # Individual-level diversity
  if(type=="type") 
    tmp <- dplyr::mutate(tmp, label = paste0(
      "italic(",normalised,"italic(",symbol,")",bracket,"[ij])"))
  
  # Metacommunity-level diversity
  if(type=="types" & partition=="metacommunity") {
    tmp$symbol <- toupper(tmp$symbol)
    tmp <- dplyr::mutate(tmp, label = paste0(
      "{}^italic(q)*", normalised, "italic(", symbol, bracket, 
      ")^bold(", Z, ")"))
  }
  
  tmp$measure <- factor(tmp$measure, levels = tmp$measure, labels = tmp$label)
  if(any(colnames(tmp)=="bracket"))
    tmp <- dplyr::select(tmp, -normalised, -bracket, -Z, -symbol, -label) else
      tmp <- dplyr::select(tmp, -normalised, -Z, -symbol, -label)
  tmp
}
