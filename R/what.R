#' what
#' 
#' @param res lk
#' 
what <- function(res) {
  part <- unique(res$partition_level)
  type <- unique(res$type_level)
  
  if(type == "type") return("inddiv")
  
  if(type == "types") {
    if(length(part)>1 && any(part %in% "subcommunity") && 
       any(part %in% "metacommunity")) return("both") else
      if(length(part)==1) {
        if(part=="subcommunity") return("subdiv") else
          if(part=="metacommunity") return("metadiv")
      }
  }
}