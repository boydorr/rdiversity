#' asd
#' 
#' asd
#' 
#' @param data a
#' @param qs a
#' 
#' @details 
#' 
#' @return sd
#' @export
#' 
#' @examples 
#' 
superdiv <- function(data, qs) {
  results <- list()
  for(i in 1:length(qs)) {
    subdiv <- sapply(1:ncol(data), function(y) 
      power.mean(data[,y], 1-qs[i], data@type_weights[,y]))
    results[[i]] <- power.mean(subdiv, 1-qs[i], data@subcommunity_weights)
  }
   
  results <- data.frame(matrix(unlist(results), 
                               nrow = length(results), 
                               byrow=T), 
                        stringsAsFactors=FALSE)
  colnames(results) <- paste("supercommunity", data@measure)
  row.names(results) <- paste0("q", qs)
  
  return(results)
}