#' 
#' 
#' 
#' 
subdiv <- function(data, qs) {
  
  results <- list()
  for(i in 1:length(qs))
  results[[i]] <- sapply(1:ncol(data), function(y) 
    power.mean(data[,y], i, data@type_weights[,y]))
  
  results <- data.frame(matrix(unlist(results), 
                               nrow = length(results), 
                               byrow=T), 
                        stringsAsFactors=FALSE)
  colnames(results) <- colnames(data)
  row.names(results) <- paste0("q", qs)
  
  return(results)
}