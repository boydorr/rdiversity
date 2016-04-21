#' 
#' 
#' 
#' 
subdiv <- function(data, qs) {
  # weight <- sapply(1:ncol(data), function(x)
  #   (data@type_abundance[,x]/colSums(data@type_abundance)[x]))
  
  results <- list()
  for(i in 1:length(qs))
  results[[i]] <- sapply(1:ncol(data), function(y) 
    power.mean(data[,y], i, weight[,y]))
  
  results <- data.frame(matrix(unlist(results), 
                               nrow = length(results), 
                               byrow=T), 
                        stringsAsFactors=FALSE)
  colnames(results) <- colnames(data)
  row.names(results) <- paste0("q", qs)
  
  return(results)
}