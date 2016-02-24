calculate.diversity <-
function(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix))) 
{ 
  # If a single measure was entered, put it in a list
  if(!is.list(measure)&length(measure)==1) measure <- list(measure)
  if(!is.list(measure)&length(measure)!=1) 
    stop('Measure(s) must be entered as a list, or as a single string')
  
  # Check measures belong to the partitioning framework
  if(any(!unlist(lapply(measure,function(x) is(x,'diversity'))))) 
    stop('Measure does not exist.')
  
  # Calculate diversity
  output <- lapply(measure, function(x) {
    tag <- attr(x,'name')
    if(any(grep('subcommunity',tag))){
      ans <- x(pmatrix, qs, zmatrix)
      ans <- reshape2::melt(as.matrix(ans))
      ans <- cbind(ans, tag)
      colnames(ans) <- c('subcommunity','q','diversity','measure')
      ans$q <- as.numeric(gsub('q', '', ans$q))
      row.names(ans) <- NULL
      return(ans)
    }else if(any(grep('supercommunity',tag))){
      ans <- x(pmatrix, qs, zmatrix)
      ans <- cbind.data.frame('all', row.names(ans), ans, tag)
      colnames(ans) <- c('subcommunity','q','diversity','measure')
      ans$q <- as.numeric(gsub('q', '', ans$q))
      row.names(ans) <- NULL
      return(ans)
    }else stop('Something is wrong here..')
  })
  # Put everything in a nice data table
  output <- do.call(rbind,output)
  return(output)
}
