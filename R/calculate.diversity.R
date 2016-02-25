diversity <-
  function(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix))) 
  {
    output <- lapply(measure, function(x) {
      
      ans <- x(pmatrix, qs, zmatrix)
      tag <- attr(ans, 'measure') 
      
      if(attr(ans,'type')=='subcommunity') {
        tmp <- reshape2::melt(as.matrix(ans)) 
        tmp <- cbind(tmp, tag)
        colnames(tmp) <- c('subcommunity','q','diversity','measure')
        tmp$q <- as.numeric(gsub('q', '', tmp$q))
        row.names(tmp) <- NULL
        attr(tmp,'measure') <- attr(ans,'measure')
        attr(tmp,'tag') <- attr(ans,'tag')
        attr(tmp,'type') <- attr(ans,'type')
        return(tmp)
        
    }else if(attr(ans,'type')=='supercommunity') {
      tmp <- reshape2::melt(as.matrix(ans)) 
      tmp <- cbind(tmp, tag)
      tmp <- tmp[,-2]
      colnames(tmp) <- c('q','diversity','measure')
      tmp$q <- as.numeric(gsub('q', '', tmp$q))
      row.names(tmp) <- NULL
      attr(tmp,'measure') <- attr(ans,'measure')
      attr(tmp,'tag') <- attr(ans,'tag')
      attr(tmp,'level') <- attr(ans,'level')
      return(tmp)
    }}
  )
}
