#' Calculate Diversity
#' 
#' Calculates the diversity, subcommunity or supercommunity, of a series of 
#' columns representing independent subcommunity counts, for a series of 
#' orders, repesented as a vector of qs.
#'
#' @param measure Diversity measure
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#'
#' @return A list of length = length(measure), where each list item contains 
#' the diversity output for the corresponding measure
#' @export
#' 
diversity <-
  function(measure, populations, qs, zmatrix = diag(nrow(pmatrix))) 
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
