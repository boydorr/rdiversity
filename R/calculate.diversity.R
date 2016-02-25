#' Calculate Diversity
#' 
#' The function \code{diversity} calculates the diversity of a supercommunity 
#' or series of subcommunities inclusive of similarity. 
#' 
#' The argument \code{measure} takes the following inputs: \cr
#' \tabular{ll}{
#' \code{subcommunity.alpha} \tab - estimate of naive-community supercommunity diversity  \cr
#' \code{subcommunity.alpha.bar} \tab - similarity-sensitive diversity of subcommunity \emph{j} in isolation \cr
#' \code{subcommunity.rho} \tab - redundancy of subcommunity \emph{j} \cr
#' \code{subcommunity.rho.bar} \tab - representativeness of subcommunity \emph{j}  \cr
#' \code{subcommunity.beta} \tab - distinctiveness of subcommunity \emph{j} \cr
#' \code{subcommunity.beta.bar} \tab - estimate of effective number of distinct subcommunities  \cr
#' \code{subcommunity.gamma} \tab - contribution per individual toward supercommunity diversity \cr
#' \code{supercommunity.A} \tab - naive-community supercommunity diversity \cr
#' \code{supercommunity.A.bar} \tab - average similarity-sensitive diversity of subcommunities \cr
#' \code{supercommunity.R} for \tab - average redundancy of subcommunities \cr
#' \code{supercommunity.R.bar} \tab - average representativeness of subcommunities \cr
#' \code{supercommunity.B} \tab - average distinctiveness of subcommunities \cr
#' \code{supercommunity.B.bar} \tab - effective number of distinct subcommunities  \cr
#' \code{supercommunity.G} \tab - supercommunity similarity-sensitive diversity \cr
#' }
#' 
#' @param measure diversity measure (see Details)
#' @param pmatrix matrix of population counts or proportions. 
#' These are defined as a series of columns representing independent subcommunity counts
#' @param qs vector of \emph{q} values. These are defined as the order of 
#' diversity / parameter of conservativeness.
#' @param zmatrix matrix of similarities between 'types' 
#'
#' @return A list of length = length(measure), where each list item contains 
#' the diversity output for the corresponding measure
#' @export
#' 
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
