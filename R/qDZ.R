#' Similarity-sensitive diversity
#' 
#' Calculates the similarity-sensitive diversity of a series of columns 
#' representing independent populations, for a series of orders repesented as 
#' a vector of \code{qs}.
#'
#' @param super object of class \code{supercommunity}.
#' @param qs \code{vector} of \emph{q} values.
#' 
#' @include as.supercommunity.R subdiv.R
#' @return Returns a matrix of similarity-sensitive diversities, where rows 
#' and columns represent values of q and subcommunities, respectively.
#' @export
#' 
#' @examples 
#' pop <- sample(1:50, 5)
#' 
#' # Create similarity matrix
#' Z <- diag(1, length(pop))
#' Z[Z==0] <- 0.4
#' 
#' dat <- supercommunity(pop, Z)
#' 
#' # Calculate similarity-sensitive diversity of order 0 (species richness)
#' qDZ(dat, 0)
#' 
qDZ <-
function(populations, qs) {
    # If we just have a single vector, then turn it into single column matrix
    if (is.vector(populations))
        populations <- array(populations, dim=c(length(populations), 1))
    
    # If it's a dataframe make it a matrix
    isdf <- is.data.frame(populations)
    if (isdf)
        populations <- as.matrix(populations)
    
    # # If populations are input as proportions, check that they sum to 1
    # if (any(populations > 0 & populations < 1)) {
    #     if (!isTRUE(all.equal(apply(populations, 2, sum), rep(1, ncol(populations))))) {
    #         stop('populations (argument) must be entered as either: a set of integers or a set of proportions which sum to 1.')
    #     }}
    
    # if (is.vector(populations) & dim(Z)[1] == 0) Z = diag(nrow(matrix((populations))))
    
    subcommunity.alpha.bar(populations = populations, qs = qs)
}
