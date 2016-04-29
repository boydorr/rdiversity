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
qDZ <- subcommunity.alpha.bar