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
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace=T),
#'                         subcommunityB = sample(1:50, 5, replace=T))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' output <- diversity(subcommunity.alpha.bar, data, 0:2)
#' output
#' 
#' output <- diversity(subcommunity, data, qs)
#' names(output)
#' output[[2]]
#' 
#' output <- calculate.diversity(c(subcommunity.alpha.bar, supercommunity.A.bar), data, qs)
#' output[[1]]
#' 
diversity <-
  function(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix))) 
  {
    if(deparse(substitute(measure))=='all') {
      div.all(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
    }else if(deparse(substitute(measure))=='subcommunity') {
      div.sub(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
    }else if(deparse(substitute(measure))=='supercommunity') {
      div.super(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
    }else if(length(measure)==1 & class(measure)=='function') {
      output <- measure(pmatrix, qs, zmatrix)
    }else 
      output <- calculate.diversity(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
  }


div.all <- function(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
{
  measure <- list(subcommunity.alpha, subcommunity.alpha.bar, 
                     subcommunity.beta, subcommunity.beta.bar, 
                     subcommunity.rho, subcommunity.rho.bar,
                     subcommunity.gamma, subcommunity.gamma.bar, 
                     supercommunity.A, supercommunity.A.bar,
                     supercommunity.B, supercommunity.B.bar,
                     supercommunity.R, supercommunity.R.bar,
                     supercommunity.G, supercommunity.G.bar)
  
  output <- lapply(measure, function(x) ans <- x(pmatrix, qs, zmatrix))
}


div.sub <- function(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
{
  measure <- list(subcommunity.alpha, subcommunity.alpha.bar, 
                     subcommunity.beta, subcommunity.beta.bar, 
                     subcommunity.rho, subcommunity.rho.bar,
                     subcommunity.gamma, subcommunity.gamma.bar)
  
  output <- lapply(measure, function(x) ans <- x(pmatrix, qs, zmatrix))
}


div.super <- function(pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
{
  measure <- list(supercommunity.A, supercommunity.A.bar,
                     supercommunity.B, supercommunity.B.bar,
                     supercommunity.R, supercommunity.R.bar,
                     supercommunity.G, supercommunity.G.bar)
  
  output <- lapply(measure, function(x) ans <- x(pmatrix, qs, zmatrix))
}


calculate.diversity <- function(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix))) {
  output <- lapply(measure, function(x) ans <- x(pmatrix, qs, zmatrix))
}



