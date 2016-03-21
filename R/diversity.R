#' @title Calculate Diversity
#' 
#' @description \code{diversity()} calculates the diversity of a  
#' supercommunity or series of subcommunities, inclusive of pair-wise 
#' similarity (whether naive, taxonomic, phenotypic, genetic, phylogenetic, 
#' functional, \emph{etc.}) between individuals. 
#' 
#' @param measure function or list of functions; see Details, below
#' @param data object of class \code{initDiv} 
#' @param qs object of class \code{numeric}; vector of \emph{q} values
#' 
#' @details There are three common ways to use \code{diversity()}:
#' \itemize{
#' \item \code{diversity(subcommunity.alpha.bar, 0:2)} 
#' \item \code{diversity(all.subcommunity, 0:2)} 
#' \item \code{diversity(c(subcommunity.alpha.bar, supercommunity.R), 0:2)} 
#' } 
#' The first method will calculate a single diversity measure from the list 
#' below; the second method will calculate all subcommunity measures of  
#' diversity (alternatively we can calculate \code{all.supercommunity} measures
#' of diversity or \code{all.measures}); and the last method will calculate a 
#' number of specific measures of diversity. \cr
#' 
#' The argument \code{measure} takes the following inputs:
#' \tabular{ll}{
#' \code{subcommunity.alpha} \tab - estimate of naive-community supercommunity 
#' diversity \cr
#' \code{subcommunity.alpha.bar} \tab - similarity-sensitive diversity of 
#' subcommunity \emph{j} in isolation \cr
#' \code{subcommunity.rho} \tab - redundancy of subcommunity \emph{j} \cr
#' \code{subcommunity.rho.bar} \tab - representativeness of subcommunity 
#' \emph{j} \cr
#' \code{subcommunity.beta} \tab - distinctiveness of subcommunity \emph{j} \cr
#' \code{subcommunity.beta.bar} \tab - estimate of effective number of distinct 
#' subcommunities \cr
#' \code{subcommunity.gamma} \tab - contribution per individual toward 
#' supercommunity diversity \cr
#' \code{supercommunity.A} \tab - naive-community supercommunity diversity \cr
#' \code{supercommunity.A.bar} \tab - average similarity-sensitive diversity of 
#' subcommunities \cr
#' \code{supercommunity.R} for \tab - average redundancy of subcommunities \cr
#' \code{supercommunity.R.bar} \tab - average representativeness of 
#' subcommunities \cr
#' \code{supercommunity.B} \tab - average distinctiveness of subcommunities \cr
#' \code{supercommunity.B.bar} \tab - effective number of distinct 
#' subcommunities \cr
#' \code{supercommunity.G} \tab - supercommunity similarity-sensitive 
#' diversity \cr
#' }
#'
#' @return A list is returned; where each element contains an array of 
#' diversities, with the first dimension representing subcommunities and the 
#' last representing values of \emph{q}. 
#' 
#' @examples 
#' # Create population
#' population <- data.frame(A = sample(1:50, 5, replace = TRUE),
#'                          B = sample(1:50, 5, replace = TRUE))
#' 
#' # Create object of class initDiv, an S4 class with two slots: 
#' # .Data - proportional abundances; and zmatrix - pairwise similarities
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' output <- diversity(c(subcommunity.alpha.bar, supercommunity.R), data, 0:2)
#' output[[1]]
#' 
#' # Calculate diversity
#' output <- diversity(all.subcommunity, data, 0:2)
#' names(output)
#' output[[2]]
#' 
#' @export
#' 
diversity <-
  function(measure, data, qs) 
  {
    if(is.function(measure)) {
      measure(data, qs)
    } else 
      some.diversity(measure, data, qs)
  }


#' @rdname diversity 
#' @usage div.all(data, qs) # Calculates all subcommunity and supercommunity 
#' # measures of diversity, returning results in a list 
#' 
all.measures <- function(data, qs)
{
  measure <- list(subcommunity.alpha, subcommunity.alpha.bar, 
                     subcommunity.beta, subcommunity.beta.bar, 
                     subcommunity.rho, subcommunity.rho.bar,
                     subcommunity.gamma, subcommunity.gamma.bar, 
                     supercommunity.A, supercommunity.A.bar,
                     supercommunity.B, supercommunity.B.bar,
                     supercommunity.R, supercommunity.R.bar,
                     supercommunity.G, supercommunity.G.bar)
  
  output <- lapply(measure, function(x) ans <- x(data, qs))
  colnames(output) <- c("subcommunity.alpha", "subcommunity.alpha.bar", 
                        "subcommunity.beta", "subcommunity.beta.bar", 
                        "subcommunity.rho", "subcommunity.rho.bar",
                        "subcommunity.gamma", "subcommunity.gamma.bar", 
                        "supercommunity.A", "supercommunity.A.bar",
                        "supercommunity.B", "supercommunity.B.bar",
                        "supercommunity.R", "supercommunity.R.bar",
                        "supercommunity.G", "supercommunity.G.bar")
  return(output)
}


#' @rdname diversity 
#' @usage div.sub(data, qs) # Calculates all subcommunity measures of 
#' # diversity, returning results in a list 
#' 
all.subcommunity <- function(data, qs)
{
  measure <- list(subcommunity.alpha, subcommunity.alpha.bar, 
                     subcommunity.beta, subcommunity.beta.bar, 
                     subcommunity.rho, subcommunity.rho.bar,
                     subcommunity.gamma, subcommunity.gamma.bar)
  
  output <- lapply(measure, function(x) ans <- x(data, qs))
  names(output) <- c("subcommunity.alpha", "subcommunity.alpha.bar", 
                     "subcommunity.beta", "subcommunity.beta.bar", 
                     "subcommunity.rho", "subcommunity.rho.bar",
                     "subcommunity.gamma", "subcommunity.gamma.bar")
  return(output)
}


#' @rdname diversity 
#' @usage div.super(data, qs) # Calculates all supercommunity measures of 
#' # diversity, returning results in a list 
#' 
all.supercommunity <- function(data, qs)
{
  measure <- list(supercommunity.A, supercommunity.A.bar,
                     supercommunity.B, supercommunity.B.bar,
                     supercommunity.R, supercommunity.R.bar,
                     supercommunity.G, supercommunity.G.bar)
  
  output <- lapply(measure, function(x) ans <- x(data, qs))
  names(output) <- c("supercommunity.A", "supercommunity.A.bar",
                     "supercommunity.B", "supercommunity.B.bar",
                     "supercommunity.R", "supercommunity.R.bar",
                     "supercommunity.G", "supercommunity.G.bar")
  return(output)
}


#' @rdname diversity 
#' @usage calculate.diversity(measure, data, qs) # Calculates multiple measures 
#' # of diversity, returning results in a list 
#' @examples 
#' output <- calculate.diversity(c(subcommunity.alpha.bar, supercommunity.A.bar), data, 0:2)
#' output[[1]]
#' @export
#' 
some.diversity <- function(measure, data, qs) {
  lapply(measure, function(x) ans <- x(data, qs))
}



