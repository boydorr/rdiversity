#' @title Calculate Diversity
#' 
#' @description \code{diversity()} calculates the diversity of a  
#' supercommunity or series of subcommunities, inclusive of pair-wise 
#' similarity (whether naive, taxonomic, phenotypic, genetic, phylogenetic, 
#' functional, \emph{etc.}) between individuals. 
#' 
#' @param measure function or list of functions; see Details, below
#' @param data object of class \code{supercommunity}; containing pmatrix and 
#' zmatrix
#' @param qs \code{vector} of mode \code{numeric}; giving \emph{q} values
#' 
#' @details 
#' There are three common ways to input the \code{measure} argument:
#' 
#' \enumerate{
#' \item As a single function, \emph{e.g.} \code{diversity(subcommunity.alpha.bar, 0:2)}
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
#' \item As a list of functions, \emph{e.g.} 
#' \code{diversity(c(subcommunity.alpha.bar, supercommunity.R), 0:2)} 
#' \item As a string, \emph{e.g.} \code{diversity("subcommunity", 0:2)}
#' \tabular{ll}{
#' \code{"subMeasures"} \tab - all subcommunity measures of similarity-sensitive 
#' diversity \cr
#' \code{"superMeasures"} \tab - all supercommunity measures of 
#' similarity-sensitive 
#' diversity \cr
#' \code{"allMeasures"} \tab - all subcommunity and supercommunity measures of 
#' similarity-sensitive diversity \cr
#' }
#' }
#' 
#' @return A list is returned of length equal to the number of diversity 
#' measures calculated. Each list item contains an array of diversities. 
#' Each row corresponds to a different subcommunity and each column represents 
#' a particular value of \emph{q}. 
#' 
#' @export
#'  
#' @examples 
#' # Create population
#' population <- data.frame(A = sample(1:50, 5, replace = TRUE),
#'                          B = sample(1:50, 5, replace = TRUE))
#' 
#' # Coerse object into a collection
#' data <- as.collection(population)
#' 
#' # Calculate diversity
#' # Single measure
#' output <- diversity(subcommunity.alpha.bar, data, 0:2)
#' 
diversity <-
  function(measure, data, qs) 
  {
    if(is.function(measure)) {
      measure(data, qs)
    }else if(is.character(measure)) {
      if(measure=="subcommunity") {
        subMeasures(data, qs)
      }else if(measure=="supercommunity") {
        superMeasures(data, qs)
      }else if(measure=="allMeasures") {
        allMeasures(data, qs)
      }
    }else if(is.list(measure)){
      selectMeasures(measure, data, qs)
    }else stop('Argument measure is unknown.')
  }


#' @rdname diversity
#' @examples 
#' # A number of specific measures 
#' output <- diversity(c(subcommunity.alpha.bar, supercommunity.G), data, 0:2)
#' names(output)
#' output
#' 
selectMeasures <- function(measure, data, qs) {
  lapply(measure, function(x) ans <- x(data, qs))
}


#' @rdname diversity
#' @examples 
#' # All subcommunity measures
#' output <- diversity("subMeasures", data, 0:2)
#' names(output)
#' output[[2]]
#' 
subMeasures <- function(data, qs)
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
#' @examples 
#' # All supercommunity measures 
#' output <- diversity("superMeasures", data, 0:2)
#' names(output)
#' output[[2]]
#' 
superMeasures <- function(data, qs)
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
#' @examples 
#' # All subcommunity and supercommunity measures 
#' output <- diversity("allMeasures", data, 0:2)
#' names(output)
#' output[[2]]
#' 
allMeasures <- function(data, qs)
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

