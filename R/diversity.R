#' @name diversity
#' 
#' @title Calculate Diversity
#' 
#' @description The function \code{diversity} calculates the diversity of a supercommunity 
#' or series of subcommunities inclusive of similarity. 
#' 
#' @param measure diversity measure (see Details)
#' @param data object of class \code{initDiv}
#' These are defined as a series of columns representing independent subcommunity counts
#' @param qs vector of \emph{q} values. These are defined as the order of 
#' diversity / parameter of conservativeness.
#' 
#' @details The argument \code{measure} takes the following inputs: \cr
#' \tabular{ll}{
#' \code{subcommunity.alpha} \tab - estimate of naive-community supercommunity diversity\cr
#' \code{subcommunity.alpha.bar} \tab - similarity-sensitive diversity of subcommunity \emph{j} in isolation \cr
#' \code{subcommunity.rho} \tab - redundancy of subcommunity \emph{j} \cr
#' \code{subcommunity.rho.bar} \tab - representativeness of subcommunity \emph{j} \cr
#' \code{subcommunity.beta} \tab - distinctiveness of subcommunity \emph{j} \cr
#' \code{subcommunity.beta.bar} \tab - estimate of effective number of distinct subcommunities \cr
#' \code{subcommunity.gamma} \tab - contribution per individual toward supercommunity diversity \cr
#' \code{supercommunity.A} \tab - naive-community supercommunity diversity \cr
#' \code{supercommunity.A.bar} \tab - average similarity-sensitive diversity of subcommunities \cr
#' \code{supercommunity.R} for \tab - average redundancy of subcommunities \cr
#' \code{supercommunity.R.bar} \tab - average representativeness of subcommunities \cr
#' \code{supercommunity.B} \tab - average distinctiveness of subcommunities \cr
#' \code{supercommunity.B.bar} \tab - effective number of distinct subcommunities \cr
#' \code{supercommunity.G} \tab - supercommunity similarity-sensitive diversity \cr
#' }
#'
#' @return A list of length = length(measure), where each list item contains 
#' the diversity output for the corresponding measure
#' 
#' @examples 
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace=TRUE),
#'                         subcommunityB = sample(1:50, 5, replace=TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Create object of class initDiv
#' data <- set.collection(population)
#' 
#' # Calculate diversity
#' output <- diversity(subcommunity.alpha.bar, data, 0:2)
#' output
#' 
#' output <- diversity(subcommunity, data, 0:2)
#' names(output)
#' output[[2]]
#' 
#' @rdname diversity
#' @export
#' 
diversity <-
  function(measure, data, qs) 
  {
    if(deparse(substitute(measure))=='all') {
      div.all(data, qs)
    }else if(deparse(substitute(measure))=='subcommunity') {
      div.sub(data, qs)
    }else if(deparse(substitute(measure))=='supercommunity') {
      div.super(data, qs)
    }else if(length(measure)==1 & class(measure)=='function') {
      measure(data, qs)
    }else 
      calculate.diversity(measure, data, qs)
  }


#' @rdname diversity 
#' @usage div.all(data, qs) # Calculates all subcommunity and supercommunity 
#' # measures of diversity, returning results in a list 
#' 
div.all <- function(data, qs)
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
div.sub <- function(data, qs)
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
div.super <- function(data, qs)
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
calculate.diversity <- function(measure, data, qs) {
  lapply(measure, function(x) ans <- x(data, qs))
}



