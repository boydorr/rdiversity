#' Diversity Profile
#' 
#' Calculates and plots the diversity profile of a population.
#' 
#' @export 
#' 
profile <- function(measure, pmatrix, qs, Z = diag(nrow(pmatrix))) 
{
  ans <- diversity(measure, pmatrix, qs, Z)
  plot(ans)
  
}