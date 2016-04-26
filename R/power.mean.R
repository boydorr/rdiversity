#' Power mean of vector elements
#' 
#' \code{power.mean()} calculates the power mean of a set of values.
#' 
#' @param values Values for which to calculate mean
#' @param order Order of power mean
#' @param weights Weights of elements, normalised to 1 inside function
#'
#' @details \code{power.mean()} calculates the order-th power mean of a single 
#' set of non-negative values, weighted by weights; by default, weights are 
#' equal and order is 1, so this is just the arithmetic mean. Equal weights 
#' and a order of 0 gives the geometric mean, and an order of -1 gives the 
#' harmonic mean.
#' 
#' @return Weighted power mean
#' @export
#' 
#' @examples 
#' values <- rnorm(5)
#' power.mean(values)
#' 
power.mean <-
function(values, order = 1, weights = rep(1, length(values)))
{
  # Check that the number of 'values' is equal to the number of 'weights'
  if (length(values) != length(weights))
    stop('The number of values does not equal the number of weights, please check arguments')
  
  # Check that 'values' and 'weights' are non-negative
  if (any(values < 0, na.rm=TRUE) || any(weights < 0, na.rm=TRUE))
      stop('Check that values and weights (arguments) are non-negative.')
  
  # Normalise weights to sum to 1 (as per Rényi)
  proportions <- weights / sum(weights)
  
  # Check whether all proportions are NaN - happens in normalisation when all
  # weights are zero in group. In that case we want to propagate the NaN
  if (all(is.nan(proportions)))
    return(NaN)
  
  # Otherwise NaNs should only occur (in values) when weight is 0 and so will be
  # stripped out here as we have to eliminate non-zero weights
  non.zero <- weights > 0
  values <- values[non.zero]
  proportions <- proportions[non.zero]
  
  if (abs(order) < .Machine$double.eps ^ 0.5) {  # Avoid rounding errors for order 0
      prod(values ^ proportions, na.rm = T)
    } else if (is.infinite(order)) {
      if (order > 0)
        max(values, na.rm = T)
      else
        min(values, na.rm = T)
    } else {
      sum(proportions * values ^ order, na.rm = T) ^ (1 / order)
    }
}
