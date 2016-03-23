#' Power mean
#' 
#' Calculates the order-th power mean of a single set of values, weighted by
#' weights; by default, weights are equal and order is 1, so this is just the 
#' arithmetic mean. Equal weights and a order of 0 gives the geometric mean,
#' and an order of -1 gives the harmonic mean.
#'
#' @param values Values for which to calculate mean
#' @param order Order of power mean
#' @param weights Weights of elements, normalised to 1 inside function
#'
#' @return Weighted power mean
#' 
power.mean <-
function(values, order = 1, weights = rep(1, length(values)))
{
  # Normalise weights to sum to 1 (as per Rényi)
  proportions <- weights / sum(weights)
  
  # Check that the number of 'values' is equal to the number of 'weights'
  if (length(values) != length(weights))
    stop('The number of values does not equal the number of weights, please check arguments')
  
  # Check that 'values' are non-negative
  if (any(values[!is.nan(values)] < 0))
      stop('Check that values (argument) are non-negative.')
  
  # Check whether all proportions are NaN - happens when nothing in group
  # In that case we want to propagate the NaN
  if (all(is.nan(proportions)))
    return(NaN)
  
  # Otherwise NaNs should only occur when weight is 0
  # and so will be ignored
  if (order > 0) {
    if (is.infinite(order)) {
      max(values[weights > 0])
    } else if (isTRUE(all.equal(order, 0))) {  # Avoid rounding errors for order 0
      prod(values[weights > 0] ^ proportions[weights > 0])
    } else {
      sum(proportions[weights > 0] * values[weights > 0] ^ order) ^ (1 / order)
    }
  } else { # Negative orders, need to remove zeros
    if (is.infinite(order)) {
      min(values[weights > 0])
    } else if (isTRUE(all.equal(order, 0))) {  # Avoid rounding errors for order 0
      prod(values[weights > 0] ^ proportions[weights > 0])
    } else {
      sum(proportions[weights > 0] * values[weights > 0] ^ order) ^ (1 / order)
    }
  }
}
