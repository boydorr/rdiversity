#' Summary function
#' 
#' This function converts columns of an array (each representing population 
#' counts) into proportions, so that each column sums to 1.
#'
#' @param populations An S x N array whose columns are counts of individuals.
#' @param normalise Normalise probability distribution to sum to 1 for each 
#' column rather than just along each set.
#' 
#' @return Returns an array whose columns are proportions.
#' 
summarise <-
  function(populations, normalise = TRUE)
  {
    totals <- array(rowSums(populations), dim=c(dim(populations)[1], 1))
    
    if (normalise)
    {
      total <- sum(totals)
      totals <- totals / total
      proportions <- populations / total
      weights <- colSums(proportions)
      proportions <- proportions %*% diag(1/(weights))
    } else {
      proportions <- populations
      weights <- colSums(proportions)
    }
    num <- length(weights)
    
    list(proportions=proportions, totals=totals,
         weights=array(weights, dim=c(1, num)), num=num)
  }
