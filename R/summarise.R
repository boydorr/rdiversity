summarise <-
function(populations, normalise = T)
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
