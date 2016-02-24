supercommunity.B <-
structure(function(populations, qs, Z = diag(nrow(populations)),
                        normalise = F)
{
  ## If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  ## Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  ## Turn all columns into proportions if needed
  ds <- subcommunity.beta(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
}, class = "diversity", name = "supercommunity.B")
