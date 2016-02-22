subcommunity.gamma <-
structure(function(populations, qs, Z = diag(nrow(populations)),
                               normalise = F)
{
  ## If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  ## If it's a dataframe make it a matrix
  isdf <- is.data.frame(populations)
  if (isdf)
    populations <- as.matrix(populations)
  
  ## Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  ## Turn all columns into proportions if needed, and multiply by Z
  Zp <- Z %*% data$totals
  
  ## Now mark all of the species that have nothing similar as NaNs
  ## because diversity of an empty group is undefined
  Zp[Zp==0] <- NaN
  
  ## Calculate diversities
  res <- mapply(qDZ.single,
                proportions = as.list(as.data.frame(data$proportions)), # Will repeat length(qs) times
                q = as.list(rep(qs, rep(data$num, length(qs)))),
                MoreArgs = list(Z = Z, Zp = Zp))
  
  ## Restore dimensions and names of original population array,
  ## removing species and adding qs
  d.n <- dimnames(populations)
  if (is.null(d.n[[2]]))
  {
    d.n <- list()
    d.n[[1]] <- paste("sc", 1:dim(populations)[2], sep="") 
    d.n[[2]] <- paste("q", qs, sep=".")
  } else
      d.n[[1]] <- d.n[[2]]
  d.n[[2]] <- paste("q", qs, sep="")
  
  res <- array(res, dim = c(dim(populations)[-1], length(qs)), dimnames = d.n)
  if (isdf)
    res <- as.data.frame(res)
  res
}, class = "diversity", name = "subcommunity.gamma")
