#' Similarity-sensitive Raw subcommunity.alpha
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of qs
#'
#' @param populations Population counts or proportions
#' @param qs Vector of values of parameter q
#' @param Z Similarity matrix
#' @param normalise Normalise probability distribution to sum to 1
#'
#' @return An array of diversities, first dimension representing 
#' sub-communities, and last representing values of q
#' 
subcommunity.alpha <-
function(populations, qs, Z = diag(nrow(populations)), normalise = F)
{
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  # If it's a dataframe make it a matrix
  isdf <- is.data.frame(populations)
  if (isdf)
    populations <- as.matrix(populations)
  
  # Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  # multiply by Z to get Zp.j
  Zp.j <- Z %*% data$proportions
  
  # Now mark all of the species that have nothing similar as NaNs
  # because diversity of an empty group is undefined
  Zp.j[Zp.j==0] <- NaN
  
  # Calculate diversities
  res <- mapply(qDZ.single,
                proportions = as.list(as.data.frame(data$proportions)), # Will repeat length(qs) times
                q = as.list(rep(qs, rep(data$num, length(qs)))),
                Zp = as.list(as.data.frame(Zp.j)), # Will repeat length(qs) times
                MoreArgs = list(Z = Z))
  
  # Restore dimensions and names of original population array,
  # removing species and adding qs
  d.n <- dimnames(populations)
  if (is.null(d.n[[2]]))
  {
    d.n <- list()
    d.n[[1]] <- paste("sc", 1:dim(populations)[2], sep="") 
    d.n[[2]] <- paste("q", qs, sep=".")
  } else
      d.n[[1]] <- d.n[[2]]
  d.n[[2]] <- paste("q", qs, sep="")
  
  res <- array(res, dim = c(data$num, length(qs)), dimnames = d.n)
  if (isdf)
    res <- as.data.frame(res)
  
  attr(res, 'measure') <- 'Subcommunity alpha'
  attr(res, 'tag') <- bquote('Subcommunity' ~ alpha)
  attr(res, 'type') <- 'subcommunity'
  return(res)
}