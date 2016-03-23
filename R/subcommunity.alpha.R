#' Similarity-sensitive Raw subcommunity.alpha
#' 
#' Calculates the diversity of a series of columns representing
#' independent subcommunity counts, for a series of orders, repesented as
#' a vector of \code{qs}.
#'
#' @inheritParams inherit_params
#' @param normalise logical operator; T returns normalised probability 
#' distribution summed to 1
#' @return Returns an array of diversities, first dimension representing 
#' subcommunities, and last representing values of \emph{q}
#' 
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Coerse object into a collection
#' data <- as.collection(population)
#' 
#' # Calculate diversity
#' subcommunity.alpha(data, 0:2)
#' 
subcommunity.alpha <-
function(populations, qs, normalise = F)
{
  Z = populations@zmatrix
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
                proportions = as.list(as.data.frame(data$proportions)), 
                q = as.list(rep(qs, rep(data$num, length(qs)))),
                Zp = as.list(as.data.frame(Zp.j)), 
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
  # if (isdf)
    res <- as.data.frame(res)
  
  output <- new('rdiv', res, measure = 'Subcommunity alpha',
            tag = bquote('Subcommunity' ~ alpha),
            level = 'subcommunity')
  return(output)
}