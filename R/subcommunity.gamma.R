#' Similarity-sensitive Raw subcommunity.gamma diversity
#' 
#' \code{subcommunity.gamma()} is used to calculate the 
#' contribution per individual toward supercommunity diversity.
#' 
#' \code{subcommunity.gamma()} calculates the subcommunity gamma diversity of 
#' a series of columns representing independent subcommunities counts relative 
#' to a total supercommunity (by default the sum of the subcommunities), for 
#' a series of orders, repesented as a vector of \code{qs}.
#'
#' @inheritParams subcommunity.alpha.bar
#' @param normalise logical operator; T returns normalised probability 
#' distribution summed to 1
#' 
#' @return Data frame of diversities, columns representing populations, and
#' rows representing values of \emph{q}
#' @seealso \code{\link{diversity}}
#' @export
#' 
#' @examples
#' # Species counts
#' population <- data.frame(subcommunityA = sample(1:50, 5, replace = TRUE),
#'                         subcommunityB = sample(1:50, 5, replace = TRUE))
#' row.names(population) <- c('cows', 'sheep', 'ducks', 'foxes', 'bears')
#' 
#' # Coerse object into a supercommunity
#' data <- supercommunity(population)
#' 
#' # Calculate diversity
#' subcommunity.gamma(data, 0:2)
#' 
subcommunity.gamma <-
  function(populations, qs, normalise = F)
  {
    if(!is.supercommunity(populations))
      stop('populations must be object of class supercommunity.')
    
    Z = populations@similarity
    # If we just have a single vector, then turn it into single column matrix
    if (is.vector(populations))
      populations <- array(populations, dim=c(length(populations), 1))
    
    # If it's a dataframe make it a matrix
    isdf <- is.data.frame(populations)
    if (isdf)
      populations <- as.matrix(populations)
    
    # Turn all columns into proportions if needed
    data <- summarise(populations, normalise)
    
    # Turn all columns into proportions if needed, and multiply by Z
    Zp <- Z %*% data$totals
    
    # Now mark all of the species that have nothing similar as NaNs
    # because diversity of an empty group is undefined
    Zp[Zp==0] <- NaN
    
    # Calculate diversities
    res <- mapply(qDZ.single,
                  proportions = as.list(as.data.frame(data$proportions)), # Will repeat length(qs) times
                  q = as.list(rep(qs, rep(data$num, length(qs)))),
                  MoreArgs = list(Z = Z, Zp = Zp))
    
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
    
    res <- array(res, dim = c(dim(populations)[-1], length(qs)), dimnames = d.n)
    # if (isdf)
    res <- as.data.frame(res)
    
    output <- new('rdiv', res, measure = 'Subcommunity gamma',
                  tag = bquote('Subcommunity' ~ gamma),
                  level = 'subcommunity')
    output
  }