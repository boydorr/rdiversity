#' Hill number / naive diversity with no similarity 
#' 
#' Calculates the diversity of a series of columns representing independent
#' populations, for a series of orders, repesented as a vector of \code{qs}.
#'
#' @param populations - population counts or proportions.
#' @param  qs - vector of values of parameter \emph{q}.
#' 
#' @details \code{qD} is used to calculate the diversity of a population
#' (in the naive-type case). 
#' 
#' @return Returns an object of class \code{data.frame} with columns as
#' populations, rows as values of \emph{q}, and elements containing diversities.
#' @export
#' 
qD <-
function(populations, qs)
{
  # If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  # If it's a dataframe make it a matrix
  isdf <- is.data.frame(populations)
  if (isdf)
    populations <- as.matrix(populations)
  
  # If populations are input as proportions, check that they sum to 1
  if (any(populations > 0 & populations < 1)) {
      if (!isTRUE(all.equal(apply(populations, 2, sum), rep(1, ncol(populations))))) {
          stop('populations (argument) must be entered as either: a set of integers or a set of proportions which sum to 1.')
      }}
  
  # Turn all columns into proportions, and then into separate
  # elements of a list
  props <- lapply(as.list(as.data.frame(populations)), function(x) x / sum(x))
  
  # Calculate diversities
  res <- mapply(qD_single,
                proportions=props, # Will repeat length(qs) times
                q=rep(qs, rep(length(props), length(qs))))

  # Restore dimensions and names of original population array,
  # removing species and adding qs
  d.n <- dimnames(populations)
  
  # Check for presence of column names (sample / population names)
  if (is.null(d.n[[2]]))
  {
    d.n <- list()
    d.n[[1]] <- paste("sc", 1:dim(populations)[2], sep="") 
    d.n[[2]] <- paste("q", qs, sep="")
  } else
      d.n[[1]] <- d.n[[2]]
  d.n[[2]] <- paste("q", qs, sep="")
  
  res <- array(res, dim=c(dim(populations)[-1], length(qs)), dimnames=d.n)
  if (isdf)
    res <- as.data.frame(res)
  res
}
