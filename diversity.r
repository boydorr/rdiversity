### Functions to calculate diversities of populations, with or without 
### similarities

## summarise - Summary function
## Converts columns of an array (each representing population counts) into
## proportions, so that each column sums to 1
##
## Arguments:
## - populations - S x N array whose columns are counts of individuals
## - normalise - normalise probability distribution to sum to 1 for
##   each column rather than just along each set
##
## Returns:
## - array whose columns are proportions
summarise <- function(populations, normalise = T)
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

## power.mean - Power mean
## Calculates the order-th power mean of a single set of values, weighted by
## weights; by default, weights are equal and order is 1, so this is just the 
## arithmetic mean
##
## Arguments:
## - values - values for which to calculate mean
## - order - order of power mean
## - weights - weights of elements, normalised to 1 inside function
##
## Returns:
## - weighted power mean
power.mean <- function(values, order = 1, weights = rep(1, length(values)))
{
  ## Normalise weights to sum to 1 (as per Rényi)
  proportions <- weights / sum(weights)
  
  ## Check that the number of 'values' is equal to the number of 'weights'
  if (length(values) != length(weights)) stop('The number of values does not equal the number of weights, please check arguments')
  
  ## Check that 'values' are non-negative
  if (any(values[!is.nan(values)] < 0))
      stop('Check that values (argument) are non-negative.')
  
  ## Check whether all proportions are NaN - happens when nothing in group
  ## In that case we want to propagate the NaN
  if (all(is.nan(proportions))) return(NaN)
  
  ## Otherwise NaNs should only occur when weight is 0
  ## and so will be ignored
  if (order > 0) {
    if (is.infinite(order)) {
      max(values[weights > 0])
    } else if (isTRUE(all.equal(order, 0))) {  ## Avoid rounding errors for order 0
      prod(values[weights > 0] ^ proportions[weights > 0])
    } else {
      sum(proportions[weights > 0] * values[weights > 0] ^ order) ^ (1 / order)
    }
  } else { ## Negative orders, need to remove zeros
    if (is.infinite(order)) {
      min(values[weights > 0])
    } else if (isTRUE(all.equal(order, 0))) {  ## Avoid rounding errors for order 0
      prod(values[weights > 0] ^ proportions[weights > 0])
    } else {
      sum(proportions[weights > 0] * values[weights > 0] ^ order) ^ (1 / order)
    }
  }
}

## qD.single - Hill number / naive diversity with no similarity of a single population
## Calculates the Hill number / naive diversity of order q of a population
## with given relative proportions
##
## Arguments:
## - proportions - relative proportions of different individuals /
##                 species in population
## - q - order of diversity measurement
qD.single <- function(proportions, q)
  1 / power.mean(values = proportions, order = q - 1, weights = proportions)

## qD - Hill number / naive diversity with no similarity 
## Calculates the diversity of a series of columns representing independent
## populations, for a series of orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
##
## Returns:
## - data frame of diversities, columns representing populations, and
##   rows representing values of q
qD <- function(populations, qs)
{
  ## If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  ## If it's a dataframe make it a matrix
  isdf <- is.data.frame(populations)
  if (isdf)
    populations <- as.matrix(populations)
  
  ## If populations are input as proportions, check that they sum to 1
  if (any(populations > 0 & populations < 1)) {
      if (!isTRUE(all.equal(apply(populations, 2, sum), rep(1, ncol(populations))))) {
          stop('populations (argument) must be entered as either: a set of integers or a set of proportions which sum to 1.')
      }}
  
  ## Turn all columns into proportions, and then into separate
  ## elements of a list
  props <- lapply(as.list(as.data.frame(populations)), function(x) x / sum(x))
  
  ## Calculate diversities
  res <- mapply(qD.single,
                proportions=props, # Will repeat length(qs) times
                q=rep(qs, rep(length(props), length(qs))))
  
  ## Restore dimensions and names of original population array,
  ## removing species and adding qs
  d.n <- dimnames(populations)
  
  ## Check for presence of column names (sample / population names)
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

## qDZ.single - Similarity-sensitive diversity of a single population
## Calculates the similarity-sensitive diversity of order q of a population 
## with given relative proportions
##
## Arguments:
## - proportions - relative proportions of different individuals /
##                 species in population
## - q - order of diversity measurement
## - Z - similarity matrix
## - Zp - ordinariness of individuals / species in population
qDZ.single <- function(proportions, q,
                       Z = diag(nrow(proportions)),
                       Zp = Z %*% proportions)
  1 / power.mean(values = Zp, order = q - 1, weights = proportions)

## qDZ - Similarity-sensitive diversity
## Calculates the diversity of a series of columns representing independent
## populations, for a series of orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
##
## Returns:
## - data frame of diversities, columns representing populations, and
##   rows representing values of q
qDZ <- function(populations, qs, Z = diag(nrow(populations))) {
    ## If we just have a single vector, then turn it into single column matrix
    if (is.vector(populations))
        populations <- array(populations, dim=c(length(populations), 1))
    
    ## If it's a dataframe make it a matrix
    isdf <- is.data.frame(populations)
    if (isdf)
        populations <- as.matrix(populations)
    
    ## If populations are input as proportions, check that they sum to 1
    if (any(populations > 0 & populations < 1)) {
        if (!isTRUE(all.equal(apply(populations, 2, sum), rep(1, ncol(populations))))) {
            stop('populations (argument) must be entered as either: a set of integers or a set of proportions which sum to 1.')
        }}
    
    if (is.vector(populations) & dim(Z)[1] == 0) Z = diag(nrow(matrix((populations))))
    
    subcommunity.alpha.bar(populations = populations, qs = qs, Z = Z)
}

## subcommunity.alpha - Similarity-sensitive subcommunity alpha diversity
## subcommunity.alpha.bar - Normalised subcommunity.alpha
## Calculates the diversity of a series of columns representing
## independent subcommunity counts, for a series of orders, repesented as
## a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - an array of diversities, first dimension representing sub-communities, and
##   last representing values of q
subcommunity.alpha.bar <- function(populations, qs, Z = diag(nrow(populations)))
  subcommunity.alpha(populations, qs, Z, normalise = T)

subcommunity.alpha <- function(populations, qs, Z = diag(nrow(populations)), 
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
  
  ## multiply by Z to get Zp.j
  Zp.j <- Z %*% data$proportions
  
  ## Now mark all of the species that have nothing similar as NaNs
  ## because diversity of an empty group is undefined
  Zp.j[Zp.j==0] <- NaN
  
  ## Calculate diversities
  res <- mapply(qDZ.single,
                proportions = as.list(as.data.frame(data$proportions)), # Will repeat length(qs) times
                q = as.list(rep(qs, rep(data$num, length(qs)))),
                Zp = as.list(as.data.frame(Zp.j)), # Will repeat length(qs) times
                MoreArgs = list(Z = Z))
  
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
  
  res <- array(res, dim = c(data$num, length(qs)), dimnames = d.n)
  if (isdf)
    res <- as.data.frame(res)
  res
}

## supercommunity.A - Similarity-sensitive supercommunity alpha diversity
## supercommunity.A.bar - Normalised supercommunity.A
## Calculates the total supercommunity alpha diversity of a series of columns
## representing subcommunity counts, for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - array of diversities, last representing values of q
supercommunity.A.bar <- function(populations, qs, Z = diag(nrow(populations)))
  supercommunity.A(populations, qs, Z, normalise = T)

supercommunity.A <- function(populations, qs, Z = diag(nrow(populations)),
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
  ds <- subcommunity.alpha(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
}

## subcommunity.gamma - Similarity-sensitive subcommunity gamma diversity
## subcommunity.gamma.bar - Normalised subcommunity.gamma
## Calculates the diversity of a series of columns representing
## independent sub-communities counts relative to a total supercommunity
## (by default the sum of the sub-communities), for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions - single vector or matrix
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - data frame of diversities, columns representing populations, and
##   rows representing values of q
subcommunity.gamma.bar <- function(populations, qs, Z = diag(nrow(populations)), ...)
  subcommunity.gamma(populations, qs, Z, ..., normalise = T)

subcommunity.gamma <- function(populations, qs, Z = diag(nrow(populations)),
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
}

## supercommunity.G - Similarity-sensitive supercommunity gamma diversity
## supercommunity.G.bar - Normalised supercommunity.G
## Calculates the total supercommunity gamma diversity of a series of columns
## representing subcommunity counts, for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - array of diversities, last representing values of q
supercommunity.G.bar <- function(populations, qs, Z = diag(nrow(populations)))
  supercommunity.G(populations, qs, Z, normalise = T)

supercommunity.G <- function(populations, qs, Z = diag(nrow(populations)),
                        normalise = F)
{
  ## If we just have a single vector, then turn it into single column matrix
  if (is.vector(populations))
    populations <- array(populations, dim=c(length(populations), 1))
  
  ## If it's a dataframe make it a matrix
  if (is.data.frame(populations))
    populations <- as.matrix(populations)
  
  ## Turn all columns into proportions if needed
  data <- summarise(populations, normalise)
  
  ## Turn all columns into proportions if needed
  ds <- subcommunity.gamma(populations, qs, Z, normalise)
  
  res <- mapply(power.mean,
                values = as.list(as.data.frame(ds)),
                order = as.list(1 - qs),
                MoreArgs = list(weights = data$weights))
  
  d.n <- list(paste("q", qs, sep=""), "supercommunity")
  array(res, dim = c(length(qs), 1), dimnames = d.n)
}

## subcommunity.beta - Similarity-sensitive subcommunity beta diversity
## subcommunity.beta.bar - Normalised subcommunity.beta
## Calculates the diversity of a series of columns representing
## independent sub-communities counts relative to a total supercommunity
## (by default the sum of the sub-communities), for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions - single vector or matrix
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - data frame of diversities, columns representing populations, and
##   rows representing values of q
subcommunity.beta.bar <- function(populations, qs, Z = diag(nrow(populations)), ...)
  subcommunity.beta(populations, qs, Z, ..., normalise = T)

subcommunity.beta <- function(populations, qs, Z = diag(nrow(populations)),
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
  
  ## multiply by Z to get Zp and Zp.j
  Zp <- Z %*% data$totals %*% t(rep(1, data$num))
  Zp.j <- Z %*% data$proportions
  Zb <- Zp.j / Zp
  
  ## Now mark all of the species that have nothing similar as NaNs
  ## because diversity of an empty group is undefined
  Zb[Zb==0] <- NaN
  
  ## Calculate diversities
  res <- mapply(power.mean,
                values = as.list(as.data.frame(Zb)), # Will repeat length(qs) times
                order = as.list(rep(qs - 1, rep(data$num, length(qs)))),
                weights = as.list(as.data.frame(data$proportions))) # Will repeat length(qs) times
  
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
}

## supercommunity.B - Similarity-sensitive supercommunity beta diversity
## supercommunity.B.bar - Normalised supercommunity.B
## Calculates the total supercommunity beta diversity of a series of columns
## representing subcommunity counts, for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - array of diversities, last representing values of q
supercommunity.B.bar <- function(populations, qs, Z = diag(nrow(populations)))
  supercommunity.B(populations, qs, Z, normalise = T)

supercommunity.B <- function(populations, qs, Z = diag(nrow(populations)),
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
}

## subcommunity.rho - Similarity-sensitive subcommunity rho diversity; 
## the inverse of the similarity-sensitive subcommunity beta diversity
## subcommunity.rho.bar - Normalised subcommunity.rho; the inverse of the 
## normalised similarity-sensitive subcommunity beta diversity
## Calculates the diversity of a series of columns representing
## independent sub-communities counts relative to a total supercommunity
## (by default the sum of the sub-communities), for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions - single vector or matrix
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - data frame of diversities, columns representing populations, and
##   rows representing values of q
subcommunity.rho <- function(populations, qs, Z = diag(nrow(populations)), ...)
    1 / subcommunity.beta(populations, qs, Z, ...)

subcommunity.rho.bar <- function(populations, qs, Z = diag(nrow(populations)), ...)
    1 / subcommunity.beta.bar(populations, qs, Z, ...)

## supercommunity.R - Similarity-sensitive supercommunity R diversity
## supercommunity.R.bar - Normalised supercommunity.R 
## Calculates the total supercommunity R and R.bar diversity of a series of columns
## representing subcommunity counts, for a series of
## orders, repesented as a vector of qs
##
## Arguments:
## - populations - population counts or proportions
## - qs - vector of values of parameter q
## - Z - similarity matrix
## - normalise - normalise probability distribution to sum to 1
##
## Returns:
## - array of diversities, last representing values of q
supercommunity.R.bar <- function(populations, qs, Z = diag(nrow(populations)))
    supercommunity.R(populations, qs, Z, normalise = T)

supercommunity.R <- function(populations, qs, Z = diag(nrow(populations)),
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
    ds <- subcommunity.rho(populations, qs, Z, normalise)
    
    res <- mapply(power.mean,
                  values = as.list(as.data.frame(ds)),
                  order = as.list(1 - qs),
                  MoreArgs = list(weights = data$weights))
    
    d.n <- list(paste("q", qs, sep=""), "supercommunity")
    array(res, dim = c(length(qs), 1), dimnames = d.n)
}



# S3

class(subcommunity.alpha) <- 'diversity'
class(subcommunity.alpha.bar) <- 'diversity'
class(supercommunity.A) <- 'diversity'
class(supercommunity.A.bar) <- 'diversity'
class(subcommunity.beta) <- 'diversity'
class(subcommunity.beta.bar) <- 'diversity'
class(supercommunity.B) <- 'diversity'
class(supercommunity.B.bar) <- 'diversity'
class(subcommunity.rho) <- 'diversity'
class(subcommunity.rho.bar) <- 'diversity'
class(supercommunity.R) <- 'diversity'
class(supercommunity.R.bar) <- 'diversity'
class(subcommunity.gamma) <- 'diversity'
class(subcommunity.gamma.bar) <- 'diversity'
class(supercommunity.G) <- 'diversity'
class(supercommunity.G.bar) <- 'diversity'

attr(subcommunity.alpha, 'name') <- 'subcommunity.alpha'
attr(subcommunity.alpha.bar, 'name') <- 'subcommunity.alpha.bar'
attr(supercommunity.A, 'name') <- 'supercommunity.A'
attr(supercommunity.A.bar, 'name') <- 'supercommunity.A.bar'
attr(subcommunity.beta, 'name') <- 'subcommunity.beta'
attr(subcommunity.beta.bar, 'name') <- 'subcommunity.beta.bar'
attr(supercommunity.B, 'name') <- 'supercommunity.B'
attr(supercommunity.B.bar, 'name') <- 'supercommunity.B.bar'
attr(subcommunity.rho, 'name') <- 'subcommunity.rho'
attr(subcommunity.rho.bar, 'name') <- 'subcommunity.rho.bar'
attr(supercommunity.R, 'name') <- 'supercommunity.R'
attr(supercommunity.R.bar, 'name') <- 'supercommunity.R.bar'
attr(subcommunity.gamma, 'name') <- 'subcommunity.gamma'
attr(subcommunity.gamma.bar, 'name') <- 'subcommunity.gamma.bar'
attr(supercommunity.G, 'name') <- 'supercommunity.G'
attr(supercommunity.G.bar, 'name') <- 'supercommunity.G.bar'


## diversity.to.additive - converts diversity values (for any q) to 'additive diversities'
## 
## Arguments:
## - diversities - dataframe comprising diversity values, whereby each row and
## column corresponds to a particular subcommunity and q value, respectively; 
## q values should be input in column names as character strings, e.g. "q1"
## 
## Returns:
## - dataframe comprising 'additive diversity' values, whereby each row and column 
## corresponds to a particular subcommunity and q value, respectively
diversity.to.additive <- function(diversities) {
    if (!is.data.frame(diversities)) diversities <- as.data.frame(diversities)
    output <- list()
    for (i in 1:ncol(diversities)) {
        this.q <- colnames(diversities)[i]
        q.index <- as.numeric(gsub("q","",this.q))
        this.div <- diversities[,i,drop=F]
        if (this.q=="q0") output[[i]] <- this.div
        else if (this.q=="q1") output[[i]] <- log(this.div)
        else output[[i]] <- (this.div)^(1-q.index)
    }
    output <- do.call(cbind,output)
    return(output)
}

## additive.to.diversity - converts 'additive diversity' values (for any q) to diversities
## 
## Arguments:
## - additive - dataframe comprising 'additive diversity' values, whereby each row and
## column corresponds to a particular subcommunity and q value, respectively; 
## q values should be input in column names as character strings, e.g. "q1"
## 
## Returns:
## - dataframe comprising diversity values, whereby each row and column 
## corresponds to a particular subcommunity and q value, respectively
additive.to.diversity <- function(additive) {
    if (!is.data.frame(additive)) as.data.frame(additive)
    output <- list()
    for (i in 1:ncol(additive)) {
        this.q <- colnames(additive)[i]
        q.index <- as.numeric(gsub("q","",this.q))
        this.div <- additive[,i,drop=F]
        if (this.q=="q0") output[[i]] <- this.div
        else if (this.q=="q1") output[[i]] <- exp(this.div)
        else output[[i]] <- (this.div)^(1/(1-q.index))
    }
    output <- do.call(cbind,output)
    return(output)
}







