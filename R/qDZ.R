qDZ <-
function(populations, qs, Z = diag(nrow(populations))) {
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
