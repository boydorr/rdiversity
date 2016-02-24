#' additive.to.diversity 
#' 
#' Converts 'additive diversity' values (for any q) to diversities.
#' 
#' @param additive Dataframe comprising 'additive diversity' values, whereby 
#' each row and column corresponds to a particular subcommunity and q value, 
#' respectively; q values should be input in column names as character 
#' strings, e.g. "q1".
#' 
#' @return Dataframe comprising diversity values, whereby each row and column 
#' corresponds to a particular subcommunity and q value, respectively
#' 
additive.to.diversity <-
function(additive) {
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
