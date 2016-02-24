#' diversity.to.additive 
#' 
#' Converts diversity values (for any q) to 'additive diversities'.
#' 
#' @param diversities Dataframe comprising diversity values, whereby each row 
#' and column corresponds to a particular subcommunity and q value, 
#' respectively; q values should be input in column names as character 
#' strings, e.g. "q1"
#' 
#' @return Dataframe comprising 'additive diversity' values, whereby each row
#' and column corresponds to a particular subcommunity and q value, respectively
#' 
diversity.to.additive <-
function(diversities) {
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
