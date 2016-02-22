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
