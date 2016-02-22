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
