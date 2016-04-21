#' Calculate subcommunity diversity
#' 
#' Calculates subcommunity diversity by taking the powermean of diversity 
#' terms.
#' 
#' @name subdiv
#' @param data two-dimensional \code{matrix} of mode \code{numeric}; diversity 
#' terms.
#' @param qs \code{vector} of mode \code{numeric}; parameter of conservatism.
#' 
#' @details as ds
#' 
#' @return Returns a two-dimensional \code{matrix} of mode \code{numeric}.
#' @export 
#' @examples as
#' 
setGeneric(name = "subdiv",
           def = function(data, qs) {
             standardGeneric("subdiv")
           } )

#' @rdname subdiv
#' 
setMethod(f = "subdiv", signature= "powermean", 
          definition = function(data, qs) {
            results <- list()
            for(i in 1:length(qs))
              results[[i]] <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], 1-qs[i], data@type_weights[,y]))
            
            results <- data.frame(matrix(unlist(results), 
                                         nrow = length(results), 
                                         byrow=T), 
                                  stringsAsFactors=FALSE)
            colnames(results) <- colnames(data)
            row.names(results) <- paste0("q", qs) 
            return(results)
            } )
