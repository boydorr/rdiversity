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
            for(i in seq_along(qs))
              results[[i]] <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], 1-qs[i], data@type_weights[,y]))
            results <- data.frame(matrix(unlist(results), 
                                         nrow = length(results), 
                                         byrow = TRUE), 
                                  stringsAsFactors = FALSE)
            colnames(results) <- colnames(data)
            row.names(results) <- paste0("q", qs) 
            results
          } )


#' @rdname subdiv
#' 
setMethod(f = "subdiv", signature= "relativeentropy", 
          definition = function(data, qs) {
            results <- list()
            for(i in seq_along(qs))
              results[[i]] <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], qs[i]-1, data@type_weights[,y]))
            results <- data.frame(matrix(unlist(results), 
                                         nrow = length(results), 
                                         byrow = TRUE), 
                                  stringsAsFactors = FALSE)
            colnames(results) <- colnames(data)
            row.names(results) <- paste0("q", qs) 
            results
          } )


#' @rdname superdiv
#' 
setMethod(f = "subdiv", signature= "supercommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.terms <- list(alpha(data), alphabar(data), 
                              beta(data), betabar(data),
                              rho(data), rhobar(data),
                              gamma(data))
            # Calculate subcommunity diversity
            results <- lapply(div.terms, subdiv, qs)
            results
          } )