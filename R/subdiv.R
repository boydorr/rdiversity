#' Calculate subcommunity diversity
#' 
#' Generic function for calculating subcommunity diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates subcomunity alpha, alphabar, rho, rhobar, 
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy} calculates subcommunity beta or betabar 
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{supercommunity} caculates all subcommunity measures of diversity}
#' }
#' 
#' @param data two-dimensional \code{matrix} of mode \code{numeric}; diversity 
#' components.
#' @param qs \code{vector} of mode \code{numeric}; parameter of conservatism.
#' 
#' @details as ds
#' 
#' @return Returns a two-dimensional \code{data_frame} of mode \code{numeric}.
#' @export 
#' @examples 
#' # Calculate the diversity of a single population
#' pop <- sample(1:50, 5)
#' pop <- pop/sum(pop)
#' super <- supercommunity(pop)
#' 
#' # Subcommunity gamma diversity (takes the power mean)
#' g <- gamma(super)
#' subdiv(g, 0:2)
#' 
#' # Subcommunity beta diversity (takes the relative entropy)
#' b <- beta(super)
#' subdiv(b, 0:2)
#' 
#' # All measures of subcommunity diversity
#' subdiv(super, 0:2)
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
            
            output <- do.call(cbind,results)
            colnames(output) <- paste0("q", qs) 
            row.names(output) <- colnames(data)
            tibble::as_data_frame(output)
          } )


#' @rdname subdiv
#' @return 
#' 
setMethod(f = "subdiv", signature= "relativeentropy", 
          definition = function(data, qs) {
            results <- list()
            for(i in seq_along(qs))
              results[[i]] <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], qs[i]-1, data@type_weights[,y]))

            output <- do.call(cbind,results)
            colnames(output) <- paste0("q", qs) 
            row.names(output) <- colnames(data)
            tibble::as_data_frame(output)
          } )


#' @rdname subdiv
#' 
setMethod(f = "subdiv", signature= "supercommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.measures <- list(alpha, alphabar, 
                              beta, betabar,
                              rho, rhobar,
                              gamma)
            # Calculate subcommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- subdiv(x(data), qs))
            names(results) <- c("alpha", "alphabar", "beta", "betabar",
                                "rho", "rhobar", "gamma") 
            results
          } )
           