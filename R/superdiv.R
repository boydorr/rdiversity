#' Calculate supercommunity diversity
#' 
#' Generic function for calculating supercommunity diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates raw or normalised supercomunity alpha, rho
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy} calculates raw or normalised supercommunity beta
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{supercommunity} calculates all supercommunity measures of diversity}
#' }
#' 
#' @inheritParams subdiv 
#' 
#' @return Returns a \code{data_frame} containing all of the diversity measures.
#' @export
#' @examples 
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' pop <- pop / sum(pop)
#' super <- supercommunity(pop)
#' 
#' # Calculate supercommunity gamma diversity (takes the power mean)
#' g <- raw.gamma(super)
#' superdiv(g, 0:2)
#' 
#' # Calculate supercommunity beta diversity (takes the relative entropy)
#' b <- raw.beta(super)
#' superdiv(b, 0:2)
#' 
#' # Calculate all measures of supercommunity diversity
#' superdiv(super, 0:2)
#' 
setGeneric(name = "superdiv",
           def = function(data, qs) {
             standardGeneric("superdiv")
           } )


#' @rdname superdiv
#' 
setMethod(f = "superdiv", signature= "powermean", 
          definition = function(data, qs) {  
            results <- list()
            for(i in 1:length(qs)) {
              subdiv <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], 1-qs[i], data@type_weights[,y]))
              results[[i]] <- power.mean(subdiv, 1-qs[i], 
                                         data@subcommunity_weights)
            }
            
            output <- do.call(cbind,results)
            row.names(output) <- "supercommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "supercommunity", data@measure, 
                                       stringsAsFactors = FALSE)
            colnames(output) <- c("partition", "q", "diversity", "community", "measure")
            output$partition <- as.character(output$partition)
            tibble::as_data_frame(output)
          } )


#' @rdname superdiv
#' 
setMethod(f = "superdiv", signature= "relativeentropy", 
          definition = function(data, qs) {  
            results <- list()
            for(i in 1:length(qs)) {
              subdiv <- sapply(1:ncol(data), function(y) 
                power.mean(data[,y], qs[i]-1, data@type_weights[,y]))
              results[[i]] <- power.mean(subdiv, 1-qs[i], 
                                         data@subcommunity_weights)
            }
            
            output <- do.call(cbind,results)
            row.names(output) <- "supercommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "supercommunity", data@measure,
                                       stringsAsFactors = FALSE)
            colnames(output) <- c("partition", "q", "diversity", "community", "measure")
            output$partition <- as.character(output$partition)
            tibble::as_data_frame(output)
          } )


#' @rdname superdiv
#' 
setMethod(f = "superdiv", signature= "supercommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.measures <- list(raw.alpha, normalised.alpha, 
                                 raw.beta, normalised.beta,
                                 raw.rho, normalised.rho,
                                 raw.gamma)
            # Calculate supercommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- superdiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
          } )
