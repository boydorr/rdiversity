#' Calculate metacommunity diversity
#' 
#' Generic function for calculating metacommunity diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates raw or normalised supercomunity alpha, rho
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy} calculates raw or normalised metacommunity beta
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{metacommunity} calculates all metacommunity measures of diversity}
#' }
#' 
#' @inheritParams subdiv 
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} metacommunity), and \code{measure} (alpha, beta, rho, or gamma). 
#' 
#' @export
#' @examples 
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' pop <- pop / sum(pop)
#' super <- metacommunity(pop)
#' 
#' # Calculate metacommunity gamma diversity (takes the power mean)
#' g <- raw.gamma(super)
#' superdiv(g, 0:2)
#' 
#' # Calculate metacommunity beta diversity (takes the relative entropy)
#' b <- raw.beta(super)
#' superdiv(b, 0:2)
#' 
#' # Calculate all measures of metacommunity diversity
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
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "metacommunity", data@measure, 
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
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "metacommunity", data@measure,
                                       stringsAsFactors = FALSE)
            colnames(output) <- c("partition", "q", "diversity", "community", "measure")
            output$partition <- as.character(output$partition)
            tibble::as_data_frame(output)
          } )


#' @rdname superdiv
#' 
setMethod(f = "superdiv", signature= "metacommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.measures <- list(raw.alpha, normalised.alpha, 
                                 raw.beta, normalised.beta,
                                 raw.rho, normalised.rho,
                                 raw.gamma)
            # Calculate metacommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- superdiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
          } )
