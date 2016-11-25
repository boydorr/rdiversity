#' Calculate metacommunity diversity
#' 
#' Generic function for calculating metacommunity diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates raw or normalised metacomunity alpha, rho
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
#' meta <- metacommunity(pop)
#' 
#' # Calculate metacommunity gamma diversity (takes the power mean)
#' g <- raw_gamma(meta)
#' metadiv(g, 0:2)
#' 
#' # Calculate metacommunity beta diversity (takes the relative entropy)
#' b <- raw_beta(meta)
#' metadiv(b, 0:2)
#' 
#' # Calculate all measures of metacommunity diversity
#' metadiv(meta, 0:2)
#' 
setGeneric(name = "metadiv",
           def = function(data, qs) {
             standardGeneric("metadiv")
           } )


#' @rdname metadiv
#' 
setMethod(f = "metadiv", signature= "powermean", 
          definition = function(data, qs) {  
            results <- list()
            for(i in 1:length(qs)) {
              subdiv <- sapply(1:ncol(data), function(y) 
                power_mean(data[,y], 1-qs[i], data@type_weights[,y]))
              results[[i]] <- power_mean(subdiv, 1-qs[i], 
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


#' @rdname metadiv
#' 
setMethod(f = "metadiv", signature= "relativeentropy", 
          definition = function(data, qs) {  
            results <- list()
            for(i in 1:length(qs)) {
              subdiv <- sapply(1:ncol(data), function(y) 
                power_mean(data[,y], qs[i]-1, data@type_weights[,y]))
              results[[i]] <- power_mean(subdiv, 1-qs[i], 
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


#' @rdname metadiv
#' 
setMethod(f = "metadiv", signature= "metacommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.measures <- list(raw_alpha, normalised_alpha, 
                                 raw_beta, normalised_beta,
                                 raw_rho, normalised_rho,
                                 raw_gamma)
            # Calculate metacommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- metadiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
          } )
