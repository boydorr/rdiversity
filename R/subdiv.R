#' Calculate subcommunity diversity
#' 
#' Generic function for calculating subcommunity diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates raw and normalised subcomunity alpha, rho 
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy} calculates raw or normalised subcommunity beta 
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{metacommunity} caculates all subcommunity measures of diversity}
#' }
#' 
#' @param data two-dimensional \code{matrix} of mode \code{numeric}; diversity 
#' components.
#' @param qs \code{vector} of mode \code{numeric}; parameter of conservatism.
#' 
#' @return Returns a five-column \code{tibble}/\code{dataframe} containing:  
#' \code{partition} (label attributed to partition), \code{q} (parameter of 
#' conservatism), \code{diversity}, \code{community} (level of diversity, 
#' \emph{i.e.} subcommunity), and \code{measure} (alpha, beta, rho, or gamma). 
#' 
#' @export 
#' @examples 
#' # Calculate the diversity of a single population
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#' 
#' # Subcommunity gamma diversity (takes the power mean)
#' g <- raw.gamma(meta)
#' subdiv(g, 0:2)
#' 
#' # Subcommunity beta diversity (takes the relative entropy)
#' b <- raw.beta(meta)
#' subdiv(b, 0:2)
#' 
#' # All measures of subcommunity diversity
#' subdiv(meta, 0:2)
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
            
            output <- do.call(cbind, results)
            row.names(output) <- colnames(data)
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "subcommunity", data@measure, 
                                       stringsAsFactors = FALSE)
            colnames(output) <- c("partition", "q", "diversity", "community", "measure")
            output$partition <- as.character(output$partition)
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
            row.names(output) <- colnames(data)
            colnames(output) <- qs
            output <- reshape2::melt(output)
            output <- cbind.data.frame(output, "subcommunity", data@measure,
                                       stringsAsFactors = FALSE)
            colnames(output) <- c("partition", "q", "diversity", "community", "measure")
            output$partition <- as.character(output$partition)
            tibble::as_data_frame(output)
          } )


#' @rdname subdiv
#' 
setMethod(f = "subdiv", signature= "metacommunity", 
          definition = function(data, qs) {  
            # Calculate terms
            div.measures <- list(raw.alpha, normalised.alpha, 
                                 raw.beta, normalised.beta,
                                 raw.rho, normalised.rho,
                                 raw.gamma)
            # Calculate subcommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- subdiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
          } )
           
