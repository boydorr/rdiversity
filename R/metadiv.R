#' Calculate metacommunity-level diversity
#'
#' Generic function for calculating metacommunity-level diversity.
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
#' @return Returns a standard output of class \code{tibble}, with columns:
#' \itemize{
#' \item\code{measure}: raw or normalised, alpha, beta, rho, or gamma
#' \item\code{q}: parameter of conservatism
#' \item\code{type_level}: "metacommunity"
#' \item\code{type_name}: label attributed to type
#' \item\code{partition_level}: level of diversity, \emph{i.e.} metacommunity
#' \item\code{partition_name}: label attributed to partition
#' \item\code{diversity}: calculated metacommunity diversity
#' }
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. 
#' arXiv 1404.6520v3:1–9.
#' 
#' @export
#' @examples
#' # Define metacommunity
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
            # Calculate
            results <- lapply(seq_along(qs), function(x) {
              subdiv <- sapply(1:ncol(data@results), function(y)
                power_mean(data@results[,y], 1-qs[x], data@type_weights[,y]))
              power_mean(subdiv, 1-qs[x], data@subcommunity_weights)
            })
            # Tidy up
            output <- do.call(cbind,results)
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            output <- cbind.data.frame(measure = data@measure,
                                       q = output$Var2, 
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "metacommunity",
                                       partition_name = "",
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname metadiv
#'
setMethod(f = "metadiv", signature= "relativeentropy",
          definition = function(data, qs) {
            # Calculate
            results <- lapply(seq_along(qs), function(x) {
              subdiv <- sapply(1:ncol(data@results), function(y)
                power_mean(data@results[,y], qs[x]-1, data@type_weights[,y]))
              power_mean(subdiv, 1-qs[x], data@subcommunity_weights)
            })
            # Tidy up
            output <- do.call(cbind,results)
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            output <- cbind.data.frame(measure = data@measure,
                                       q = output$Var2, 
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "metacommunity",
                                       partition_name = "",
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname metadiv
#'
setMethod(f = "metadiv", signature= "metacommunity",
          definition = function(data, qs) {
            # Calculate terms
            div.measures <- list(raw_alpha, norm_alpha,
                                 raw_beta, norm_beta,
                                 raw_rho, norm_rho,
                                 raw_gamma)
            # Calculate metacommunity diversity
            results <- lapply(div.measures, function(x)
              res <- metadiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
            tibble::as_data_frame(results)
          } )
