#' Calculate subcommunity-level diversity
#'
#' Generic function for calculating subcommunity-level diversity.
#'
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{power_mean}: calculates raw and normalised subcomunity alpha, rho
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy}: calculates raw or normalised subcommunity beta
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{metacommunity}: calculates all subcommunity measures of diversity}
#' }
#'
#' @param data \code{matrix} of mode \code{numeric}; containing diversity
#' components.
#' @param qs \code{vector} of mode \code{numeric}; parameter of conservatism.
#'
#' @return Returns a standard output of class \code{tibble}, with columns:
#' \itemize{
#' \item\code{measure}: raw or normalised, alpha, beta, rho, or gamma
#' \item\code{q}: parameter of conservatism
#' \item\code{type_level}: "subcommunity"
#' \item\code{type_name}: label attributed to type
#' \item\code{partition_level}: level of diversity, \emph{i.e.} subcommunity
#' \item\code{partition_name}: label attributed to partition
#' \item\code{diversity}: calculated subcommunity diversity
#' }
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. 
#' arXiv 1404.6520v3:1–9.
#' 
#' @export
#' @examples
#' # Define metacommunity
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#'
#' # Calculate subcommunity gamma diversity (takes the power mean)
#' g <- raw_gamma(meta)
#' subdiv(g, 0:2)
#'
#' # Calculate subcommunity beta diversity (takes the relative entropy)
#' b <- raw_beta(meta)
#' subdiv(b, 0:2)
#'
#' # Calculate all measures of subcommunity diversity
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
            # Calculate
            results <- lapply(seq_along(qs), function(x)
              sapply(1:ncol(data@results), function(y)
                power_mean(data@results[,y], 1-qs[x], data@type_weights[,y])))
            # Tidy up
            output <- do.call(cbind, results)
            row.names(output) <- colnames(data@results)
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            output <- cbind.data.frame(measure = data@measure,
                                       q = output$Var2, 
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "subcommunity",
                                       partition_name = output$Var1,
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname subdiv
#' @return
#'
setMethod(f = "subdiv", signature= "relativeentropy",
          definition = function(data, qs) {
            # Calculate
            results <- lapply(seq_along(qs), function(x)
              sapply(1:ncol(data@results), function(y)
                power_mean(data@results[,y], qs[x]-1, data@type_weights[,y])))
            # Tidy up
            output <- do.call(cbind,results)
            row.names(output) <- colnames(data@results)
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            output <- cbind.data.frame(measure = data@measure,
                                       q = output$Var2, 
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "subcommunity",
                                       partition_name = output$Var1,
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname subdiv
#'
setMethod(f = "subdiv", signature= "metacommunity",
          definition = function(data, qs) {
            # Calculate terms
            div.measures <- list(raw_alpha, norm_alpha,
                                 raw_beta, norm_beta,
                                 raw_rho, norm_rho,
                                 raw_gamma)
            # Calculate subcommunity diversity
            results <- lapply(div.measures, function(x) subdiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
            tibble::as_data_frame(results)
          } )

