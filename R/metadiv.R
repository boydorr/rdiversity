#' Metacommunity-level diversity
#'
#' Generic function for calculating metacommunity-level diversity.
#'
#' \code{data} may be input as one of three different classes:
#' \itemize{
#' \item{\code{powermean}: raw or normalised metacomunity alpha, rho or gamma 
#' diversity components; will calculate metacommunity-level raw or normalised 
#' metacomunity alpha, rho or gamma diversity}
#' \item{\code{relativeentropy}: raw or normalised metacommunity beta
#' diversity components; will calculate metacommunity-level raw or normalised 
#' metacommunity beta diversity}
#' \item{\code{metacommunity}: will calculate all metacommunity measures of 
#' diversity}
#' }
#'
#' @param data see \emph{Details}
#' @param qs \code{vector} of mode \code{numeric}; \emph{q} parameter.
#'
#' @return \code{metadiv()} returns a standard \code{diversity} output, 
#' with columns:
#' \itemize{
#' \item\code{measure}: raw or normalised, alpha, beta, rho, or gamma
#' \item\code{q}: order of diversity 
#' \item\code{type_level}: "types"
#' \item\code{type_name}: label attributed to type
#' \item\code{partition_level}: level of diversity, \emph{i.e.} "metacommunity"
#' \item\code{partition_name}: label attributed to partition
#' \item\code{diversity}: calculated metacommunity-level diversity
#' }
#' 
#' @seealso \code{\link{inddiv}} for type-level diversity and 
#' \code{\link{subdiv}} for subcommunity-level diversity.
#' 
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity. 
#' arXiv 1404.6520v3:1–9.
#' @exportMethod metadiv
#' 
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
setMethod(f = "metadiv", signature = "powermean",
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
            new("diversity", output)
            } )


#' @rdname metadiv
#'
setMethod(f = "metadiv", signature = "relativeentropy",
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
            new("diversity", output)
            } )


#' @rdname metadiv
#'
setMethod(f = "metadiv", signature = "metacommunity",
          definition = function(data, qs) {
            # Calculate terms
            div.measures <- list(raw_alpha, norm_alpha,
                                 raw_beta, norm_beta,
                                 raw_rho, norm_rho,
                                 raw_gamma)
            # Calculate metacommunity diversity
            output <- lapply(div.measures, function(x) metadiv(x(data), qs))
            output <- do.call(rbind.data.frame, output)
            new("diversity", output)
          } )
