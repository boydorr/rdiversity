#' Calculate individual-level diversity
#'
#' Generic function for calculating individual-level diversity.
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
#' @inheritParams subdiv
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
#' inddiv(g, 0:2)
#'
#' # Calculate subcommunity beta diversity (takes the relative entropy)
#' b <- raw_beta(meta)
#' inddiv(b, 0:2)
#'
#' # Calculate all measures of individual diversity
#' inddiv(meta, 0:2)
#' 
setGeneric(name = "inddiv",
           def = function(data, qs) {
             standardGeneric("inddiv")
           } )


#' @rdname inddiv
#'
setMethod(f = "inddiv", signature= "powermean",
          definition = function(data, qs) {
            output <- reshape2::melt(data@results)
            output <- cbind.data.frame(measure = data@measure, 
                                       q = rep(qs, each=nrow(output)),  
                                       type_level = "type", 
                                       type_name = output$Var1, 
                                       partition_level = "subcommunity",
                                       partition_name = output$Var2,
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname inddiv
#' @return
#'
setMethod(f = "inddiv", signature= "relativeentropy",
          definition = function(data, qs) {
            output <- reshape2::melt(data@results)
            output <- cbind.data.frame(measure = data@measure, 
                                       q = rep(qs, each=nrow(output)),  
                                       type_level = "type", 
                                       type_name = output$Var1, 
                                       partition_level = "subcommunity",
                                       partition_name = output$Var2,
                                       diversity = output$value, 
                                       stringsAsFactors = FALSE)
            tibble::as_data_frame(output)
          } )


#' @rdname inddiv
#'
setMethod(f = "inddiv", signature= "metacommunity",
          definition = function(data, qs) {
            # Calculate terms
            div.measures <- list(raw_alpha, norm_alpha,
                                 raw_beta, norm_beta,
                                 raw_rho, norm_rho,
                                 raw_gamma)
            # Calculate subcommunity diversity
            results <- lapply(div.measures, function(x) inddiv(x(data), qs))
            results <- do.call(rbind.data.frame, results)
            tibble::as_data_frame(results)
          } )

