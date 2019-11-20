#' Calculate individual-level diversity
#'
#' Generic function for calculating individual-level diversity.
#'
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{power_mean}: calculates raw and normalised subcommunity alpha, rho
#' or gamma diversity by taking the powermean of diversity components}
#' \item{\code{relativeentropy}: calculates raw or normalised subcommunity beta
#' diversity by taking the relative entropy of diversity components}
#' \item{\code{metacommunity}: calculates all subcommunity measures of diversity}
#' }
#'
#' @inheritParams subdiv
#'
#' @return \code{inddiv()} returns a standard output of class \code{rdiv}
#' @exportMethod inddiv
#'
#' @seealso \code{\link{subdiv}} for subcommunity-level diversity and
#' \code{\link{metadiv}} for metacommunity-level diversity.
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
#'
#' @examples
#' # Define metacommunity
#' pop <- cbind.data.frame(A = c(1,1), B = c(2,0), C = c(3,1))
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
setMethod(f = "inddiv", signature = "powermean",
          definition = function(data, qs) {
            output <- reshape2::melt(data@results)
            param <- data@similarity_parameters
            cbind.data.frame(measure = data@measure,
                                       q = rep(qs, each = nrow(output)),
                                       type_level = "type",
                                       type_name = output$Var1,
                                       partition_level = "subcommunity",
                                       partition_name = output$Var2,
                                       diversity = output$value,
                                       dat_id = data@dat_id,
                                       transformation = param$transform,
                                       normalised = param$normalise,
                                       k = param$k,
                                       max_d = param$max_d,
                                       stringsAsFactors = FALSE)
          } )


#' @rdname inddiv
#'
setMethod(f = "inddiv", signature = "relativeentropy",
          definition = function(data, qs) {
            output <- reshape2::melt(data@results)
            param <- data@similarity_parameters
            cbind.data.frame(measure = data@measure,
                                       q = rep(qs, each = nrow(output)),
                                       type_level = "type",
                                       type_name = output$Var1,
                                       partition_level = "subcommunity",
                                       partition_name = output$Var2,
                                       diversity = output$value,
                                       dat_id = data@dat_id,
                                       transformation = param$transform,
                                       normalised = param$normalise,
                                       k = param$k,
                                       max_d = param$max_d,
                                       stringsAsFactors = FALSE)
          } )


#' @rdname inddiv
#'
setMethod(f = "inddiv", signature = "metacommunity",
          definition = function(data, qs) {
            # Calculate terms
            div.measures <- list(raw_alpha, norm_alpha,
                                 raw_beta, norm_beta,
                                 raw_rho, norm_rho,
                                 raw_gamma)
            # Calculate subcommunity diversity
            output <- lapply(div.measures, function(x) inddiv(x(data), qs))
            do.call(rbind.data.frame, output)
          } )
