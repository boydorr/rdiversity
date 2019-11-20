#' Metacommunity-level diversity
#'
#' Generic function for calculating metacommunity-level diversity.
#'
#' \code{data} may be input as one of three different classes:
#' \itemize{
#' \item{\code{powermean}: raw or normalised metacommunity alpha, rho or gamma
#' diversity components; will calculate metacommunity-level raw or normalised
#' metacommunity alpha, rho or gamma diversity}
#' \item{\code{relativeentropy}: raw or normalised metacommunity beta
#' diversity components; will calculate metacommunity-level raw or normalised
#' metacommunity beta diversity}
#' \item{\code{metacommunity}: will calculate all metacommunity measures of
#' diversity}
#' }
#'
#' @inheritParams subdiv
#'
#' @return \code{metadiv()} returns a standard output of class \code{rdiv}
#' @exportMethod metadiv
#'
#' @seealso \code{\link{inddiv}} for type-level diversity and
#' \code{\link{subdiv}} for subcommunity-level diversity.
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt,
#' S. Mitchell, and L. Matthews. 2016. How to partition diversity.
#' arXiv 1404.6520v3:1–9.
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
              subdiv <- vapply(seq_len(ncol(data@results)), function(y)
                power_mean(data@results[, y], 1 - qs[x], data@type_weights[, y]),
                numeric(1))
              power_mean(subdiv, 1 - qs[x], data@subcommunity_weights)
            })
            # Tidy up
            output <- do.call(cbind, results)
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            param <- data@similarity_parameters
            cbind.data.frame(measure = data@measure,
                                       q = output$Var2,
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "metacommunity",
                                       partition_name = "",
                                       diversity = output$value,
                                       dat_id = data@dat_id,
                                       transformation = param$transform,
                                       normalised = param$normalise,
                                       k = param$k,
                                       max_d = param$max_d,
                                       stringsAsFactors = FALSE)
            } )


#' @rdname metadiv
#'
setMethod(f = "metadiv", signature = "relativeentropy",
          definition = function(data, qs) {
            # Calculate
            results <- lapply(seq_along(qs), function(x) {
              subdiv <- vapply(seq_len(ncol(data@results)), function(y)
                power_mean(data@results[, y], qs[x] - 1, data@type_weights[, y]),
                numeric(1))
              power_mean(subdiv, 1 - qs[x], data@subcommunity_weights)
            })
            # Tidy up
            output <- do.call(cbind, results)
            row.names(output) <- "metacommunity"
            colnames(output) <- qs
            output <- reshape2::melt(output)
            # Output
            param <- data@similarity_parameters
            cbind.data.frame(measure = data@measure,
                                       q = output$Var2,
                                       type_level = "types",
                                       type_name = "",
                                       partition_level = "metacommunity",
                                       partition_name = "",
                                       diversity = output$value,
                                       dat_id = data@dat_id,
                                       transformation = param$transform,
                                       normalised = param$normalise,
                                       k = param$k,
                                       max_d = param$max_d,
                                       stringsAsFactors = FALSE)
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
            do.call(rbind.data.frame, output)
          } )
