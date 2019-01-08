#' Generate rdiv
#'
#' Functions to check if an object is a \code{rdiv}
#'
#' @param res object of class \code{rdiv}
#'
#' @return \code{rdiv()} returns an object of class \code{rdiv}.
#' @include class-rdiv.R
#' @name rdiv
#' @rdname rdiv-methods
#' @exportMethod rdiv
#'
#' @examples
#' pop <- data.frame(a = c(1,3), b = c(1,1))
#' row.names(pop) <- paste0("sp", 1:2)
#' pop <- pop/sum(pop)
#' meta <- metacommunity(pop)
#' qs <- c(seq(0,1,.1),2:10, seq(20,100,10),Inf)
#'
#' b <- raw_beta(meta)
#' sc <- subdiv(b, qs)
#' mc <- metadiv(b, qs)
#' class(sc)
#' class(mc)
#'
#' res <- rbind.data.frame(sc, mc)
#' res <- rdiv(res)
#' class(res)
#'
#' res <- list(sc, mc)
#' res <- rdiv(res)
#' class(res)
#'
setGeneric(name = "rdiv",
           def = function(res) {
             standardGeneric("rdiv")
           } )



#' @rdname rdiv-methods
#' @aliases rdiv,data.frame-method
#'
setMethod(f = "rdiv", signature = "data.frame",
          definition = function(res) {
            new('rdiv', res)
          } )



#' @rdname rdiv-methods
#' @aliases rdiv,list-method
#'
setMethod(f = "rdiv", signature = "list",
          definition = function(res) {
            res <- do.call(rbind.data.frame, res)
            new('rdiv', res)
          } )



#' @rdname rdiv-class
#' @param object object of class \code{rdiv}
#'
setMethod(f = "show", signature = "rdiv",
          definition = function(object) {
            object <- asS3(object)
            object <- tibble::as_data_frame(object)
            print(object)
          } )



