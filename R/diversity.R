#' Generate diversity
#'
#' Functions to check if an object is a \code{diversity}
#'
#' @name diversity
#' @rdname diversity-methods
#' @exportMethod diversity
#' 
#' @param res object of class \code{diversity}
#' @include class-diversity.R 
#' @return \code{diversity()} returns an object of class \code{diversity}.
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
#' res <- diversity(res)
#' class(res)
#' 
#' res <- list(sc, mc)
#' res <- diversity(res)
#' class(res)
#' 
setGeneric(name = "diversity",
           def = function(res) {
             standardGeneric("diversity")
           } )



#' @rdname diversity-methods
#' @aliases diversity,data.frame-method
#'
setMethod(f = "diversity", signature = "data.frame",
          definition = function(res) {
            new('diversity', res)
          } )



#' @rdname diversity-methods
#' @aliases diversity,list-method
#'
setMethod(f = "diversity", signature = "list",
          definition = function(res) {
            res <- do.call(rbind.data.frame, res)
            new('diversity', res)
          } )



#' @rdname diversity-class
#' @param object object of class \code{diversity}
#'
setMethod(f = "show", signature = "diversity",
          definition = function(object) {
            object <- asS3(object)
            object <- tibble::as_data_frame(object)
            print(object)
          } )



setGeneric("plot")

#' @rdname diversity-class
#' @param x object of class \code{diversity}
#' @export
#'
setMethod(f = "plot", signature(x = "diversity"),
          definition = function(x) {
            what <- what(x)
            if(what=="inddiv") return(plot_inddiv(x))
            if(what=="subdiv" || (what=="metadiv") || what=="both") {
              qs <- unique(x$q)
              if(length(qs)==1) return(plot_single(x))
              if(any(qs==0) && any(qs==Inf)) return(plot_diversity(x))
              if(!any(qs %in% Inf)) return(simple_plot(x))
            }
          })
