#' Class 'rdiv'
#' 
#' Define S4 class \linkS4class{rdiv}.
#' 
#' @slot measure object of class \code{character}; e.g. 'subcommunity.alpha.bar'
#' @slot tag object of class \code{formula}; e.g. bquote('Subcommunity' ~ bar(alpha))
#' @slot level object of class \code{character}; e.g. 'subcommunity'
#' @param object object of class \linkS4class{rdiv}
#' @export
#' 
rdiv <- setClass("rdiv",
                 contains = 'data.frame',
                 slots = c(.Data ='data.frame',
                           measure = "character",
                           tag = "formula",
                           level = "character"))


is.rdiv <-
  function (x) 
  {
    inherits(x, "rdiv")
  }


#' @describeIn rdiv 
setMethod(f = "show", signature= "rdiv", 
          definition = function(object) {
            cat(object@measure, '\n\n')
            print(object) 
            } )

