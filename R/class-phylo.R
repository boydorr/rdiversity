#' Class 'rdphylo'
#' 
#' Define S4 class \code{rdphylo}.
#' 
#' 
#' 
#' 
rdphylo <- setClass("rdphylo",
                    contains = 'phylo',
                    slots = c(hs.name = "vector",
                              hs.pds = "vector",
                              hs.edge = "matrix",
                              hs.abundance = "vector")
)


#' 
#' 
#' 
#' 
#' 
#' 
is.rdphylo <-
  function (x) 
  {
    inherits(x, "rdphylo")
  }


#' 
#' 
#' 
#' 
#' 
#' 
#' 
setMethod(f = "show", signature(object = "rdphylo"), 
          definition = function(object){
            cat('Phylogenetic tree with', length(object$tip.label), 
                'tips and', object$Nnode, 
                'internal nodes (including the root.\n\n')
            
            cat('Tip labels:\n', head(object$tip.label), '\n\n')
            
            if(is.rooted(object)) {
              rooted <- 'Rooted'
            } else rooted <- 'Unrooted'
            
            cat(rooted, '.')
          } )


#' 
#' 
#' 
#' 
#' 
#' 
setMethod(f = "rdplot", signature = "rdphylo", 
          definition = function(data){
            ape::plot.phylo(data) 
          } )







