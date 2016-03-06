#' Class 'rdphylo'
#'
#' @slot hs.name \code{vector} of length hS; historic species names
#' @slot hs.pds \code{vector} of length hS; descendant present day species
#' @slot hs.edge \code{matrix}; ancestral and descendant nodes
#' @slot hs.abundance \code{vector} of length hS; historic species abundance
#' @export
#' 
setClass("rdphylo",
         contains = "phylo",
         slots = c(hs.name = "vector",
                   hs.pds = "vector",
                   hs.edge = "matrix",
                   hs.abundance = "vector"))


is.rdphylo <-
  function (x)
  {
    inherits(x, "rdphylo")
  }


setMethod(f = "show", signature(object = "rdphylo"),
          definition = function(object){
            cat('Phylogenetic tree with', length(object$tip.label),
                'tips and', object$Nnode,
                'internal nodes (including the root.\n\n')

            cat('Tip labels:\n', head(object$tip.label), '\n\n')

            if(ape::is.rooted(object)) {
              rooted <- 'Rooted'
            } else rooted <- 'Unrooted'

            cat(rooted, '.')
          } )




