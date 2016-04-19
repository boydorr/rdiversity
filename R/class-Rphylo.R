setOldClass("phylo")

#' Class 'rdphylo'
#' 
#' Container for various parameters associated with calculating phylogenetic 
#' diversity. 
#' 
#' @field hs.edge two-column \code{matrix} of mode numeric; each row represents
#' a historic species. Nodes and tips are symbolized with numbers. Tips are 
#' numbered 1, 2, ..., and nodes are numbered after the tips. 
#' For each row, the first column gives the ancestor.
#' @field hs.name \code{vector} of more character; giving the names of the 
#' historic species, ordered by the number in hs.edge.
#' @field hs.pds \code{vector} of mode numeric; giving the present day 
#' species descendant from each historic species, ordered by the number in 
#' hs.edge.
#' @field hs.abundance \code{vector} of mode numeric; giving the relative 
#' abundance of historic species, ordered by the number in hs.edge.
#' @field Lj \code{vector} of length \emph{S}; giving the total evolutionary 
#' change of present day species, ordered by the number in edge.
#' @field Tbar \code{numeric} element; giving the mean total evolutionary 
#' change over present day species.
#' @param object object of class \linkS4class{rdphylo}
#' 
#' @return 
#' An object of class \code{rdphylo}. Builds on class \code{phylo} and 
#' therefore contains the following components:
#' \tabular{ll}{
#' \code{edge} \tab two-column \code{matrix} of mode numeric; each row  
#' represents an edge of the tree. Nodes and tips are symbolized with numbers.
#' Tips are numbered 1, 2, ...,N and inner nodes are numbered N+1, N+2, ... 
#' For each row, the first column gives the ancestor. \cr
#' \code{tip.label} \tab \code{vector} of mode character; giving the names of the 
#' tips ordered by the (positive) number in edge. \cr
#' \code{edge.length} \tab (optional) \code{vector} of mode numeric; giving the 
#' lengths of the branches given by edge. \cr
#' \code{Nnode} \tab \code{integer}; number of internal nodes. \cr
#' }
#' 
#' @export
#' 
rdphylo <- setClass("rdphylo",
         contains = "phylo",
         slots = c(hs.name = "character",
                   hs.pds = "numeric",
                   hs.edge = "matrix",
                   hs.length = "integer",
                   hs.abundance = "matrix",
                   Lj = "numeric",
                   Tbar = "numeric"
                   ))


#' @describeIn rdphylo prints various parameters associated with the phylogeny
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




