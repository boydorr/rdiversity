setOldClass("phylo")

#' rdphylo-class
#' 
#' Container for various parameters associated with calculating phylogenetic 
#' diversity. 
#' 
#' @name rdphylo-class
#' @rdname rdphylo-class
#' @exportClass rdphylo
#' 
#' @field historic.species \code{data_frame} object 
#' @field terminal.taxa \code{data_frame} object
#' @field Tbar object of class \code{numeric}
#' @field structure \code{matrix} of mode \code{numeric}
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
setClass("rdphylo", contains = "phylo",
                    slots = c(historic.species = "tbl_df",
                              terminal.taxa = "tbl_df", 
                              Tbar = "numeric", 
                              structure = "matrix"))

