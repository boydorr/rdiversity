#' phy_struct-class
#' 
#' Container for various parameters associated with calculating phylogenetic 
#' diversity. 
#' 
#' @name phy_struct-class
#' @rdname phy_struct-class
#' @exportClass phy_struct
#' 
#' @field structure \code{matrix} object
#' @field parameters object of class \code{tbl_df}
#' 
#' @return 
#' An object of class \code{phy_struct}. Builds on class \code{phylo} and 
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
setClass("phy_struct", slots = c(structure = "matrix", 
                                   parameters = "tbl_df"))

