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
#' An object of class \code{phy_struct}.
#' 
setClass("phy_struct", slots = c(structure = "matrix", 
                                 parameters = "tbl_df"))

