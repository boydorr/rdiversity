#' relativeentropy-class
#' 
#' Container for 
#' 
#' @name relativeentropy-class
#' @rdname relativeentropy-class
#' @exportClass relativeentropy
#' 
#' @field .Data two-dimensional \code{matrix} of mode \code{numeric}; contains 
#' diversity term values 
#' @field measure object of class \code{character}; contains an identifier 
#' associated with the diversity term being calculated
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains proportional abundance of \emph{types} in the subcommunity 
#' as a fraction of the metacommunity as a whole (in the phylogenetic case, 
#' this corresponds to the proportional abundance of historic species, which
#' is calculated from the proportional abundance of present day species)
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains ordinariness of types 
#' @field subcommunity_weights \code{vector} of mode \code{numeric}; contains
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains weight of types within a subcommunity
#' 
setClass("relativeentropy", contains = "matrix",
         slots = c(.Data = "matrix", 
                   measure = "character",
                   type_abundance = "matrix",
                   ordinariness = "matrix",
                   subcommunity_weights = "vector",
                   type_weights = "matrix"))


