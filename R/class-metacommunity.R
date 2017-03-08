#' metacommunity-class
#' 
#' Container for proportional abundance and similarity matrices. 
#' 
#' @name metacommunity-class
#' @rdname metacommunity-class
#' @exportClass metacommunity
#' 
#' @field .Data two-dimensional \code{matrix} of mode \code{numeric}; contains 
#' proportional abundance of samples (usually types, except in the phylogenetic
#' case where samples correspond to the present day species)
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains pairwise similarity between \emph{types}
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains proportional abundance of \emph{types} in the subcommunity 
#' as a fraction of the metacommunity as a whole (in the phylogenetic case, 
#' this corresponds to the proportional abundance of historic species, which
#' is calculated from the proportional abundance of present day species)
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains ordinariness of types 
#' @field phylo_struct two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains length of historic species (in phylogeny)
#' @field subcommunity_weights \code{vector} of mode \code{numeric}; contains
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains weight of types within a subcommunity
#' 
setClass("metacommunity", 
         slots = c(.Data = "matrix", 
                   similarity = "matrix",
                   type_abundance = "matrix",
                   ordinariness = "matrix",
                   phylo_struct = "matrix",
                   subcommunity_weights = "numeric", 
                   type_weights = "matrix"))

