#' metacommunity-class
#' 
#' Container for class \code{metacommunity}.
#' 
#' @name metacommunity-class
#' @rdname metacommunity-class
#' @exportClass metacommunity
#' 
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}  
#' with rows as types, columns as subcommunities, and elements containing  
#' relative abundances of types in subcommunities.  (in the phylogenetic case, 
#' this corresponds to the proportional abundance of historic species, which
#' is calculated from the proportional abundance of present day species)
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains pairwise similarity between \emph{types}
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains ordinariness of types 
#' @field subcommunity_weights \code{vector} of mode \code{numeric}; contains
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains weight of types within a subcommunity
#' @field raw_abundance two-dimensional \code{matrix} of mode \code{numeric};  
#' contains proportional abundance of samples (usually types, except in the 
#' phylogenetic case where samples correspond to the present day species)
#' @field raw_structure two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains length of historic species (in phylogeny)
#' @field parameters \code{tibble} containing parameters associated with each 
#' historic species (in phylogeny)
#' 
setClass("metacommunity", 
         slots = c(type_abundance = "matrix",
                   similarity = "matrix",
                   ordinariness = "matrix",
                   subcommunity_weights = "numeric", 
                   type_weights = "matrix",
                   raw_abundance = "matrix",
                   raw_structure = "matrix",
                   parameters = "data.frame"))

