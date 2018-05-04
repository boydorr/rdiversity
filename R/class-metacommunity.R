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
#' relative abundances of types in subcommunities.  In the phylogenetic case, 
#' this corresponds to the proportional abundance of historic species, which
#' is calculated from the proportional abundance of present day species.
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as types, and elements containing pairwise 
#' similarities between types
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric} 
#' with rows as types, columns as subcommunities, and elements containing the
#' ordinariness of types within subcommunities 
#' @field subcommunity_weights \code{vector} of mode \code{numeric}; contains
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric}, 
#' with rows as types, columns as subcommunities, and elements containing 
#' weights of types within a subcommunity
#' @field raw_abundance [Phylogenetic] two-dimensional \code{matrix} of mode 
#' \code{numeric} with rows as types, columns as subcommunities, and elements 
#' containing the relative abundance of present day species
#' @field raw_structure [Phylogenetic] two-dimensional \code{matrix} of mode 
#' \code{numeric} with rows as historical species, columns as present day 
#' species, and elements containing historical species lengths within lineages
#' @field parameters [Phylogenetic] \code{tibble} containing parameters 
#' associated with each historic species in the phylogeny
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

