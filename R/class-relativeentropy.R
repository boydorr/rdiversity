#' relativeentropy-class
#' 
#' Container for class \code{relativeentropy}.
#' 
#' @field results \code{data.frame} containing rdiversity output
#' @field measure object of class \code{character} naming the diversity
#' measure being calculated
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}; 
#' contains proportional abundance of \emph{types} in the subcommunity 
#' as a fraction of the metacommunity as a whole (in the phylogenetic case, 
#' this corresponds to the proportional abundance of historic species, which
#' is calculated from the proportional abundance of present day species)
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric} 
#' with rows as types, columns as subcommunities, and elements containing the
#' ordinariness of types within subcommunities 
#' @field subcommunity_weights \code{vector} of mode \code{numeric} containing
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric}, 
#' with rows as types, columns as subcommunities, and elements containing 
#' weights of types within a subcommunity
#' @field datID object of class \code{character} describing the class of 
#' distance / similarity being used, e.g. "naive", "taxonomic", and so on
#' @field similarity_components list containining the components necessary to 
#' calculate similarity. This list is empty when \code{precompute_dist = TRUE} 
#' when calculating distance. When a pairwise distance matrix is too large and  
#' \code{precompute_dist = FALSE}, this list contains all the information 
#' required to calculate pairwise distance between types
#' @field similarity_parameters list containining parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()} 
#' arguments)
#' 
#' @name relativeentropy-class
#' @rdname relativeentropy-class
#' @exportClass relativeentropy
#' 
setClass("relativeentropy", slots = c(results = "matrix",
                                      measure = "character",
                                      type_abundance = "matrix",
                                      ordinariness = "matrix",
                                      subcommunity_weights = "vector",
                                      type_weights = "matrix",
                                      datID = "character",
                                      similarity_components = "list",
                                      similarity_parameters = "list"))


