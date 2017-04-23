#' relativeentropy-class
#' 
#' Container for class \code{relativeentropy}.
#' 
#' @name relativeentropy-class
#' @rdname relativeentropy-class
#' @exportClass relativeentropy
#' 
#' @field results object of class \code{matrix} of mode \code{numeric}; contains
#' diversity term values output from \code{raw_beta()} or \code{norm_beta()}
#' @field measure measure
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
#' @references Reeve, R., T. Leinster, C. Cobbold, J. Thompson, N. Brummitt, 
#' S. Mitchell, and L. Matthews. 2014. How to partition diversity. 
#' arXiv 1404.6520:1–9.
#' 
setClass("relativeentropy", slots = c(results = "matrix",
                                      measure = "character",
                                      type_abundance = "matrix",
                                      ordinariness = "matrix",
                                      subcommunity_weights = "vector",
                                      type_weights = "matrix"))


