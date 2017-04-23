#' powermean-class
#' 
#' Container for class \code{powermean}.
#' 
#' @name powermean-class
#' @rdname powermean-class
#' @exportClass powermean
#' 
#' @field output object of class \code{tibble}, with columns:
#' \code{measure}, (raw alpha, norm alpha, raw rho, etc.),
#' \code{q} (parameter of conservatism), 
#' \code{type_level} (), 
#' \code{type_name} (label attributed to type), 
#' \code{partition_level} (level of diversity, \emph{i.e.} subcommunity), 
#' \code{partition_name} (label attributed to partition), and 
#' \code{diversity}
#' @field results \code{matrix} of mode \code{numeric}; contains values 
#' calculated from diversity-term values output from \code{raw_alpha()}, 
#' \code{norm_alpha()}, \code{raw_rho()}, \code{norm_rho()}, or 
#' \code{raw_gamma()}
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
setClass("powermean", slots = c(results = "matrix",
                                measure = "character",
                                type_abundance = "matrix",
                                ordinariness = "matrix",
                                subcommunity_weights = "vector",
                                type_weights = "matrix"))


