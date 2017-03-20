setOldClass("phylo")

#' Coerce to Metacommunity
#'
#' Functions to check if an object is a \code{metacommunity} or coerce an
#' object into a \code{metacommunity}.
#'
#' \enumerate{
#' \item type_abundance - proportional abundance of \emph{types} in the  
#' subcommunity as a fraction of the metacommunity as a whole (in the 
#' phylogenetic case, this corresponds to the proportional abundance of 
#' historic species, which is calculated from the proportional abundance of 
#' present day species)
#' \item similarity - pairwise similarity between \emph{types}
#' \item ordinariness - ordinariness of types 
#' \item subcommunity_weights - subcommunity weights
#' \item type_weights - weight of types within a subcommunity
#' \item raw_abundance - proportional abundance of samples (usually types, 
#' except in the phylogenetic case where samples correspond to the present 
#' day species)
#' \item raw_structure - length of historic species (in phylogeny)
#' \item parameters - parameters associated with each 
#' historic species (in phylogeny)
#' }
#'
#' @name metacommunity
#' @rdname metacommunity-methods
#' @include class-metacommunity.R check_partition.R check_similarity.R
#' @exportMethod metacommunity
#'
#' @param partition two-dimensinal \code{matrix} of mode \code{numeric} with 
#' rows as types, columns as subcommunities, and elements containing relative 
#' abundances of types in subcommunities. In the case of phylogenetic 
#' metacommunities, these are the relative abundances of terminal taxa. 
#' @param similarity (optional) object that describes similarity between
#' individuals or types. Usually missing (all types are distinct) or a matrix
#' showing similarities, but can be of class \code{phylo}.
#' @param ... (optional) additional arguments, especially:
#' @param interval (optional) for phylogenetic metacommunities only, how
#' far back we go in the tree, with 0 marking the date of the most
#' recent tip, and 1 (the default) marking the most recent common
#' ancestor. Numbers greater than 1 extend the root of the tree.
#'
#' @return Returns an object of class \code{metacommunity} (see Details).
#'
#' @seealso \code{\link{metacommunity-class}}
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- tree$tip.label
#' partition <- partition / sum(partition)
#' 
#' a <- metacommunity(partition, tree)
#' b <- metacommunity(partition)
#' 
setGeneric(name = "metacommunity",
           def = function(partition, similarity, ...) {
             standardGeneric("metacommunity")
           } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,data.frame-method
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "missing"),
          definition = function(partition) {
            # If similarity is data.frame, convert to matrix
            partition <- as.matrix(partition)
            
            metacommunity(partition)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,numeric-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "missing"),
          definition = function(partition) {
            # If similarity is numeric/vector, convert to matrix
            partition <- as.matrix(partition)
            
            metacommunity(partition)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,matrix-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "missing"),
          definition = function(partition) {
            # If similarity is not input, create identity matrix
            similarity <- diag(1, nrow(partition))
            row.names(similarity) <- row.names(partition)
            colnames(similarity) <- row.names(partition)
            
            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,data.frame-method,matrix-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "matrix"),
          definition = function(partition, similarity) {
            # If similarity is data.frame, convert to matrix
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,numeric-method,matrix-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "matrix"),
          definition = function(partition, similarity) {
            # If similarity is numeric/vector, convert to matrix
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,matrix-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "matrix"),
          definition = function(partition, similarity) {
            # Check partition and simliarity matrices
            type_abundance <- check_partition(partition)
            similarity <- check_similarity(partition, similarity)
            
            # Calculate parameters
            subcommunity_weights <- colSums(type_abundance) /
              sum(type_abundance)
            type_weights <- apply(type_abundance, 2, function(x) x/sum(x))
            Zp.j <- similarity %*% type_abundance
            
            # Mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            if(!is.matrix(type_weights)) {
              type_weights<- t(as.matrix(type_weights))
              row.names(type_weights) <- row.names(type_abundance)
            }
            
            new('metacommunity', 
                type_abundance = type_abundance,
                similarity = similarity,
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "missing", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            colnames(partition) <- "sc1"
            
            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,numeric-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity, interval)            
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,data.frame-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity, interval)            
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,matrix-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)
            ps <- phy_struct(similarity)
            ps <- chainsaw(ps, interval)
            structure_matrix <- ps$structure
            
            type_abundance <- phy_abundance(partition, structure_matrix)
            s <- smatrix(ps)
            z <- zmatrix(partition, s, ps)
            z <- check_similarity(type_abundance, z)
            
            # Calculate parameters
            subcommunity_weights <- colSums(type_abundance) /
              sum(type_abundance)
            type_weights <- apply(type_abundance, 2, function(x) x/sum(x))
            Zp.j <- z %*% type_abundance
            
            # Mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            if(!is.matrix(type_weights)) {
              type_weights<- t(as.matrix(type_weights))
              row.names(type_weights) <- row.names(type_abundance)
            }
            
            new('metacommunity', 
                type_abundance = type_abundance,
                similarity = z,
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights,
                raw_abundance = partition,
                raw_structure = ps$structure,
                parameters = ps$parameters)
          } )


#' @rdname metacommunity-class
#' @param x any R object
#' @return
#' returns TRUE if its argument is a metacommunity, FALSE otherwise.
#' @export
#'
is.metacommunity <- function (x)
  inherits(x, "metacommunity")


#' @rdname metacommunity-class
#' @param object object of class \code{metacommunity}
#'
setMethod(f = "show", signature= "metacommunity",
          definition = function(object) {
            
            cat('@type_abundance: Matrix of relative abundances (', 
                ncol(object@type_abundance), 'subcommunities,',
                nrow(object@type_abundance), 'types )\n')
            cat('@similarity: Similarity matrix\n')
            cat('@ordinariness: Matrix of type ordinariness\n')
            cat('@subcommunity_weights: Vector of subcommunity weights\n')
            cat('@type_weights: Vector of type weights\n')

            if(!isTRUE(all.equal(0, length(object@raw_abundance))))
              cat('@raw_abundance: Matrix of (phylo) tip relative abundances (',
                  ncol(object@raw_abundance), 'subcommunities,',
                  nrow(object@raw_abundance), 'terminal taxa )\n')

            if(!isTRUE(all.equal(0, length(object@raw_structure))))
              cat('@raw_structure: Matrix of (phylo) structure (',
                  sum(colSums(object@raw_structure) > 0), 'tips,',
                  sum(rowSums(object@raw_structure) > 0), 'historic species )\n')

            if(!isTRUE(all.equal(0, length(object@parameters))))
              cat('@parameters: Parameters associated with (phylo) historic species\n')
          } )

