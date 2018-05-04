setOldClass("phylo")

#' Coerce to \code{metacommunity}
#'
#' Functions to check if an object is a \code{metacommunity} or coerce an
#' object into a \code{metacommunity}.
#'
#' When calculating phylogenetic diversity either:
#' \itemize{
#' \item set \code{partition} as the relative abundance of present-day species,  
#' with \code{similarity} as an object of class \code{phylo}, from which the 
#' relative abundance and pairwise similarity of historical species will be
#' calculated; or
#' \item set \code{partition} as the relative abundance of historical species,
#' with \code{similarity} as the pairwise similarity of historical species.
#' }
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
#' @name metacommunity
#' @rdname metacommunity-methods
#' @include class-metacommunity.R check_partition.R check_similarity.R
#' @exportMethod metacommunity
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}  
#' with rows as types, columns as subcommunities, and elements containing  
#' the relative abundances of types in subcommunities. For phylogenetic 
#' diversity, see \emph{Details}.
#' @param similarity (optional) two-dimensional \code{matrix} of mode 
#' \code{numeric}, with rows as types, columns as types, and elements 
#' containing the pairwise similarity between types. For phylogenetic 
#' diversity, see \emph{Details}.
#' @param ... (optional) additional arguments, especially:
#' @param depth (optional; and for phylogenetic metacommunities only) how
#' much evolutionary history should be retained, with 0 marking the most
#' recent present-day species, and 1 (the default) marking the most recent 
#' common ancestor. Numbers greater than 1 extend the root of the tree.
#'
#' @return Returns an object of class \code{metacommunity} (see \emph{Fields}).
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
          definition = function(partition, similarity, depth = 1) {
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            colnames(partition) <- "sc1"
            
            metacommunity(partition, similarity, depth)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,numeric-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "phylo"),
          definition = function(partition, similarity, depth = 1) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity, depth)            
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,data.frame-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "phylo"),
          definition = function(partition, similarity, depth = 1) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity, depth)            
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,matrix-method,phylo-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "phylo"),
          definition = function(partition, similarity, depth = 1) {
            partition <- check_partition(partition = partition)
            ps <- phy_struct(tree = similarity, partition = partition)
            
            chainsaw(partition = partition, ps = ps, depth = depth)
          } )


#' @rdname metacommunity-methods
#' @param x any R object
#' @return
#' Returns TRUE if its argument is a metacommunity, FALSE otherwise.
#' @export
#'
is.metacommunity <- function (x)
  inherits(x, "metacommunity")


#' @rdname metacommunity-class
#' @param object object of class \code{metacommunity}
#'
setMethod(f = "show", signature= "metacommunity",
          definition = function(object) {
            cat('Object of class metacommunity, containing:\n')
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
                  sum(rowSums(object@raw_structure) > 0), 'historical species )\n')
            
            if(!isTRUE(all.equal(0, length(object@parameters))))
              cat('@parameters: Parameters associated with (phylo) historical species\n')
          } )

