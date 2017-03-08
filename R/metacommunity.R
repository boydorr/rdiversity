setOldClass("phylo")

#' Coerce to Metacommunity
#'
#' Functions to check if an object is a \code{metacommunity} or coerce an
#' object into a \code{metacommunity}.
#'
#' \enumerate{
#' \item .Data (partition) - proportional abundance of samples (usually types,
#' except in the phylogenetic case where samples correspond to the present day
#' species)
#' \item similarity - pairwise similarity between \emph{types}
#' \item type_abundance - proportional abundance of \emph{types} in the
#' subcommunity as a fraction of the metacommunity as a whole (in the
#' phylogenetic case, this corresponds to the proportional abundance of
#' historic species, which is calculated from the proportional abundance of
#' present day species)
#' \item ordinariness - ordinariness of types
#' \item phylo_struct - length of historic species (in phylogeny)
#' \item subcommunity_weights - subcommunity weights
#' \item type_weights - weight of types within a subcommunity
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
#' showing similarities, but can be of class \code{phylo} or \code{rdphylo}.
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
            
            new('metacommunity', partition,
                similarity = similarity,
                type_abundance = type_abundance,
                ordinariness = Zp.j,
                phylo_struct = diag(1, nrow(type_abundance)),
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
            ps <- trim(partition, similarity, interval)
            
            type_abundance <- phy_abundance(partition, ps)
            s_matrix <- s_matrix(similarity, ps)
            zmatrix <- z_matrix(partition, s_matrix, ps)
            similarity <- check_similarity(type_abundance, zmatrix)
            
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
            
            new('metacommunity', partition,
                similarity = similarity,
                type_abundance = type_abundance,
                ordinariness = Zp.j,
                phylo_struct = ps@structure,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
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
            n <- dim(object@type_abundance)[2]
            S <- dim(object@type_abundance)[1]
            cat('Metacommunity object with', n,
                'subcommunities and', S, 'types.\n\n')
            cat('Subcommunity labels:\n')
            cat(colnames(object@type_abundance),'\n\n')
            cat('Type labels:\n')
            cat(rownames(object@type_abundance),'\n')
          } )

