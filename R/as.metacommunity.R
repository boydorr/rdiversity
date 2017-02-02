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
#' \item subcommunity_weights - subcommunity weights
#' \item type_weights - weight of types within a subcommunity
#' }
#'
#' @name metacommunity
#' @rdname metacommunity-methods
#' @include class-metacommunity.R check_partition.R check_similarity.R as.rdphylo.R
#' @exportMethod metacommunity
#'
#' @param partition \code{vector} or \code{matrix} containing the
#' relative abundances of individuals or types in their subcommunities. In the
#' case of phylogenetic metacommunities, these are the relative abundances of
#' the tips of the tree (the present day species).
#' @param similarity (optional) object that describes similarity between
#' individuals or types. Usually missing (all types are distinct) or a matrix
#' showing similarities, but can be of class \code{phylo} or \code{rdphylo}.
#' @param ... (optional) additional arguments, especially:
#' @param interval (optional) for phylogenetic metacommunities only, how
#' far back we go in the tree, with 0 marking the date of the most
#' recent tip, and 1 (the default) marking the most recent common
#' ancestor. Numbers greater than 1 extend the root of the tree.
#'
#' @return Returns an object of class \code{metacommunity}; an S4 object
#' containing five slots (see Details).
#'
#' @seealso \code{\link{metacommunity-class}}
#'
#' @examples
#' tree <- ape::rtree(n = 5)
#' partition <- data.frame(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' partition <- partition / sum(partition)
#' a <- metacommunity(partition, tree)
#' a
#' slotNames(a)
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
            partition <- check_partition(partition)

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
            partition <- check_partition(partition)

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
#' @aliases metacommunity,data.frame,matrix-method
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "matrix"),
          definition = function(partition, similarity) {
            # If similarity is data.frame, convert to matrix
            partition <- check_partition(partition)

            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "matrix"),
          definition = function(partition, similarity) {
            # If similarity is numeric/vector, convert to matrix
            partition <- check_partition(partition)

            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
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
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "missing", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "phylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "missing", similarity = "rdphylo"),
          definition = function(partition, similarity, interval = 1) {
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            partition <- check_partition(partition)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "rdphylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "rdphylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)

            metacommunity(partition, similarity, interval)
          } )


#' @rdname metacommunity-methods
#' @export
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "rdphylo"),
          definition = function(partition, similarity, interval = 1) {
            partition <- check_partition(partition)

            if(interval != 1)
              meta <- chainsaw(similarity, partition, interval)
            else {
              historic.species <- similarity@historic.species
              terminal.taxa <- similarity@terminal.taxa
              Tbar <- similarity@Tbar
              tag <- row.names(similarity@structure)

              type_abundance <- hs_abundance(partition,
                                             historic.species,
                                             terminal.taxa,
                                             Tbar,
                                             tag)

              subcommunity_weights <- colSums(type_abundance) /
                sum(type_abundance)
              type_weights <- sapply(1:ncol(type_abundance), function(x)
                (type_abundance[,x]/colSums(type_abundance)[x]))

              zmatrix <- similarity_phylo(similarity, partition)

              Zp.j <- zmatrix %*% type_abundance

              # Now mark all of the species that have nothing similar as NaNs
              # because diversity of an empty group is undefined
              Zp.j[Zp.j==0] <- NaN

              new('metacommunity', partition,
                  similarity = zmatrix,
                  type_abundance = type_abundance,
                  ordinariness = Zp.j,
                  subcommunity_weights = subcommunity_weights,
                  type_weights = type_weights)
            }} )


#' @rdname metacommunity-methods
#' @export
#'
as.metacommunity <- metacommunity


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

