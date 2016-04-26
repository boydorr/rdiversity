#' Coerse to Supercommunity
#' 
#' Functions to check if an object is a \code{supercommunity} or coerce an  
#' object into a \code{supercommunity}.
#' 
#' @rdname supercommunity-class
#' @param partition \code{matrix} (usually two-dimensional) of mode numeric;  
#' relative abundance of types
#' @param similarity (optional) two-dimensional \code{matrix} of mode numeric; 
#' pair-wise similarity of types. Default sets similarity to the naive-type 
#' case, where types are completely distinct. 
#' @param ... additional arguments.
#' 
#' @details 
#' \enumerate{
#' \item .Data (partition) - proportional abundance of samples (usually types, 
#' except in the phylogenetic case where samples correspond to the present day 
#' species)
#' \item similarity - pairwise similarity between \emph{types}
#' \item type_abundance - proportional abundance of \emph{types} in the  
#' subcommunity as a fraction of the supercommunity as a whole (in the  
#' phylogenetic case, this corresponds to the proportional abundance of 
#' historic species, which is calculated from the proportional abundance of 
#' present day species)
#' \item ordinariness - ordinariness of types 
#' \item subcommunity_weights - subcommunity weights
#' \item type_weights - weight of types within a subcommunity
#' }
#' 
#' @return Returns an object of class \code{supercommunity}; an S4 object 
#' containing five slots (see Details). 
#' 
#' @exportMethod supercommunity
#' @include class-supercommunity.R check_partition.R check_similarity.R
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' a <- supercommunity(tree)
#' b <- phylogenetic_similarity(tree)
#' 
setGeneric(name = "supercommunity",
           def = function(partition, similarity, ...) {
             standardGeneric("supercommunity")
           } )


#' @rdname supercommunity-class
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "missing"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is data.frame, convert to matrix
            partition <- as.matrix(partition)
            
            supercommunity(partition)
          } )


#' @rdname supercommunity-class
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "matrix"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is data.frame, convert to matrix
            partition <- as.matrix(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-class
#' 
setMethod(f = "supercommunity", 
          signature(partition = "matrix", similarity = "missing"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is not input, create identity matrix 
            similarity <- diag(1, nrow(partition))
            row.names(similarity) <- row.names(partition)
            colnames(similarity) <- row.names(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-class
#' 
setMethod(f = "supercommunity", 
          signature(partition = "matrix", similarity = "matrix"), 
          definition = function(partition, similarity = NA) {  
            # Check partition and simliarity matrices
            type_abundance <- check_partition(partition)
            similarity <- check_similarity(partition, similarity)
            
            # Calculate parameters
            subcommunity_weights <- colSums(type_abundance) / 
              sum(type_abundance)
            type_weights <- sapply(1:ncol(type_abundance), function(x)
              (type_abundance[,x]/colSums(type_abundance)[x]))
            Zp.j <- similarity %*% type_abundance
            
            # Mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            new('supercommunity', partition, 
                similarity = similarity, 
                type_abundance = type_abundance, 
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname supercommunity-class
#' @param pds.abundance \code{vector} of mode \code{numeric}; containing the 
#' proportional abundance of present day species (leaves)
#' 
setMethod(f = "supercommunity", 
          signature(partition = "phylo", similarity = "missing"), 
          definition = function(partition, similarity = NA, pds.abundance = NA) {  
            # Calculate pair-wise similarity of historic species
            new.tree <- as.rdphylo(partition)
            similarity <- phylogenetic_similarity(new.tree, pds.abundance)
            
            supercommunity(new.tree, similarity)
          } )


#' @rdname supercommunity-class
#' 
setMethod(f = "supercommunity", 
          signature(partition = "phylo", similarity = "matrix"), 
          definition = function(partition, similarity, pds.abundance = NA) {  
            new.tree <- as.rdphylo(partition)
            # Calculate relative abundance of historic species
            type_abundance <- new.tree@hs.abundance
            row.names(type_abundance) <- new.tree@hs.name
            
            type_abundance <- check_partition(type_abundance)
            # similarity <- check_similarity(type_abundance, similarity)
            
            subcommunity_weights <- colSums(type_abundance) / 
              sum(type_abundance)
            type_weights <- sapply(1:ncol(type_abundance), function(x)
              (type_abundance[,x]/colSums(type_abundance)[x]))
            Zp.j <- similarity %*% type_abundance
            
            # Now mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            new('supercommunity', partition, 
                similarity = similarity, 
                type_abundance = type_abundance, 
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname supercommunity-class
#' 
as.supercommunity <- supercommunity


#' @rdname supercommunity-class
#' @param x any R object 
#' @return 
#' returns TRUE if its argument is a supercommunity, FALSE otherwise.
#' @export
#' 
is.supercommunity <- function (x) 
  inherits(x, "supercommunity")


#' @rdname supercommunity-class
#' @param object object of class \code{supercommunity}
#' 
setMethod(f = "show", signature= "supercommunity", 
          definition = function(object) {
            print(head(object@.Data)) } )

