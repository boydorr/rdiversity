#' Coerse to Supercommunity
#' 
#' Functions to check if an object is a \code{supercommunity} or coerce an  
#' object into a \code{supercommunity}.
#' 
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
#' @name supercommunity
#' @rdname supercommunity-methods
#' @exportMethod supercommunity
#' 
#' @param partition (optional) \code{vector} of mode \code{numeric}; containing the 
#' proportional abundance of present day species (leaves)
#' @param similarity object of class \code{phylo} or \code{rdphylo}
#' @param interval object of mode numeric
#' @param ... additional arguments.
#' 
#' @return Returns an object of class \code{supercommunity}; an S4 object 
#' containing five slots (see Details). 
#' 
#' @include class-supercommunity.R check_partition.R check_similarity.R
#' @seealso \code{\link{supercommunity-class}}
#' 
#' @examples 
#' tree <- ape::rtree(n = 5)
#' partition <- data.frame(a = c(1,0,1,0,0), b = c(0,1,0,1,1))
#' partition <- partition / sum(partition)
#' a <- supercommunity(partition, tree)
#' str(a)
#' a
#' 
#' showMethods("supercommunity")
#' 
setGeneric(name = "supercommunity",
           def = function(partition, similarity, ...) {
             standardGeneric("supercommunity")
           } )


#' @rdname supercommunity-methods
#' @aliases supercommunity, ANY-method
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "missing"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is data.frame, convert to matrix
            partition <- check_partition(partition)
            
            supercommunity(partition)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "numeric", similarity = "missing"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is numeric/vector, convert to matrix
            partition <- check_partition(partition)

            supercommunity(partition)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "matrix"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is data.frame, convert to matrix
            partition <- check_partition(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "numeric", similarity = "matrix"), 
          definition = function(partition, similarity = NA) {  
            # If similarity is numeric/vector, convert to matrix
            partition <- check_partition(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
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


#' @rdname supercommunity-methods
#' @export
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
            type_weights <- apply(type_abundance, 2, function(x) x/sum(x))
            Zp.j <- similarity %*% type_abundance
            
            # Mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            if(!is.matrix(type_weights)) {
              type_weights<- t(as.matrix(type_weights))
              row.names(type_weights) <- row.names(type_abundance)
            }
              
            new('supercommunity', partition, 
                similarity = similarity, 
                type_abundance = type_abundance, 
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "missing", similarity = "phylo"), 
          definition = function(partition, similarity, interval) { 
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)
            
            supercommunity(partition, similarity)
            } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "numeric", similarity = "phylo"), 
          definition = function(partition, similarity, interval) { 
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "phylo"), 
          definition = function(partition, similarity, interval) { 
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "matrix", similarity = "phylo"), 
          definition = function(partition, similarity, interval) {  
            partition <- check_partition(partition)
            similarity <- rdphylo(partition, similarity)
            
            supercommunity(partition, similarity)
            } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "missing", similarity = "rdphylo"), 
          definition = function(partition, similarity, interval) { 
            # If pds.abundance is not entered, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            partition <- check_partition(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "numeric", similarity = "rdphylo"), 
          definition = function(partition, similarity, interval) { 
            partition <- check_partition(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "data.frame", similarity = "rdphylo"), 
          definition = function(partition, similarity, interval) { 
            partition <- check_partition(partition)
            
            supercommunity(partition, similarity)
          } )


#' @rdname supercommunity-methods
#' @export
#' 
setMethod(f = "supercommunity", 
          signature(partition = "matrix", similarity = "rdphylo"), 
          definition = function(partition, similarity, interval) { 
            partition <- check_partition(partition)
            
            new.tree <- similarity
            historic.species <- new.tree@historic.species
            terminal.taxa <- new.tree@terminal.taxa
            Tbar <- new.tree@Tbar
            tag <- row.names(new.tree@structure)
            
            hs.abundance <- sapply(seq_along(historic.species$hs.name), function(x) {
              row.index <- match(historic.species$tip.node[x], terminal.taxa$tip.node)
              (historic.species$length[x] / Tbar) * terminal.taxa$pds.abundance[row.index]
            })
            hs.abundance <- cbind.data.frame(historic.species, hs.abundance)
            
            # Reinstate partitions
            index <- as.list(seq_along(hs.abundance$hs.name))
            type_abundance <- lapply(index, function(x) {
              row.index <- hs.abundance$tip.node[x]
              if(ncol(partition) == 1) {
                col.index <- which(partition[row.index] > 0)
              } else
                col.index <- which(partition[row.index,] > 0)
              vec <- t(matrix(rep(0, ncol(partition))))
              vec[,col.index] <- hs.abundance$hs.abundance[x]
              vec
            })
            type_abundance <- do.call(rbind, type_abundance)
            row.names(type_abundance) <- hs.abundance$hs.name
            colnames(type_abundance) <- colnames(partition)
            # type_abundance <- check_partition(type_abundance)
            
            subcommunity_weights <- colSums(type_abundance) / 
              sum(type_abundance)
            type_weights <- sapply(1:ncol(type_abundance), function(x)
              (type_abundance[,x]/colSums(type_abundance)[x]))
            
            zmatrix <- similarity_phylo(new.tree, partition)
            
            Zp.j <- zmatrix %*% type_abundance
            
            # Now mark all of the species that have nothing similar as NaNs
            # because diversity of an empty group is undefined
            Zp.j[Zp.j==0] <- NaN
            
            new('supercommunity', partition, 
                similarity = zmatrix, 
                type_abundance = type_abundance, 
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)          
          } )


#' @rdname supercommunity-methods
#' @export
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
            cat('Type abundance:', 
                paste0('[', nrow(object@type_abundance), 'x', 
                       ncol(object@type_abundance), 
                       ']\n------------------\n'))
            print(utils::head(object@type_abundance),10) 
            if(nrow(object@type_abundance)>10) {
              cat("..\t","...\n\n")
            } else
              cat("\n\n")
            
            cat('Type ordinariness:',
                paste0('[', nrow(object@ordinariness), 'x', 
                       ncol(object@ordinariness), 
                       ']\n------------------\n'))
            print(utils::head(object@ordinariness),10) 
            if(nrow(object@ordinariness)>10)
              cat("..\t","...\n\n\n")
            } )



