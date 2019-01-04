#' Metacommunity
#'
#' Functions to generate a \code{metacommunity} object.
#'
#' When calculating phylogenetic diversity either:
#' \itemize{
#' \item set \code{partition} as the relative abundance of terminal taxa,  
#' in which case \code{similarity} (the pairwise similarity of historical species) is calculated from \code{phy2branch()} and 
#' \code{dist2sim()}; or
#' \item set \code{partition} as the relative abundance of historical species,
#' with \code{similarity} (the pairwise similarity of historical species) is
#' calculated from \code{phy2branch()} and 
#' \code{dist2sim()}.
#' }
#' 
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}  
#' with rows as types, columns as subcommunities, and elements containing  
#' relative abundances of types in subcommunities. In the phylogenetic case, 
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
#' @exportMethod metacommunity
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}  
#' with rows as types, columns as subcommunities, and elements containing  
#' the relative abundances of types in subcommunities. For phylogenetic 
#' diversity, see \emph{Details}.
#' @param similarity (optional) two-dimensional \code{matrix} of mode 
#' \code{numeric}, with rows as types, columns as types, and elements 
#' containing the pairwise similarity between types.
#' 
#' @return \code{metacommunity()} returns an object of class 
#' \code{metacommunity} (see \emph{Fields}).
#'
#' @seealso \code{\link{metacommunity-class}}
#'
#' @examples
#' ## Naive-type
#' partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
#' row.names(partition) <- paste0("sp", 1:5)
#' partition <- partition / sum(partition)
#' meta <- metacommunity(partition)
#' 
setGeneric(name = "metacommunity",
           def = function(partition, similarity) {
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
#'
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "missing"),
          definition = function(partition) {
            # If similarity is not input, create identity matrix
            Z <- diag(1, nrow(partition))
            row.names(Z) <- row.names(partition)
            colnames(Z) <- row.names(partition)
            
            similarity <- new("similarity", 
                              similarity = Z,
                              datID = "naive",
                              parameters = list(transform = NA,
                                                k = NA,
                                                normalise = NA,
                                                max_d = NA))
            
            metacommunity(partition, similarity)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,data.frame-method,matrix-method
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
            
            message('From the next update, this function will no longer accept matrix objects within the `similarity` argument. Instead, use dist2sim() to generate similarity objects.')
            
            new('metacommunity', 
                type_abundance = type_abundance,
                similarity = similarity,
                ordinariness = Zp.j,
                subcommunity_weights = subcommunity_weights,
                type_weights = type_weights)
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#'
setMethod(f = "metacommunity",
          signature(partition = "missing", similarity = "similarity"),
          definition = function(partition, similarity) {
            # If partition is missing, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1/length(tips), length(tips)))
            row.names(partition) <- tips
            colnames(partition) <- "sc1"
            
            metacommunity(partition, similarity)    
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#'
setMethod(f = "metacommunity",
          signature(partition = "numeric", similarity = "similarity"),
          definition = function(partition, similarity) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity)    
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#'
setMethod(f = "metacommunity",
          signature(partition = "data.frame", similarity = "similarity"),
          definition = function(partition, similarity) {
            partition <- as.matrix(partition)
            
            metacommunity(partition, similarity)    
          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#' 
#' @examples 
#' ## Taxonomic
#' # Generate lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#' 
#' # Assign values for each level (Shimatani's taxonomic distance)
#' taxDistance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)
#' 
#' # Generate pairwise distances
#' distance <- tax2dist(lookup, taxDistance, FALSE)
#' similarity <- dist2sim(distance, "linear")
#' 
#' partition <- matrix(sample(8), ncol=2)
#' row.names(partition) <- Species
#' colnames(partition) <- LETTERS[1:2]
#' 
#' meta <- metacommunity(partition, similarity)
#' 
#' ## Distance-based phylogenetic
#' tree <- ape::rtree(5)
#' tree$tip.label <- paste0("sp", 1:5)
#' 
#' partition <- matrix(rep(1,10), nrow = 5)
#' row.names(partition) <- paste0("sp", 1:5)
#' partition <- partition / sum(partition)
#' 
#' distance <- phy2dist(tree)
#' similarity <- dist2sim(distance, "linear")
#' 
#' meta <- metacommunity(partition, similarity)
#' 
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "similarity"),
          definition = function(partition, similarity) {
            
            # If a similarity matrix is available (within the similarity 
            # object), then generate a metacommunity object in the normal way
            if(length(similarity@similarity) != 0) {
              
              # Check partition and simliarity matrices
              type_abundance <- check_partition(partition)
              Z <- similarity@similarity
              Z <- check_similarity(partition, Z)

              # Calculate parameters
              subcommunity_weights <- colSums(type_abundance) /
                sum(type_abundance)
              type_weights <- apply(type_abundance, 2, function(x) x/sum(x))
              Zp.j <- Z %*% type_abundance

              # Mark all of the species that have nothing similar as NaNs
              # because diversity of an empty group is undefined
              Zp.j[Zp.j==0] <- NaN

              if(!is.matrix(type_weights)) {
                type_weights<- t(as.matrix(type_weights))
                row.names(type_weights) <- row.names(type_abundance)
              }

              return(new('metacommunity',
                  type_abundance = type_abundance,
                  similarity = Z,
                  ordinariness = Zp.j,
                  subcommunity_weights = subcommunity_weights,
                  type_weights = type_weights,
                  datID = similarity@datID,
                  similarity_components = similarity@components,
                  similarity_parameters = similarity@parameters))
              
              
              
              # .. else calculate branch-based phylogenetic similarity and
              # generate a metacommunity object in the normal way
            }else if(similarity@datID == "phybranch") {
              
              components <- similarity@components
              ps <- phy_struct(components$tree, partition)
              return(chainsaw(partition = partition, 
                              ps = ps, 
                              depth = components$treeDepth))
              
              
              
              # .. otherwise calculate ordinariness line by line and generate
              # a metacommunity object in the normal way
            }else {
              
              components <- similarity@components
              
              Z <- lapply(1:nrow(partition), function(x) 
                get(components$ordinariness)(similarity, x))
              Z <- do.call(rbind.data.frame, Z)
              row.names(Z) <- row.names(partition)
              colnames(Z) <- colnames(partition)
              
              # Check partition and simliarity matrices
              type_abundance <- check_partition(partition)
              Z <- check_similarity(partition, Z)
              
              # Calculate parameters
              subcommunity_weights <- colSums(type_abundance) /
                sum(type_abundance)
              type_weights <- apply(type_abundance, 2, function(x) x/sum(x))
              Zp.j <- Z %*% type_abundance
              
              # Mark all of the species that have nothing similar as NaNs
              # because diversity of an empty group is undefined
              Zp.j[Zp.j==0] <- NaN
              
              if(!is.matrix(type_weights)) {
                type_weights<- t(as.matrix(type_weights))
                row.names(type_weights) <- row.names(type_abundance)
              }

              return(new('metacommunity', 
                         type_abundance = type_abundance,
                         ordinariness = Zp.j,
                         subcommunity_weights = subcommunity_weights,
                         type_weights = type_weights,
                         datID = similarity@datID,
                         similarity_components = similarity@components,
                         similarity_parameters = similarity@parameters))
            }
          } )



#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#'
setMethod(f = "metacommunity",
          signature(partition = "ANY", similarity = "phylo"),
          definition = function(partition, similarity) {
            stop("This function no longer accepts phylo objects within the `similarity` argument. Instead, generate an object of class `distance` using phy2branch() or phy2dist(), and convert this object to an object of class `similarity` using dist2sim().")
          } )


#' @rdname metacommunity-class
#' @param object object of class \code{metacommunity}
#'
setMethod(f = "show", signature = "metacommunity",
          definition = function(object) {
            cat('Object of class `metacommunity`, containing all of the data required to calculate diversity.')
          } )

