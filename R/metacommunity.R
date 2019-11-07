#' Metacommunity
#'
#' Functions to generate a \code{metacommunity} object.
#'
#' @param partition two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as subcommunities, and elements containing
#' the relative abundances of types in subcommunities. For phylogenetic
#' diversity, see \emph{Details}
#' @param similarity (optional) object of class \code{similarity}
#'
#' @field type_abundance two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types (species), columns as subcommunities, and each
#' element containing the relative abundance of types in each subcommunity
#' relative to the metacommunity as a whole. In the phylogenetic case, this
#' corresponds to the proportional abundance of historical species, which is
#' calculated from the proportional abundance of terminal taxa
#' @field similarity two-dimensional \code{matrix} of mode \code{numeric} with
#' rows as types, columns as types, and elements containing pairwise
#' similarities between types
#' @field similarity_components list containing the components necessary to
#' calculate similarity. This list is empty when \code{precompute_dist = TRUE}
#' when calculating distance. When a pairwise distance matrix is too large and
#' \code{precompute_dist = FALSE}, this list contains all the information
#' required to calculate pairwise distance between types
#' @field similarity_parameters list containing parameters associated with
#' converting pairwise distances to similarities (the \code{dist2sim()}
#' arguments)
#' @field ordinariness two-dimensional \code{matrix} of mode \code{numeric}
#' with rows as types, columns as subcommunities, and elements containing the
#' ordinariness of types within subcommunities
#' @field subcommunity_weights \code{vector} of mode \code{numeric} containing
#' subcommunity weights
#' @field type_weights two-dimensional \code{matrix} of mode \code{numeric},
#' with rows as types, columns as subcommunities, and elements containing
#' weights of types within a subcommunity
#' @field dat_ID object of class \code{character} denoting the type of diversity
#' being calculated. This can be "naive", "genetic", "taxonomic", and so on
#' @field raw_abundance [Phylogenetic] two-dimensional \code{matrix} of mode
#' \code{numeric} with rows as types, columns as subcommunities, and elements
#' containing the relative abundance of present day species
#' @field raw_structure [Phylogenetic] two-dimensional \code{matrix} of mode
#' \code{numeric} with rows as historical species, columns as present day
#' species, and elements containing historical species lengths within lineages
#' @field parameters [Phylogenetic] \code{data.frame} containing parameters
#' associated with each historic species in the phylogeny
#'
#' @return \code{metacommunity()} returns an object of class
#' \code{metacommunity} (see \emph{Fields}).
#' @name metacommunity
#' @rdname metacommunity-methods
#' @exportMethod metacommunity
#'
#' @seealso \code{\link{metacommunity-class}}
#'
#' @examples
#' # Naive-type
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
                              dat_id = "naive",
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

            stop("This function no longer accepts matrix objects within the `similarity` argument. Instead, use dist2sim() to generate similarity objects.")

          } )


#' @rdname metacommunity-methods
#' @aliases metacommunity,similarity-method
#'
setMethod(f = "metacommunity",
          signature(partition = "missing", similarity = "similarity"),
          definition = function(partition, similarity) {
            # If partition is missing, assume an even distribution
            tips <- similarity$tip.label
            partition <- matrix(rep(1 / length(tips), length(tips)))
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
setMethod(f = "metacommunity",
          signature(partition = "matrix", similarity = "similarity"),
          definition = function(partition, similarity) {

            # If a similarity matrix is available (within the similarity
            # object), then generate a metacommunity object in the normal way
            if (length(similarity@similarity) != 0) {

              # Check partition and simliarity matrices
              type_abundance <- check_partition(partition)
              Z <- similarity@similarity
              Z <- check_similarity(Z, partition)

              # Calculate parameters
              subcommunity_weights <- colSums(type_abundance) /
                sum(type_abundance)
              type_weights <- apply(type_abundance, 2, function(x) x / sum(x))
              Zp.j <- Z %*% type_abundance

              # Mark all of the species that have nothing similar as NaNs
              # because diversity of an empty group is undefined
              Zp.j[Zp.j == 0] <- NaN

              if (!is.matrix(type_weights)) {
                type_weights <- t(as.matrix(type_weights))
                row.names(type_weights) <- row.names(type_abundance)
              }

              return(new("metacommunity",
                  type_abundance = type_abundance,
                  similarity = Z,
                  ordinariness = Zp.j,
                  subcommunity_weights = subcommunity_weights,
                  type_weights = type_weights,
                  dat_id = similarity@dat_id,
                  similarity_components = similarity@components,
                  similarity_parameters = similarity@parameters))

              # .. else calculate branch-based phylogenetic similarity and
              # generate a metacommunity object in the normal way
            }else if (similarity@dat_id == "phybranch") {

              components <- similarity@components
              ps <- phy_struct(components$tree, partition)
              return(chainsaw(partition = partition,
                              ps = ps,
                              depth = components$tree_depth))

              # .. otherwise calculate ordinariness line by line and generate
              # a metacommunity object in the normal way
            }else {

              components <- similarity@components

              # Calculate parameters
              type_abundance <- check_partition(partition)
              subcommunity_weights <- colSums(type_abundance) /
                sum(type_abundance)
              type_weights <- apply(type_abundance, 2, function(x) x / sum(x))

              Zp.j <- lapply(seq_len(nrow(type_abundance)), function(x) {
                tmp <- get(components$ordinariness)(similarity, x)
                tmp <- matrix(tmp, nrow = 1)
                tmp %*% type_abundance
              })

              Zp.j <- do.call(rbind.data.frame, Zp.j)
              row.names(Zp.j) <- row.names(partition)

              # Mark all of the species that have nothing similar as NaNs
              # because diversity of an empty group is undefined
              Zp.j[Zp.j == 0] <- NaN
              Zp.j <- as.matrix(Zp.j)

              if (!is.matrix(type_weights)) {
                type_weights <- t(as.matrix(type_weights))
                row.names(type_weights) <- row.names(type_abundance)
              }

              return(new("metacommunity",
                         type_abundance = type_abundance,
                         ordinariness = Zp.j,
                         subcommunity_weights = subcommunity_weights,
                         type_weights = type_weights,
                         dat_id = similarity@dat_id,
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
            cat("Object of class `metacommunity`, containing all of the data required to calculate diversity.")
          } )
