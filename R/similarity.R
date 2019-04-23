#' Generate similarity object
#'
#' Container for class \code{similarity}.
#'
#' @param similarity similarity matrix
#' @param datID object of class \code{character} denoting the type of diversity
#' being calculated. This can be "naive", "genetic", "taxonomic", and so on
#'
#' @return \code{similarity()} returns an object of class \code{similarity}.
#' @name similarity
#' @rdname similarity-methods
#' @exportMethod similarity
#'
setGeneric(name = "similarity",
           def = function(similarity, datID) {
             standardGeneric("similarity")
           } )


#' @rdname similarity-methods
#' @aliases similarity
#'
setMethod(f = "similarity",
          signature(similarity = "matrix", datID = "character"),
          definition = function(similarity, datID) {
            similarity <- check_similarity(similarity)

            new('similarity',
                similarity = similarity,
                datID = datID,
                parameters = list(transform = NA,
                                  k = NA,
                                  normalise = NA,
                                  max_d = NA))
          } )


#' @rdname similarity-methods
#' @aliases similarity
#'
setMethod(f = "similarity",
          signature(similarity = "matrix", datID = "missing"),
          definition = function(similarity, datID) {
            similarity <- check_similarity(similarity)

            new('similarity',
                similarity = similarity,
                datID = "UserGenerated",
                parameters = list(transform = NA,
                                  k = NA,
                                  normalise = NA,
                                  max_d = NA))
          } )


#' @rdname similarity-class
#' @param object object of class \code{similarity}
#'
setMethod(f = "show", signature = "similarity",
          definition = function(object) {
            cat('Object of class `similarity`, containing either:\n (1) a similarity matrix; or\n (2) all of the data required to calculate a similarity matrix.')
          } )
