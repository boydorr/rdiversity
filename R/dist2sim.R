#' Distance to similarity
#'
#' Converts \code{distance} objects into \code{similarity} objects.
#'
#' Distances can be transformed either *linearly* or *exponentially*. That is
#' \code{1 - k * dist} for non-negative values, or \code{exp(-k * dist)},
#' respectively. If \code{normalise} is true, then \code{dist = dist/max_d}.
#'
#' @param dist object of class \code{distance}
#' @param transform object of class \code{character}, can be either "linear"
#' or "exponential"
#' @param k scaling parameter
#' @param normalise object of class \code{logical}, which when TRUE will
#' normalise distances to one
#' @param max_d object of class \code{numeric}
#'
#' @return \code{dist2sim(x)} returns an object of class \code{similarity}.
#' @include similarity.R class-similarity.R
#' @export
#'
dist2sim <- function(dist,
                     transform,
                     k = 1,
                     normalise = TRUE,
                     max_d) {

  if (class(dist) != "distance")
    stop("The argument `dist` must be of class `distance`.")

  # If a distance matrix is available, convert it into a similarity matrix
  if (length(dist@distance) != 0) {

    similarity <- dist@distance
    if (missing(max_d)) max_d <- max(dist@distance)
    if (normalise) similarity <- similarity / max_d
    if (transform == substr("linear", 1, nchar(transform))) {
      similarity <- pmax(1 - k * similarity, 0)
      transform <- "linear"
    }
    if (transform == substr("exponential", 1, nchar(transform))) {
      similarity <- exp(-k * similarity)
      transform <- "exponential"
    }

    return(new("similarity",
               similarity = similarity,
               dat_id = dist@dat_id,
               components = dist@components,
               parameters = list(transform = transform,
                                 k = k,
                                 normalise = normalise,
                                 max_d = max_d)))

  } else {

    # If there is no distance matrix...

    if (dist@dat_id == "taxonomic") {

      components <- dist@components

      tax_similarity <- components$tax_distance
      if (missing(max_d)) max_d <- max(tax_similarity)
      if (normalise) tax_similarity <- tax_similarity / max_d
      if (transform == substr("linear", 1, nchar(transform)))
        tax_similarity <- pmax(1 - k * tax_similarity, 0)
      if (transform == substr("exponential", 1, nchar(transform)))
        tax_similarity <- exp(-k * tax_similarity)

      new_components <- append(components,
                               list(tax_similarity = tax_similarity), 3)

      return(new("similarity",
                 dat_id = dist@dat_id,
                 components = new_components,
                 parameters = list(transform = transform,
                                   k = k,
                                   normalise = normalise,
                                   max_d = max_d)))
    }
  }
}
