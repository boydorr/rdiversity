#' taxvec
#'
#' Calculate the taxonomic similarity of a single species to all other species.
#' Used by \code{metacommunity()} to generate a similarity matrix line-by-line
#' when one was not precalculated by \code{tax2dist()}.
#'
#' @param similarity An object of class \code{similarity} (not containing a
#' similarity matrix).
#' @param row \code{integer} denoting which row of the similarity matrix is to
#' be calculated.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#'
#' # Assign values for each level (Shimatani's taxonomic distance)
#' tax_distance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)
#'
#' dist <- tax2dist(lookup, tax_distance, precompute_dist = FALSE)
#' similarity <- dist2sim(dist, "linear")
#' taxvec(similarity, 1)
#' }
#'
taxvec <- function(similarity, row) {
  components <- similarity@components

  total <- sum(components$tax_bits)
  species_factors <- lapply(components$tax_id, function(x)
    as.binary(x, n = total))

  difference <- lapply(species_factors, function(x) {
    tmp <- xor(species_factors[[row]], x)
    tmp <- 1 - as.numeric(as.character(tmp))
    as.binary(tmp, logic = TRUE)
  })

  split_values <- components$tax_similarity
  split_values <- vapply(seq_along(split_values), function(x)
    split_values[x] - split_values[x + 1], numeric(1))
  split_values <- split_values[-length(split_values)]

  masks <- components$tax_mask
  one <- lapply(difference, function(x) {
    tmp <- lapply(seq_along(masks), function(y)
      ( (x & masks[[y]]) == masks[[y]]) * split_values[y])
    sum(unlist(tmp))
  })
  unlist(one)
}
